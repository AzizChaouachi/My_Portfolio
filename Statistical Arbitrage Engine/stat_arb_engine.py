"""
================================================================================
  STATISTICAL ARBITRAGE ENGINE
  Pairs Trading with Engle-Granger Cointegration, Johansen Test,
  Kalman Filter Dynamic Hedge Ratios, and Alpaca Paper Trading
================================================================================
  Author  : StatArb Engine v2.1
  License : MIT
  Python  : 3.10+

  Dependencies:
    pip install numpy pandas scipy statsmodels alpaca-trade-api yfinance pykalman
================================================================================
"""

import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.tsa.stattools import adfuller, coint
from statsmodels.tsa.vector_ar.vecm import coint_johansen
from statsmodels.regression.linear_model import OLS
from statsmodels.tools import add_constant
import warnings
import logging
from dataclasses import dataclass, field
from typing import Optional
from datetime import datetime, timedelta

# ── Optional live trading imports ──────────────────────────────────────────────
try:
    import alpaca_trade_api as tradeapi
    ALPACA_AVAILABLE = True
except ImportError:
    ALPACA_AVAILABLE = False
    print("[WARN] alpaca-trade-api not installed. Live trading disabled.")

try:
    import yfinance as yf
    YFINANCE_AVAILABLE = True
except ImportError:
    YFINANCE_AVAILABLE = False
    print("[WARN] yfinance not installed. Using synthetic data.")

warnings.filterwarnings("ignore")
logging.basicConfig(level=logging.INFO,
                    format="%(asctime)s [%(levelname)s] %(message)s")
log = logging.getLogger("StatArb")


# ══════════════════════════════════════════════════════════════════════════════
#  DATA STRUCTURES
# ══════════════════════════════════════════════════════════════════════════════

@dataclass
class PairConfig:
    ticker_a: str
    ticker_b: str
    sector: str
    entry_zscore: float = 2.0
    exit_zscore: float = 0.5
    stop_loss_zscore: float = 3.5
    capital: float = 10_000.0


@dataclass
class Signal:
    timestamp: datetime
    ticker_a: str
    ticker_b: str
    action: str          # "LONG_SPREAD" | "SHORT_SPREAD" | "CLOSE" | "STOP"
    zscore: float
    hedge_ratio: float
    spread_value: float
    confidence: float


@dataclass
class Position:
    pair: str
    direction: str       # "LONG" | "SHORT"
    entry_time: datetime
    entry_zscore: float
    hedge_ratio: float
    qty_a: int
    qty_b: int
    price_a: float
    price_b: float
    is_open: bool = True
    pnl: float = 0.0


@dataclass
class BacktestResult:
    trades: list = field(default_factory=list)
    equity_curve: list = field(default_factory=list)
    total_pnl: float = 0.0
    win_rate: float = 0.0
    sharpe: float = 0.0
    max_drawdown: float = 0.0
    num_trades: int = 0
    half_life: float = 0.0


# ══════════════════════════════════════════════════════════════════════════════
#  DATA LAYER
# ══════════════════════════════════════════════════════════════════════════════

class DataFetcher:
    """Fetches OHLCV price data. Falls back to synthetic if yfinance unavailable."""

    @staticmethod
    def fetch(ticker: str, start: str, end: str, interval: str = "1d") -> pd.Series:
        if YFINANCE_AVAILABLE:
            try:
                df = yf.download(ticker, start=start, end=end,
                                 interval=interval, progress=False)
                if df.empty:
                    raise ValueError(f"No data for {ticker}")
                return df["Adj Close"].dropna()
            except Exception as e:
                log.warning(f"yfinance failed for {ticker}: {e}. Using synthetic.")

        return DataFetcher._synthetic(ticker, start, end)

    @staticmethod
    def _synthetic(ticker: str, start: str, end: str) -> pd.Series:
        """Generate realistic synthetic price series using GBM."""
        rng = sum(ord(c) for c in ticker)
        np.random.seed(rng % (2**31))
        dates = pd.date_range(start, end, freq="B")
        n = len(dates)
        base = 100 + (rng % 400)
        mu, sigma = 0.0002, 0.015
        log_returns = np.random.normal(mu, sigma, n)
        prices = base * np.exp(np.cumsum(log_returns))
        return pd.Series(prices, index=dates, name=ticker)

    @staticmethod
    def fetch_pair(cfg: PairConfig, start: str, end: str) -> tuple[pd.Series, pd.Series]:
        a = DataFetcher.fetch(cfg.ticker_a, start, end)
        b = DataFetcher.fetch(cfg.ticker_b, start, end)
        # Align on common trading days
        common = a.index.intersection(b.index)
        return a.loc[common], b.loc[common]


# ══════════════════════════════════════════════════════════════════════════════
#  COINTEGRATION ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

class CointegrationAnalyzer:
    """
    Tests for cointegration using two methods:
      1. Engle-Granger two-step procedure
      2. Johansen VECM trace/eigenvalue test
    """

    @staticmethod
    def engle_granger(series_a: pd.Series, series_b: pd.Series) -> dict:
        """
        Engle-Granger test:
          1. Run OLS: B = α + β·A + ε
          2. Apply ADF to residuals ε
          3. If ADF rejects unit root → series are cointegrated
        """
        # Step 1: OLS regression
        x = add_constant(series_a.values)
        model = OLS(series_b.values, x).fit()
        alpha, beta = model.params[0], model.params[1]
        residuals = model.resid

        # Step 2: ADF on residuals
        adf_result = adfuller(residuals, autolag="AIC")
        t_stat = adf_result[0]
        p_value = adf_result[1]
        crit_values = adf_result[4]

        # Step 3: statsmodels coint test (convenience)
        coint_t, coint_p, _ = coint(series_a.values, series_b.values)

        return {
            "method": "Engle-Granger",
            "ols_alpha": alpha,
            "ols_beta": beta,
            "residuals": residuals,
            "adf_tstat": t_stat,
            "adf_pvalue": p_value,
            "coint_pvalue": coint_p,
            "critical_1pct": crit_values["1%"],
            "critical_5pct": crit_values["5%"],
            "critical_10pct": crit_values["10%"],
            "cointegrated": p_value < 0.05,
            "confidence": "99%" if p_value < 0.01 else "95%" if p_value < 0.05 else "90%" if p_value < 0.10 else "N/A",
        }

    @staticmethod
    def johansen(series_a: pd.Series, series_b: pd.Series, det_order: int = 0, k_ar_diff: int = 1) -> dict:
        """
        Johansen test for cointegrating rank.
          det_order: -1=no deterministic, 0=constant, 1=linear trend
          Returns cointegrating vectors and whether rank >= 1
        """
        data = np.column_stack([series_a.values, series_b.values])
        result = coint_johansen(data, det_order, k_ar_diff)

        # Trace statistic test (H0: rank <= r)
        trace_stat = result.lr1
        trace_crit = result.cvt  # [90%, 95%, 99%]

        # Max eigenvalue test
        max_eig_stat = result.lr2
        max_eig_crit = result.cvm

        # Cointegrating vector (first eigenvector)
        coint_vector = result.evec[:, 0]
        beta_johansen = -coint_vector[1] / coint_vector[0]

        rank = sum(trace_stat[i] > trace_crit[i, 1] for i in range(len(trace_stat)))

        return {
            "method": "Johansen",
            "rank": rank,
            "cointegrated": rank >= 1,
            "beta_johansen": beta_johansen,
            "trace_stats": trace_stat.tolist(),
            "trace_crit_95": trace_crit[:, 1].tolist(),
            "max_eig_stats": max_eig_stat.tolist(),
            "coint_vector": coint_vector.tolist(),
            "eigenvalues": result.eig.tolist(),
        }

    @staticmethod
    def half_life(residuals: np.ndarray) -> float:
        """
        Estimate mean-reversion half-life via AR(1) regression on spread:
          ΔS_t = λ·S_{t-1} + ε  →  half_life = -ln(2) / ln(1 + λ)
        """
        spread_lag = residuals[:-1]
        spread_diff = np.diff(residuals)
        x = add_constant(spread_lag)
        model = OLS(spread_diff, x).fit()
        lam = model.params[1]
        if lam >= 0:
            return np.inf  # Not mean-reverting
        return -np.log(2) / np.log(1 + lam)

    @staticmethod
    def hurst_exponent(series: np.ndarray, max_lag: int = 20) -> float:
        """
        Hurst exponent: H < 0.5 → mean-reverting, H = 0.5 → random walk, H > 0.5 → trending
        """
        lags = range(2, max_lag)
        tau = [np.std(np.subtract(series[lag:], series[:-lag])) for lag in lags]
        reg = np.polyfit(np.log(lags), np.log(tau), 1)
        return reg[0]


# ══════════════════════════════════════════════════════════════════════════════
#  KALMAN FILTER
# ══════════════════════════════════════════════════════════════════════════════

class KalmanHedgeRatio:
    """
    Kalman Filter for dynamic hedge ratio estimation.

    State:    θ_t = [β_t, α_t]^T   (hedge ratio + intercept)
    Obs:      y_t = H_t · θ_t + ε_t   where H_t = [x_t, 1]
    Dynamics: θ_t = θ_{t-1} + w_t

    This treats β as a time-varying parameter that evolves as a random walk,
    adapting to regime changes while smoothing out noise.
    """

    def __init__(self,
                 delta: float = 1e-5,     # State transition variance (controls β drift speed)
                 vt: float = 1e-3):       # Observation noise variance
        self.delta = delta
        self.vt = vt
        # State: [beta, alpha]
        self._theta = np.zeros(2)
        self._P = np.eye(2)               # State covariance matrix
        self._Wt = delta / (1 - delta) * np.eye(2)  # Process noise covariance

    def update(self, x: float, y: float) -> tuple[float, float, float]:
        """
        Single Kalman step. Returns (beta, alpha, kalman_gain_norm).
        """
        H = np.array([x, 1.0])

        # ── Prediction step ──────────────────────────────────────────────────
        # θ_{t|t-1} = θ_{t-1}  (random walk state model)
        # P_{t|t-1} = P_{t-1} + W_t
        P_pred = self._P + self._Wt

        # ── Innovation (prediction error) ────────────────────────────────────
        y_hat = H @ self._theta
        innovation = y - y_hat

        # ── Innovation covariance ─────────────────────────────────────────────
        S = H @ P_pred @ H.T + self.vt

        # ── Kalman gain ───────────────────────────────────────────────────────
        K = P_pred @ H.T / S

        # ── Update step ───────────────────────────────────────────────────────
        self._theta = self._theta + K * innovation
        self._P = (np.eye(2) - np.outer(K, H)) @ P_pred

        return self._theta[0], self._theta[1], float(np.linalg.norm(K))

    def fit(self, series_a: np.ndarray, series_b: np.ndarray) -> dict:
        """Fit Kalman filter to full price history. Returns arrays of betas/alphas."""
        n = len(series_a)
        betas = np.zeros(n)
        alphas = np.zeros(n)
        gains = np.zeros(n)

        for i in range(n):
            b, a, g = self.update(series_a[i], series_b[i])
            betas[i], alphas[i], gains[i] = b, a, g

        return {
            "betas": betas,
            "alphas": alphas,
            "gains": gains,
            "final_beta": betas[-1],
            "final_alpha": alphas[-1],
        }


# ══════════════════════════════════════════════════════════════════════════════
#  SPREAD & Z-SCORE
# ══════════════════════════════════════════════════════════════════════════════

class SpreadCalculator:
    """Computes spread, z-score, and rolling statistics."""

    @staticmethod
    def compute(series_a: np.ndarray, series_b: np.ndarray,
                beta: float, alpha: float) -> np.ndarray:
        """Spread: S_t = B_t - β·A_t - α"""
        return series_b - beta * series_a - alpha

    @staticmethod
    def zscore(spread: np.ndarray,
               lookback: Optional[int] = None) -> np.ndarray:
        """
        Standardize spread. If lookback specified, uses rolling window.
        """
        if lookback:
            s = pd.Series(spread)
            mean = s.rolling(lookback).mean()
            std = s.rolling(lookback).std()
            return ((s - mean) / std).fillna(0).values
        else:
            mean, std = np.mean(spread), np.std(spread)
            return (spread - mean) / (std if std > 0 else 1)

    @staticmethod
    def dynamic_zscore(series_a: np.ndarray, series_b: np.ndarray,
                       kf: KalmanHedgeRatio) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Z-score using Kalman dynamic β."""
        result = kf.fit(series_a, series_b)
        betas, alphas = result["betas"], result["alphas"]
        spread = series_b - betas * series_a - alphas
        zs = SpreadCalculator.zscore(spread)
        return zs, betas, alphas


# ══════════════════════════════════════════════════════════════════════════════
#  SIGNAL GENERATOR
# ══════════════════════════════════════════════════════════════════════════════

class SignalGenerator:
    """
    Generates trading signals based on z-score thresholds.

    Rules:
      z < -entry  →  LONG SPREAD  (buy A, sell B)
      z > +entry  →  SHORT SPREAD (sell A, buy B)
      |z| < exit  →  CLOSE position (mean reversion)
      |z| > stop  →  STOP LOSS
    """

    def __init__(self, cfg: PairConfig):
        self.cfg = cfg

    def generate(self, zscores: np.ndarray, betas: np.ndarray,
                 spreads: np.ndarray, timestamps: pd.DatetimeIndex) -> list[Signal]:
        signals = []
        position = 0  # 0=flat, 1=long, -1=short

        for i, (z, beta, spread) in enumerate(zip(zscores, betas, spreads)):
            ts = timestamps[i]
            sig = None

            if position == 0:
                if z < -self.cfg.entry_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "LONG_SPREAD", z, beta, spread, abs(z) / self.cfg.entry_zscore)
                    position = 1
                elif z > self.cfg.entry_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "SHORT_SPREAD", z, beta, spread, abs(z) / self.cfg.entry_zscore)
                    position = -1

            elif position == 1:  # Long spread
                if abs(z) < self.cfg.exit_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "CLOSE", z, beta, spread, 1.0)
                    position = 0
                elif z > self.cfg.stop_loss_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "STOP", z, beta, spread, 0.0)
                    position = 0

            elif position == -1:  # Short spread
                if abs(z) < self.cfg.exit_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "CLOSE", z, beta, spread, 1.0)
                    position = 0
                elif z < -self.cfg.stop_loss_zscore:
                    sig = Signal(ts, self.cfg.ticker_a, self.cfg.ticker_b,
                                 "STOP", z, beta, spread, 0.0)
                    position = 0

            if sig:
                signals.append(sig)

        return signals


# ══════════════════════════════════════════════════════════════════════════════
#  BACKTESTER
# ══════════════════════════════════════════════════════════════════════════════

class Backtester:
    """
    Event-driven backtest engine.
    Computes PnL, Sharpe, max drawdown, win rate.
    """

    def __init__(self, cfg: PairConfig, transaction_cost: float = 0.001):
        self.cfg = cfg
        self.cost = transaction_cost  # 0.1% per leg

    def run(self, series_a: pd.Series, series_b: pd.Series,
            zscores: np.ndarray, betas: np.ndarray) -> BacktestResult:

        trades, equity, open_pos = [], [0.0], None
        pnl_running = 0.0

        for i in range(1, len(series_a)):
            z = zscores[i]
            beta = betas[i]
            pa, pb = series_a.iloc[i], series_b.iloc[i]

            # Position sizing
            qty_b = int(self.cfg.capital / pb)
            qty_a = int(qty_b * beta)

            # Entry logic
            if open_pos is None:
                if z < -self.cfg.entry_zscore:
                    open_pos = {"dir": "LONG", "entry_i": i, "entry_z": z,
                                "pa": pa, "pb": pb, "qa": qty_a, "qb": qty_b}
                elif z > self.cfg.entry_zscore:
                    open_pos = {"dir": "SHORT", "entry_i": i, "entry_z": z,
                                "pa": pa, "pb": pb, "qa": qty_a, "qb": qty_b}

            # Exit logic
            elif open_pos:
                should_exit = (
                    (open_pos["dir"] == "LONG" and (abs(z) < self.cfg.exit_zscore or z > self.cfg.stop_loss_zscore)) or
                    (open_pos["dir"] == "SHORT" and (abs(z) < self.cfg.exit_zscore or z < -self.cfg.stop_loss_zscore))
                )
                if should_exit:
                    # PnL calculation (dollar-neutral pair)
                    if open_pos["dir"] == "LONG":
                        # Bought A, sold B at entry; reverse at exit
                        pnl = (pa - open_pos["pa"]) * open_pos["qa"] - \
                              (pb - open_pos["pb"]) * open_pos["qb"]
                    else:
                        # Sold A, bought B at entry; reverse at exit
                        pnl = -(pa - open_pos["pa"]) * open_pos["qa"] + \
                               (pb - open_pos["pb"]) * open_pos["qb"]

                    # Transaction costs (2 legs × 2 sides = 4 trades)
                    cost = self.cost * (open_pos["pa"] * open_pos["qa"] +
                                        open_pos["pb"] * open_pos["qb"])
                    pnl -= cost

                    pnl_running += pnl
                    trades.append({
                        "direction": open_pos["dir"],
                        "entry_date": series_a.index[open_pos["entry_i"]],
                        "exit_date": series_a.index[i],
                        "entry_z": open_pos["entry_z"],
                        "exit_z": z,
                        "pnl": pnl,
                        "exit_type": "STOP" if abs(z) > self.cfg.stop_loss_zscore else "EXIT",
                    })
                    open_pos = None

            equity.append(pnl_running)

        # ── Performance metrics ────────────────────────────────────────────────
        eq = np.array(equity)
        returns = np.diff(eq)
        sharpe = (returns.mean() / returns.std() * np.sqrt(252)
                  if returns.std() > 0 else 0.0)

        # Max drawdown
        peak = np.maximum.accumulate(eq)
        drawdown = (eq - peak) / np.where(peak != 0, peak, 1)
        max_dd = drawdown.min()

        wins = [t for t in trades if t["pnl"] > 0]

        # Engle-Granger spread for half-life
        eg = CointegrationAnalyzer.engle_granger(series_a, series_b)
        hl = CointegrationAnalyzer.half_life(eg["residuals"])

        return BacktestResult(
            trades=trades,
            equity_curve=equity,
            total_pnl=pnl_running,
            win_rate=len(wins) / max(1, len(trades)),
            sharpe=sharpe,
            max_drawdown=max_dd,
            num_trades=len(trades),
            half_life=hl,
        )


# ══════════════════════════════════════════════════════════════════════════════
#  ALPACA EXECUTION
# ══════════════════════════════════════════════════════════════════════════════

class AlpacaExecutor:
    """
    Live paper trading via Alpaca REST API.
    Submits dollar-neutral pair orders when signals fire.

    Paper trading URL: https://paper-api.alpaca.markets
    """

    BASE_URL = "https://paper-api.alpaca.markets"

    def __init__(self, api_key: str, secret_key: str):
        if not ALPACA_AVAILABLE:
            raise ImportError("Install alpaca-trade-api: pip install alpaca-trade-api")
        self.api = tradeapi.REST(api_key, secret_key, base_url=self.BASE_URL)
        account = self.api.get_account()
        log.info(f"Connected to Alpaca. Equity: ${float(account.equity):,.2f}, "
                 f"Buying power: ${float(account.buying_power):,.2f}")

    def get_price(self, ticker: str) -> float:
        trade = self.api.get_latest_trade(ticker)
        return float(trade.price)

    def submit_pair(self, signal: Signal, capital: float = 10_000.0):
        """Execute both legs of a pair trade simultaneously."""
        pa = self.get_price(signal.ticker_a)
        pb = self.get_price(signal.ticker_b)

        qty_b = max(1, int(capital / pb))
        qty_a = max(1, int(qty_b * signal.hedge_ratio))

        if signal.action == "LONG_SPREAD":
            # Buy A (undervalued), Sell B (overvalued)
            sides = {signal.ticker_a: ("buy", qty_a),
                     signal.ticker_b: ("sell", qty_b)}
        elif signal.action == "SHORT_SPREAD":
            # Sell A, Buy B
            sides = {signal.ticker_a: ("sell", qty_a),
                     signal.ticker_b: ("buy", qty_b)}
        else:
            log.warning(f"Unhandled signal action: {signal.action}")
            return

        orders = []
        for ticker, (side, qty) in sides.items():
            try:
                order = self.api.submit_order(
                    symbol=ticker,
                    qty=qty,
                    side=side,
                    type="market",
                    time_in_force="day",
                    order_class="simple",
                )
                log.info(f"Order submitted: {side.upper()} {qty} {ticker} | ID={order.id}")
                orders.append(order)
            except Exception as e:
                log.error(f"Order failed for {ticker}: {e}")

        return orders

    def close_pair(self, ticker_a: str, ticker_b: str):
        """Flatten both legs."""
        for ticker in [ticker_a, ticker_b]:
            try:
                self.api.close_position(ticker)
                log.info(f"Closed position: {ticker}")
            except Exception as e:
                log.warning(f"Could not close {ticker}: {e}")

    def get_positions(self) -> dict:
        positions = self.api.list_positions()
        return {p.symbol: {"qty": p.qty, "side": p.side, "unrealized_pl": p.unrealized_pl}
                for p in positions}


# ══════════════════════════════════════════════════════════════════════════════
#  MAIN ENGINE
# ══════════════════════════════════════════════════════════════════════════════

class StatArbEngine:
    """
    Top-level orchestrator. Ties together all components:
      - Data fetching
      - Cointegration testing
      - Kalman hedge ratio
      - Signal generation
      - Backtesting
      - Live execution
    """

    DEFAULT_PAIRS = [
        PairConfig("JPM", "GS", "Banks"),
        PairConfig("XOM", "CVX", "Energy"),
        PairConfig("MSFT", "GOOGL", "Tech"),
        PairConfig("KO", "PEP", "Consumer"),
        PairConfig("MA", "V", "Payments"),
    ]

    def __init__(self,
                 pairs: Optional[list[PairConfig]] = None,
                 lookback_days: int = 252,
                 alpaca_key: Optional[str] = None,
                 alpaca_secret: Optional[str] = None):
        self.pairs = pairs or self.DEFAULT_PAIRS
        self.lookback = lookback_days
        self.executor = None
        if alpaca_key and alpaca_secret:
            try:
                self.executor = AlpacaExecutor(alpaca_key, alpaca_secret)
            except Exception as e:
                log.error(f"Alpaca connection failed: {e}")

    def analyze_pair(self, cfg: PairConfig,
                     start: Optional[str] = None,
                     end: Optional[str] = None) -> dict:
        """Full analysis for a single pair."""
        end = end or datetime.today().strftime("%Y-%m-%d")
        start = start or (datetime.today() - timedelta(days=self.lookback)).strftime("%Y-%m-%d")

        log.info(f"Analyzing {cfg.ticker_a}/{cfg.ticker_b} from {start} to {end}")

        # 1. Fetch data
        a, b = DataFetcher.fetch_pair(cfg, start, end)

        # 2. Cointegration tests
        eg = CointegrationAnalyzer.engle_granger(a, b)
        jo = CointegrationAnalyzer.johansen(a, b)
        hl = CointegrationAnalyzer.half_life(eg["residuals"])
        hurst = CointegrationAnalyzer.hurst_exponent(eg["residuals"])

        # 3. Kalman filter
        kf = KalmanHedgeRatio(delta=1e-5)
        zs_kalman, betas_kalman, alphas_kalman = SpreadCalculator.dynamic_zscore(
            a.values, b.values, kf
        )

        # 4. Static OLS z-score
        spread_ols = SpreadCalculator.compute(a.values, b.values, eg["ols_beta"], eg["ols_alpha"])
        zs_ols = SpreadCalculator.zscore(spread_ols)

        # 5. Signals
        sig_gen = SignalGenerator(cfg)
        signals = sig_gen.generate(zs_kalman, betas_kalman,
                                    spread_ols, a.index)

        # 6. Backtest
        bt = Backtester(cfg)
        result = bt.run(a, b, zs_kalman, betas_kalman)

        return {
            "config": cfg,
            "series_a": a,
            "series_b": b,
            "engle_granger": eg,
            "johansen": jo,
            "half_life_days": hl,
            "hurst_exponent": hurst,
            "kalman_betas": betas_kalman,
            "kalman_alphas": alphas_kalman,
            "zscores_ols": zs_ols,
            "zscores_kalman": zs_kalman,
            "signals": signals,
            "backtest": result,
            "current_zscore": zs_kalman[-1],
            "current_beta": betas_kalman[-1],
            "current_signal": signals[-1].action if signals and signals[-1].timestamp == a.index[-1] else "FLAT",
        }

    def run_all(self, start: Optional[str] = None, end: Optional[str] = None) -> list[dict]:
        """Analyze all configured pairs."""
        results = []
        for cfg in self.pairs:
            try:
                r = self.analyze_pair(cfg, start, end)
                results.append(r)
                bt = r["backtest"]
                log.info(f"  {cfg.ticker_a}/{cfg.ticker_b}: "
                         f"coint={'YES' if r['engle_granger']['cointegrated'] else 'NO'}, "
                         f"z={r['current_zscore']:.2f}, "
                         f"trades={bt.num_trades}, win={bt.win_rate:.1%}, "
                         f"sharpe={bt.sharpe:.2f}")
            except Exception as e:
                log.error(f"Failed to analyze {cfg.ticker_a}/{cfg.ticker_b}: {e}")
        return results

    def execute_signals(self, results: list[dict]):
        """Execute live signals for all pairs with actionable signals."""
        if not self.executor:
            log.warning("No Alpaca executor configured.")
            return
        for r in results:
            sig = r["signals"][-1] if r["signals"] else None
            if sig and sig.action in ("LONG_SPREAD", "SHORT_SPREAD"):
                log.info(f"Executing: {sig.action} on {sig.ticker_a}/{sig.ticker_b} | z={sig.zscore:.2f}")
                self.executor.submit_pair(sig, r["config"].capital)

    def print_report(self, results: list[dict]):
        """Print a formatted summary report."""
        print("\n" + "=" * 72)
        print("  STATISTICAL ARBITRAGE ENGINE — ANALYSIS REPORT")
        print("=" * 72)
        for r in results:
            cfg = r["config"]
            eg = r["engle_granger"]
            bt = r["backtest"]
            print(f"\n  {cfg.ticker_a}/{cfg.ticker_b}  [{cfg.sector}]")
            print(f"  {'─'*40}")
            print(f"  Cointegrated   : {'YES ✓' if eg['cointegrated'] else 'NO ✗'}  "
                  f"(ADF p={eg['adf_pvalue']:.4f}, {eg['confidence']})")
            print(f"  Johansen rank  : {r['johansen']['rank']}  "
                  f"(β={r['johansen']['beta_johansen']:.4f})")
            print(f"  Half-life      : {r['half_life_days']:.1f} days")
            print(f"  Hurst exponent : {r['hurst_exponent']:.4f}  "
                  f"({'mean-reverting' if r['hurst_exponent'] < 0.5 else 'trending'})")
            print(f"  Current z      : {r['current_zscore']:.3f}σ")
            print(f"  Current β (KF) : {r['current_beta']:.4f}")
            print(f"  Signal         : {r['current_signal']}")
            print(f"  ─ Backtest ─")
            print(f"  Trades         : {bt.num_trades}")
            print(f"  Win rate       : {bt.win_rate:.1%}")
            print(f"  Sharpe ratio   : {bt.sharpe:.2f}")
            print(f"  Max drawdown   : {bt.max_drawdown:.2%}")
            print(f"  Total P&L      : ${bt.total_pnl:,.2f}")
        print("\n" + "=" * 72)


# ══════════════════════════════════════════════════════════════════════════════
#  ENTRY POINT
# ══════════════════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Statistical Arbitrage Engine")
    parser.add_argument("--lookback", type=int, default=252, help="Days of history")
    parser.add_argument("--threshold", type=float, default=2.0, help="Z-score entry threshold")
    parser.add_argument("--alpaca-key", type=str, default=None, help="Alpaca API key")
    parser.add_argument("--alpaca-secret", type=str, default=None, help="Alpaca secret")
    parser.add_argument("--live", action="store_true", help="Execute live signals")
    args = parser.parse_args()

    # Override default thresholds
    pairs = [PairConfig(a, b, s, entry_zscore=args.threshold)
             for a, b, s in [("JPM","GS","Banks"), ("XOM","CVX","Energy"),
                              ("MSFT","GOOGL","Tech"), ("KO","PEP","Consumer"),
                              ("MA","V","Payments")]]

    engine = StatArbEngine(
        pairs=pairs,
        lookback_days=args.lookback,
        alpaca_key=args.alpaca_key,
        alpaca_secret=args.alpaca_secret,
    )

    results = engine.run_all()
    engine.print_report(results)

    if args.live:
        engine.execute_signals(results)
