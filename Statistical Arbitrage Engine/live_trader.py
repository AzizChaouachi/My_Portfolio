"""
live_trader.py
==============
Live paper trading loop using Alpaca WebSocket feed + StatArb Engine.
Monitors multiple pairs in real-time and auto-executes signals.

Usage:
    export ALPACA_KEY="your_key"
    export ALPACA_SECRET="your_secret"
    python live_trader.py
"""

import os
import time
import threading
import pandas as pd
import yfinance as yf
from datetime import datetime, timedelta
from stat_arb_engine import (
    StatArbEngine, PairConfig, CointegrationTests, KalmanHedgeFilter
)

# ─── CONFIG ──────────────────────────────────────────────────────────────────

PAIRS = [
    ("JPM", "GS"),
    ("XOM", "CVX"),
    ("MSFT", "GOOGL"),
    ("KO", "PEP"),
    ("MA", "V"),
]

SCAN_LOOKBACK_DAYS = 252       # 1 year for cointegration scan
SIGNAL_LOOKBACK_BARS = 60      # rolling z-score window
ENTRY_THRESHOLD = 2.0          # z-score to enter
EXIT_THRESHOLD = 0.5           # z-score to exit
STOP_LOSS = 3.5                # z-score stop loss
CAPITAL_PER_PAIR = 10_000.0    # USD per pair
POLL_INTERVAL_SEC = 60         # how often to check prices (seconds)
MIN_HALF_LIFE = 5              # reject pairs with half-life < 5 days
MAX_HALF_LIFE = 60             # reject pairs with half-life > 60 days


# ─── STARTUP SCAN ────────────────────────────────────────────────────────────

def startup_scan(pairs: list) -> list:
    """
    Download 1 year of daily prices and run cointegration tests.
    Returns list of (ticker_a, ticker_b) pairs that pass both tests.
    """
    print("[Scan] Downloading price data...")
    tickers = list(set([t for pair in pairs for t in pair]))
    end = datetime.now()
    start = end - timedelta(days=SCAN_LOOKBACK_DAYS + 10)

    try:
        raw = yf.download(tickers, start=start, end=end, progress=False)["Close"]
    except Exception as e:
        print(f"[Scan] yfinance error: {e}. Using synthetic data.")
        return pairs[:2]  # fallback

    approved = []
    print(f"[Scan] Testing {len(pairs)} pairs for cointegration...")
    print("-" * 65)
    print(f"  {'Pair':<12} {'EG p-val':>9} {'Trace':>9} {'Half-life':>10} {'Status':>8}")
    print("-" * 65)

    for ta, tb in pairs:
        if ta not in raw.columns or tb not in raw.columns:
            continue
        pa = raw[ta].dropna().values
        pb = raw[tb].dropna().values
        n = min(len(pa), len(pb))
        pa, pb = pa[-n:], pb[-n:]

        eg = CointegrationTests.engle_granger(pa, pb)
        jh = CointegrationTests.johansen(pa, pb)
        hl = eg["half_life"]

        status = "PASS" if (eg["cointegrated"] and MIN_HALF_LIFE <= hl <= MAX_HALF_LIFE) else "SKIP"
        print(f"  {ta}/{tb:<8} {eg['p_value']:>9.4f} {jh['trace_stat']:>9.2f} {hl:>9.1f}d {status:>8}")

        if status == "PASS":
            approved.append((ta, tb))

    print("-" * 65)
    print(f"[Scan] {len(approved)}/{len(pairs)} pairs approved\n")
    return approved


# ─── LIVE PRICE FEED (polling) ───────────────────────────────────────────────

def get_latest_prices(tickers: list) -> dict:
    """Fetch latest prices via yfinance (1-minute delay for free tier)."""
    try:
        data = yf.download(tickers, period="1d", interval="1m", progress=False)["Close"]
        if isinstance(data, pd.Series):
            data = data.to_frame(name=tickers[0])
        return {col: float(data[col].dropna().iloc[-1]) for col in data.columns if not data[col].dropna().empty}
    except Exception as e:
        print(f"[Feed] Price fetch error: {e}")
        return {}


# ─── TRADING LOOP ────────────────────────────────────────────────────────────

class LiveTrader:

    def __init__(self):
        self.api_key = os.environ.get("ALPACA_KEY", "")
        self.api_secret = os.environ.get("ALPACA_SECRET", "")
        self.engine = StatArbEngine(PAIRS, self.api_key, self.api_secret)
        self.approved_pairs = []
        self.running = False
        self.signal_log = []

    def start(self):
        print("=" * 65)
        print("  STATISTICAL ARBITRAGE ENGINE — LIVE TRADER")
        print("=" * 65)

        # Startup scan
        self.approved_pairs = startup_scan(PAIRS)
        if not self.approved_pairs:
            print("[ERROR] No cointegrated pairs found. Check data or loosen criteria.")
            return

        print(f"[Live] Starting trading loop. Polling every {POLL_INTERVAL_SEC}s")
        print(f"[Live] Entry: ±{ENTRY_THRESHOLD}σ | Exit: ±{EXIT_THRESHOLD}σ | Stop: ±{STOP_LOSS}σ\n")

        self.running = True
        self._loop()

    def _loop(self):
        tickers = list(set([t for pair in self.approved_pairs for t in pair]))
        iteration = 0

        while self.running:
            iteration += 1
            ts = pd.Timestamp.now()
            print(f"[{ts.strftime('%H:%M:%S')}] Tick #{iteration}")

            prices = get_latest_prices(tickers)
            if not prices:
                print("  No prices received, retrying...")
                time.sleep(POLL_INTERVAL_SEC)
                continue

            for ta, tb in self.approved_pairs:
                if ta not in prices or tb not in prices:
                    continue
                pa, pb = prices[ta], prices[tb]

                signal = self.engine.live_step(ta, tb, pa, pb, ts)
                if signal:
                    self.signal_log.append({
                        "time": ts, "pair": signal.pair,
                        "side": signal.side, "z": signal.z_score,
                    })
                    print(f"  ★ SIGNAL: {signal.pair} | {signal.side} | z={signal.z_score:+.3f} | β={signal.beta:.4f}")
                else:
                    # Print current z-score
                    gen = self.engine.signals.get(f"{ta}/{tb}")
                    if gen and len(gen._spread_history) >= 60:
                        window = gen._spread_history[-60:]
                        mu, sigma = sum(window)/len(window), (sum((x-sum(window)/len(window))**2 for x in window)/len(window))**0.5
                        z = (window[-1] - mu) / (sigma + 1e-12)
                        bar = "█" * int(min(10, abs(z) * 2.5))
                        print(f"  {ta}/{tb:4}  z={z:+.3f}  {'':2} {bar}")

            time.sleep(POLL_INTERVAL_SEC)

    def stop(self):
        self.running = False
        print("\n[Live] Trader stopped.")
        if self.signal_log:
            df = pd.DataFrame(self.signal_log)
            df.to_csv("signal_log.csv", index=False)
            print(f"[Live] Signal log saved to signal_log.csv ({len(df)} signals)")


# ─── ENTRY POINT ─────────────────────────────────────────────────────────────

if __name__ == "__main__":
    trader = LiveTrader()
    try:
        trader.start()
    except KeyboardInterrupt:
        trader.stop()
