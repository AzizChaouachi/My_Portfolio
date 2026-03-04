"""
backtest_analysis.py
====================
Comprehensive backtest runner with performance analytics,
walk-forward validation, and sensitivity analysis.
"""

import numpy as np
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

from stat_arb_engine import (
    CointegrationTests, KalmanHedgeFilter, SpreadCalculator,
    Backtester, PairConfig, generate_cointegrated_prices
)


# ─── WALK-FORWARD VALIDATION ─────────────────────────────────────────────────

def walk_forward_test(prices_a: pd.Series, prices_b: pd.Series,
                      train_size: int = 126, test_size: int = 21,
                      config: PairConfig = None) -> pd.DataFrame:
    """
    Walk-forward validation: train on 6 months, test on 1 month.
    Avoids overfitting by ensuring out-of-sample evaluation.
    """
    if config is None:
        config = PairConfig("A", "B")

    results = []
    n = len(prices_a)
    start = train_size

    while start + test_size <= n:
        train_a = prices_a.iloc[start - train_size:start]
        train_b = prices_b.iloc[start - train_size:start]
        test_a = prices_a.iloc[start:start + test_size]
        test_b = prices_b.iloc[start:start + test_size]

        # Fit on train
        eg = CointegrationTests.engle_granger(train_a.values, train_b.values)

        # Evaluate on test
        test_data = pd.DataFrame({"A": test_a, "B": test_b})
        bt = Backtester(config)
        trades_df = bt.run(test_a, test_b)
        stats = bt.stats()

        results.append({
            "period_start": test_a.index[0],
            "period_end": test_a.index[-1],
            "train_beta": eg["beta"],
            "train_p_value": eg["p_value"],
            "train_half_life": eg["half_life"],
            "oos_trades": stats.get("total_trades", 0),
            "oos_pnl": stats.get("total_pnl", 0),
            "oos_win_rate": stats.get("win_rate", 0),
            "oos_sharpe": stats.get("sharpe_ratio", 0),
        })
        start += test_size

    return pd.DataFrame(results)


# ─── SENSITIVITY ANALYSIS ────────────────────────────────────────────────────

def sensitivity_analysis(prices_a: pd.Series, prices_b: pd.Series) -> pd.DataFrame:
    """
    Test backtest performance across a grid of threshold parameters.
    Helps identify robust parameter regions vs. overfit ones.
    """
    results = []
    thresholds = [1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0]
    lookbacks = [30, 45, 60, 90]

    for thresh in thresholds:
        for lb in lookbacks:
            config = PairConfig("A", "B", entry_threshold=thresh, lookback=lb)
            bt = Backtester(config)
            bt.run(prices_a, prices_b)
            stats = bt.stats()
            results.append({
                "entry_threshold": thresh,
                "lookback": lb,
                "total_trades": stats.get("total_trades", 0),
                "total_pnl": stats.get("total_pnl", 0),
                "win_rate": stats.get("win_rate", 0),
                "sharpe": stats.get("sharpe_ratio", 0),
                "max_drawdown": stats.get("max_drawdown", 0),
                "profit_factor": min(stats.get("profit_factor", 0), 10),
            })

    return pd.DataFrame(results)


# ─── KALMAN vs OLS COMPARISON ────────────────────────────────────────────────

def compare_kalman_vs_ols(prices_a: pd.Series, prices_b: pd.Series) -> dict:
    """Compare static OLS hedge ratio vs Kalman dynamic hedge ratio."""

    # OLS
    eg = CointegrationTests.engle_granger(prices_a.values, prices_b.values)
    ols_spread = SpreadCalculator.compute(prices_a.values, prices_b.values,
                                          eg["beta"], eg["alpha"])
    ols_z = SpreadCalculator.rolling_zscore(ols_spread, window=60)

    # Kalman
    kf = KalmanHedgeFilter(delta=1e-4)
    kf_results = kf.batch_filter(prices_a.values, prices_b.values)
    kalman_spread = np.array([
        prices_b.values[i] - kf_results["beta"].iloc[i] * prices_a.values[i] - kf_results["alpha"].iloc[i]
        for i in range(len(prices_a))
    ])
    kalman_z = SpreadCalculator.rolling_zscore(kalman_spread, window=60)

    # Stationarity comparison
    ols_adf_t, ols_adf_p = CointegrationTests._adf_test(ols_spread)
    kal_adf_t, kal_adf_p = CointegrationTests._adf_test(kalman_spread)

    return {
        "ols": {
            "beta": eg["beta"],
            "spread_std": np.std(ols_spread),
            "spread_mean": np.mean(ols_spread),
            "adf_t_stat": ols_adf_t,
            "adf_p_value": ols_adf_p,
            "z_score_std": np.nanstd(ols_z),
        },
        "kalman": {
            "beta_mean": kf_results["beta"].mean(),
            "beta_std": kf_results["beta"].std(),
            "spread_std": np.std(kalman_spread),
            "spread_mean": np.mean(kalman_spread),
            "adf_t_stat": kal_adf_t,
            "adf_p_value": kal_adf_p,
            "z_score_std": np.nanstd(kalman_z),
        },
    }


# ─── MAIN ────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    print("=" * 60)
    print("  BACKTEST ANALYSIS SUITE")
    print("=" * 60)

    # Generate synthetic cointegrated pair
    pa, pb = generate_cointegrated_prices(n=504, beta=1.5, seed=99)

    # ── 1. Walk-forward
    print("\n[1] WALK-FORWARD VALIDATION (6mo train / 1mo test)")
    print("-" * 60)
    wf = walk_forward_test(pa, pb, train_size=126, test_size=21)
    print(wf[["period_start", "oos_trades", "oos_pnl", "oos_win_rate", "oos_sharpe"]].to_string(index=False))
    print(f"\n  Avg OOS Sharpe:  {wf['oos_sharpe'].mean():.3f}")
    print(f"  Avg OOS Win Rate:{wf['oos_win_rate'].mean():.1%}")
    print(f"  Total OOS Trades:{wf['oos_trades'].sum()}")

    # ── 2. Sensitivity
    print("\n[2] SENSITIVITY ANALYSIS (threshold × lookback grid)")
    print("-" * 60)
    sa = sensitivity_analysis(pa, pb)
    pivot = sa.pivot_table(values="sharpe", index="entry_threshold", columns="lookback")
    print("  Sharpe Ratio Grid (entry threshold vs lookback):")
    print(pivot.round(3).to_string())

    # ── 3. Kalman vs OLS
    print("\n[3] KALMAN FILTER vs STATIC OLS")
    print("-" * 60)
    comp = compare_kalman_vs_ols(pa, pb)
    for method, m in comp.items():
        print(f"\n  [{method.upper()}]")
        for k, v in m.items():
            print(f"    {k:<20} {v:.4f}")

    # ── 4. Full backtest
    print("\n[4] FULL BACKTEST — OPTIMAL PARAMS")
    print("-" * 60)
    best_row = sa.loc[sa["sharpe"].idxmax()]
    best_thresh = best_row["entry_threshold"]
    best_lb = int(best_row["lookback"])
    print(f"  Best params: threshold={best_thresh}σ, lookback={best_lb}")
    config = PairConfig("A", "B", entry_threshold=best_thresh, lookback=best_lb)
    bt = Backtester(config)
    trades = bt.run(pa, pb)
    stats = bt.stats()
    print(f"\n  Performance:")
    for k, v in stats.items():
        if v is not None and isinstance(v, (int, float)):
            print(f"    {k:<25} {v:.4f}")

    if not trades.empty:
        print(f"\n  Sample trades (last 5):")
        print(trades[["side", "entry_z", "exit_z", "pnl"]].tail(5).to_string(index=False))
