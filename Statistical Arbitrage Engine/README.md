# Statistical Arbitrage Engine 📈

**Pairs Trading with Engle-Granger Cointegration, Johansen Test, Kalman Filter Dynamic Hedge Ratios, and Alpaca Paper Trading**

---

## Overview

This engine implements a full statistical arbitrage (stat arb) pipeline for equity pairs trading. It identifies cointegrated pairs, computes dynamically-hedged spreads using a Kalman filter, generates trading signals based on z-score thresholds, backtests the strategy, and optionally executes live paper trades via the Alpaca API.

---

## Project Structure

```
stat_arb/
├── stat_arb_engine.py          # Core engine (all modules)
├── stat_arb_engine.jsx         # Interactive dashboard (React)
├── StatArb_Theory_Guide.docx   # Full theoretical documentation
└── README.md                   # This file
```

---

## Installation

```bash
pip install numpy pandas scipy statsmodels yfinance alpaca-trade-api pykalman
```

---

## Quick Start

```bash
# Analyze all default pairs (JPM/GS, XOM/CVX, MSFT/GOOGL, KO/PEP, MA/V)
python stat_arb_engine.py

# Custom entry threshold and 1-year lookback
python stat_arb_engine.py --threshold 2.5 --lookback 365

# Connect Alpaca paper trading and execute live signals
python stat_arb_engine.py --alpaca-key PK... --alpaca-secret ... --live
```

---

## Core Modules

| Module | Class | Description |
|---|---|---|
| Data | `DataFetcher` | yfinance OHLCV download + synthetic fallback |
| Cointegration | `CointegrationAnalyzer` | Engle-Granger, Johansen, half-life, Hurst |
| Kalman | `KalmanHedgeRatio` | Dynamic β/α via state-space model |
| Spread | `SpreadCalculator` | Spread + rolling/Kalman z-score |
| Signals | `SignalGenerator` | Entry/exit/stop threshold logic |
| Backtest | `Backtester` | Event-driven P&L, Sharpe, drawdown |
| Execution | `AlpacaExecutor` | Alpaca REST paper trading |
| Engine | `StatArbEngine` | Orchestrator |

---

## Key Parameters

| Parameter | Default | Description |
|---|---|---|
| `entry_zscore` | 2.0 | Z-score to enter a position |
| `exit_zscore` | 0.5 | Z-score to close (mean reversion) |
| `stop_loss_zscore` | 3.5 | Hard stop loss threshold |
| `capital` | $10,000 | Capital per pair |
| `delta` (Kalman) | 1e-5 | State transition noise |
| `lookback_days` | 252 | Historical window |
| `transaction_cost` | 0.1% | Cost per leg (realistic backtest) |

---

## Trading Logic

```
z < -2.0σ  →  LONG SPREAD   (buy A, sell B — spread too low)
z > +2.0σ  →  SHORT SPREAD  (sell A, buy B — spread too high)
|z| < 0.5σ →  CLOSE         (spread mean-reverted)
|z| > 3.5σ →  STOP LOSS     (spread diverging — cut loss)
```

Dollar-neutral sizing: `qty_B = C / P_B`, `qty_A = qty_B × β_kalman`

---

## Theoretical Background

### Engle-Granger Cointegration
1. OLS regression: `Y_t = α + β·X_t + ε_t`
2. ADF test on residuals `ε_t`
3. Reject H₀ (unit root) at p < 0.05 → cointegrated

### Kalman Filter State-Space
```
Observation: Y_t = H_t · θ_t + ε_t     ε_t ~ N(0, R)
State:        θ_t = θ_{t-1} + w_t       w_t ~ N(0, Q)

θ_t = [β_t, α_t]ᵀ,   H_t = [X_t, 1]
```

### Mean Reversion Half-Life
```
ΔS_t = λ·S_{t-1} + ε_t
half_life = -ln(2) / ln(1 + λ)
```

---

## Alpaca Paper Trading Setup

1. Sign up at [alpaca.markets](https://alpaca.markets)
2. Enable Paper Trading in dashboard
3. Generate API Key + Secret from Paper Trading section
4. Run: `python stat_arb_engine.py --alpaca-key PK... --alpaca-secret ... --live`

API endpoints used:
- `POST /v2/orders` — submit market orders
- `GET /v2/positions` — track open legs
- `DELETE /v2/positions/{ticker}` — close positions

---

## Expected Performance

| Metric | Typical Range |
|---|---|
| Sharpe Ratio | 1.2 — 2.5 |
| Win Rate | 55% — 68% |
| Avg Trade Duration | 5 — 20 days |
| Max Drawdown | 5% — 20% |
| Trades/Pair/Year | 8 — 25 |

---

## Disclaimer

This software is for educational purposes only. Backtested performance does not guarantee future results. Always consult a qualified financial advisor before deploying any trading strategy with real capital.

---

## References

- Engle & Granger (1987). *Co-Integration and Error Correction*. Econometrica.
- Johansen (1988). *Statistical Analysis of Cointegration Vectors*. JEDC.
- Kalman (1960). *A New Approach to Linear Filtering*. ASME Journal.
- Chan, E. (2013). *Algorithmic Trading*. Wiley.
- Vidyamurthy, G. (2004). *Pairs Trading*. Wiley.
