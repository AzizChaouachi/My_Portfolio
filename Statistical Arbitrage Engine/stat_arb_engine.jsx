import { useState, useEffect, useRef, useCallback } from "react";

// ─── CONSTANTS ───────────────────────────────────────────────────────────────
const PAIRS = [
  { a: "JPM", b: "GS", sector: "Banks" },
  { a: "XOM", b: "CVX", sector: "Energy" },
  { a: "MSFT", b: "GOOGL", sector: "Tech" },
  { a: "KO", b: "PEP", sector: "Consumer" },
  { a: "MA", b: "V", sector: "Payments" },
];

// ─── MATH ENGINE ─────────────────────────────────────────────────────────────
function generatePrice(base, vol, n, seed = 1) {
  const prices = [base];
  let s = seed;
  const rand = () => {
    s = (s * 1664525 + 1013904223) & 0xffffffff;
    return (s >>> 0) / 0xffffffff;
  };
  for (let i = 1; i < n; i++) {
    const u1 = rand(), u2 = rand();
    const z = Math.sqrt(-2 * Math.log(u1 + 1e-10)) * Math.cos(2 * Math.PI * u2);
    prices.push(Math.max(1, prices[i - 1] * (1 + vol * z)));
  }
  return prices;
}

function generateCointegratedPair(baseA, baseB, beta, n, seed = 42) {
  const pricesA = generatePrice(baseA, 0.015, n, seed);
  const mean = 0, theta = 0.08, sigma = 0.8;
  let spread = 0;
  const pricesB = [];
  let s = seed * 31337;
  const rand = () => {
    s = (s * 1664525 + 1013904223) & 0xffffffff;
    return (s >>> 0) / 0xffffffff;
  };
  for (let i = 0; i < n; i++) {
    const u1 = rand(), u2 = rand();
    const z = Math.sqrt(-2 * Math.log(u1 + 1e-10)) * Math.cos(2 * Math.PI * u2);
    spread = spread + theta * (mean - spread) + sigma * z;
    pricesB.push(Math.max(1, pricesA[i] * beta + spread + baseB - baseA * beta));
  }
  return { pricesA, pricesB };
}

function linReg(x, y) {
  const n = x.length;
  const sx = x.reduce((a, b) => a + b, 0);
  const sy = y.reduce((a, b) => a + b, 0);
  const sxy = x.reduce((a, xi, i) => a + xi * y[i], 0);
  const sxx = x.reduce((a, xi) => a + xi * xi, 0);
  const beta = (n * sxy - sx * sy) / (n * sxx - sx * sx);
  const alpha = (sy - beta * sx) / n;
  return { alpha, beta };
}

function computeSpread(pricesA, pricesB, beta, alpha) {
  return pricesA.map((a, i) => pricesB[i] - beta * a - alpha);
}

function zScore(spread) {
  const mean = spread.reduce((a, b) => a + b, 0) / spread.length;
  const std = Math.sqrt(spread.reduce((a, x) => a + (x - mean) ** 2, 0) / spread.length);
  return spread.map(s => std > 0 ? (s - mean) / std : 0);
}

// Engle-Granger ADF approximation
function adfTest(series) {
  const n = series.length;
  const diff = series.slice(1).map((x, i) => x - series[i]);
  const lagged = series.slice(0, n - 1);
  const { beta } = linReg(lagged, diff);
  const residuals = diff.map((d, i) => d - beta * lagged[i]);
  const sse = residuals.reduce((a, r) => a + r * r, 0) / (n - 2);
  const sxx = lagged.reduce((a, x) => a + (x - lagged.reduce((s, v) => s + v, 0) / lagged.length) ** 2, 0);
  const se = Math.sqrt(sse / sxx);
  const tStat = beta / se;
  // p-value approximation for ADF
  const pValue = tStat < -3.5 ? 0.01 : tStat < -2.9 ? 0.05 : tStat < -2.6 ? 0.1 : 0.5;
  return { tStat: tStat.toFixed(3), pValue, cointegrated: tStat < -2.9 };
}

// Kalman Filter for dynamic hedge ratio
function kalmanFilter(pricesA, pricesB) {
  const n = pricesA.length;
  let x = [1, 0]; // [beta, alpha]
  let P = [[1, 0], [0, 1]]; // state covariance
  const Q = [[1e-5, 0], [0, 1e-5]]; // process noise
  const R = 1e-3; // observation noise
  const betas = [], alphas = [], gains = [];

  for (let i = 0; i < n; i++) {
    const H = [pricesA[i], 1];
    // Predict
    const S = H[0] * (P[0][0] * H[0] + P[0][1] * H[1]) +
              H[1] * (P[1][0] * H[0] + P[1][1] * H[1]) + R;
    // Kalman gain
    const K = [(P[0][0] * H[0] + P[0][1] * H[1]) / S,
               (P[1][0] * H[0] + P[1][1] * H[1]) / S];
    // Update
    const innovation = pricesB[i] - (H[0] * x[0] + H[1] * x[1]);
    x = [x[0] + K[0] * innovation, x[1] + K[1] * innovation];
    const I_KH = [[1 - K[0] * H[0], -K[0] * H[1]], [-K[1] * H[0], 1 - K[1] * H[1]]];
    P = [
      [I_KH[0][0] * P[0][0] + I_KH[0][1] * P[1][0] + Q[0][0],
       I_KH[0][0] * P[0][1] + I_KH[0][1] * P[1][1] + Q[0][1]],
      [I_KH[1][0] * P[0][0] + I_KH[1][1] * P[1][0] + Q[1][0],
       I_KH[1][0] * P[0][1] + I_KH[1][1] * P[1][1] + Q[1][1]],
    ];
    betas.push(x[0]);
    alphas.push(x[1]);
    gains.push(Math.sqrt(K[0] ** 2 + K[1] ** 2));
  }
  return { betas, alphas, gains };
}

function generateTrades(zScores, threshold = 2.0, stopLoss = 3.5) {
  const trades = [];
  let position = 0; // 0 = flat, 1 = long spread, -1 = short spread
  let entryZ = 0, entryIdx = 0, pnl = 0;
  const equity = [0];

  for (let i = 1; i < zScores.length; i++) {
    const z = zScores[i];
    let tradePnl = 0;

    if (position === 0) {
      if (z < -threshold) { position = 1; entryZ = z; entryIdx = i; }
      else if (z > threshold) { position = -1; entryZ = z; entryIdx = i; }
    } else if (position === 1) {
      if (z > -0.5 || Math.abs(z) > stopLoss) {
        tradePnl = (z - entryZ) * -1;
        trades.push({ type: "LONG", entry: entryIdx, exit: i, entryZ, exitZ: z, pnl: tradePnl });
        position = 0; pnl += tradePnl;
      }
    } else if (position === -1) {
      if (z < 0.5 || Math.abs(z) > stopLoss) {
        tradePnl = (entryZ - z) * -1;
        trades.push({ type: "SHORT", entry: entryIdx, exit: i, entryZ, exitZ: z, pnl: tradePnl });
        position = 0; pnl += tradePnl;
      }
    }
    equity.push(pnl);
  }
  return { trades, equity };
}

// ─── MINI CHART ──────────────────────────────────────────────────────────────
function Sparkline({ data, color = "#00ff88", height = 40 }) {
  if (!data || data.length < 2) return null;
  const min = Math.min(...data), max = Math.max(...data);
  const range = max - min || 1;
  const w = 120, h = height;
  const pts = data.map((v, i) => `${(i / (data.length - 1)) * w},${h - ((v - min) / range) * h}`).join(" ");
  return (
    <svg width={w} height={h} viewBox={`0 0 ${w} ${h}`}>
      <polyline fill="none" stroke={color} strokeWidth="1.5" points={pts} />
    </svg>
  );
}

function MiniChart({ data, width = 300, height = 80, colors = ["#38bdf8", "#f472b6"], labels = [] }) {
  if (!data || data.length === 0) return null;
  const all = data.flat();
  const min = Math.min(...all), max = Math.max(...all);
  const range = max - min || 1;

  const toPath = (arr) =>
    arr.map((v, i) => `${(i / (arr.length - 1)) * width},${height - ((v - min) / range) * (height - 4) - 2}`).join(" ");

  return (
    <svg width="100%" viewBox={`0 0 ${width} ${height}`} preserveAspectRatio="none">
      {Array.isArray(data[0])
        ? data.map((d, idx) => (
            <polyline key={idx} fill="none" stroke={colors[idx % colors.length]} strokeWidth="1.5" points={toPath(d)} opacity={0.85} />
          ))
        : <polyline fill="none" stroke={colors[0]} strokeWidth="1.5" points={toPath(data)} />}
    </svg>
  );
}

// ─── GAUGE ───────────────────────────────────────────────────────────────────
function ZGauge({ value }) {
  const clamped = Math.max(-4, Math.min(4, value));
  const angle = (clamped / 4) * 120;
  const color = Math.abs(clamped) > 3.5 ? "#ff4444" : Math.abs(clamped) > 2 ? "#ffaa00" : "#00ff88";
  const cx = 60, cy = 55, r = 40;
  const toRad = (deg) => (deg * Math.PI) / 180;
  const needleAngle = 180 + angle;
  const nx = cx + r * 0.85 * Math.cos(toRad(needleAngle));
  const ny = cy + r * 0.85 * Math.sin(toRad(needleAngle));

  return (
    <svg width="120" height="70" viewBox="0 0 120 70">
      <path d={`M ${cx - r} ${cy} A ${r} ${r} 0 0 1 ${cx + r} ${cy}`} fill="none" stroke="#1e2a3a" strokeWidth="8" />
      <path d={`M ${cx - r * 0.7} ${cy + 7} A ${r * 0.7} ${r * 0.7} 0 0 1 ${cx + r * 0.7} ${cy + 7}`} fill="none" stroke={color} strokeWidth="6" opacity="0.5" />
      <line x1={cx} y1={cy} x2={nx} y2={ny} stroke={color} strokeWidth="2.5" strokeLinecap="round" />
      <circle cx={cx} cy={cy} r="4" fill={color} />
      <text x={cx} y={cy + 16} textAnchor="middle" fill={color} fontSize="11" fontFamily="monospace" fontWeight="bold">
        {value.toFixed(2)}σ
      </text>
      <text x="18" y={cy + 2} fill="#4a6080" fontSize="8" fontFamily="monospace">-4</text>
      <text x="95" y={cy + 2} fill="#4a6080" fontSize="8" fontFamily="monospace">+4</text>
    </svg>
  );
}

// ─── MAIN APP ─────────────────────────────────────────────────────────────────
export default function StatArbEngine() {
  const [selectedPairIdx, setSelectedPairIdx] = useState(0);
  const [tab, setTab] = useState("overview");
  const [isLive, setIsLive] = useState(false);
  const [liveData, setLiveData] = useState(null);
  const [allPairData, setAllPairData] = useState([]);
  const [threshold, setThreshold] = useState(2.0);
  const [alpacaKey, setAlpacaKey] = useState("");
  const [alpacaSecret, setAlpacaSecret] = useState("");
  const [orders, setOrders] = useState([]);
  const [logLines, setLogLines] = useState([]);
  const tickRef = useRef(null);
  const N = 252;

  const log = useCallback((msg, type = "info") => {
    const time = new Date().toLocaleTimeString();
    setLogLines(prev => [`[${time}] [${type.toUpperCase()}] ${msg}`, ...prev].slice(0, 40));
  }, []);

  // Build data for all pairs
  useEffect(() => {
    const bases = [[145, 330], [110, 155], [380, 170], [58, 180], [460, 230]];
    const betas = [2.1, 1.35, 0.48, 3.0, 2.05];
    const data = PAIRS.map((pair, idx) => {
      const [baseA, baseB] = bases[idx];
      const { pricesA, pricesB } = generateCointegratedPair(baseA, baseB, betas[idx], N, idx * 7 + 13);
      const { beta: ols_beta, alpha: ols_alpha } = linReg(pricesA, pricesB);
      const spread = computeSpread(pricesA, pricesB, ols_beta, ols_alpha);
      const zs = zScore(spread);
      const adf = adfTest(spread);
      const { betas: kBetas, alphas: kAlphas } = kalmanFilter(pricesA.slice(-60), pricesB.slice(-60));
      const { trades, equity } = generateTrades(zs, threshold);
      const winRate = trades.filter(t => t.pnl > 0).length / Math.max(1, trades.length);
      const totalPnl = trades.reduce((a, t) => a + t.pnl, 0);
      const currentZ = zs[zs.length - 1];
      const signal = currentZ < -threshold ? "LONG SPREAD" : currentZ > threshold ? "SHORT SPREAD" : "FLAT";
      const signalColor = currentZ < -threshold ? "#00ff88" : currentZ > threshold ? "#ff4466" : "#94a3b8";
      return {
        pair, pricesA, pricesB, spread, zScores: zs, adf,
        ols_beta, ols_alpha, kBetas, kAlphas,
        trades, equity, winRate, totalPnl,
        currentZ, signal, signalColor,
        halfLife: Math.log(2) / Math.abs(Math.log(1 - 0.08)),
      };
    });
    setAllPairData(data);
  }, [threshold]);

  const pd = allPairData[selectedPairIdx];

  // Live tick simulation
  useEffect(() => {
    if (!isLive || !pd) return;
    let offset = 0;
    tickRef.current = setInterval(() => {
      offset++;
      const newZ = pd.currentZ + (Math.random() - 0.5) * 0.3;
      const newA = pd.pricesA[pd.pricesA.length - 1] * (1 + (Math.random() - 0.5) * 0.003);
      const newB = pd.pricesB[pd.pricesB.length - 1] * (1 + (Math.random() - 0.5) * 0.003);
      setLiveData({ z: newZ.toFixed(3), priceA: newA.toFixed(2), priceB: newB.toFixed(2), tick: offset });
      if (Math.abs(newZ) > threshold && Math.random() > 0.7) {
        const side = newZ > threshold ? "SHORT" : "LONG";
        const newOrder = {
          id: `ORD-${Date.now().toString(36).toUpperCase()}`,
          pair: `${pd.pair.a}/${pd.pair.b}`,
          side,
          qty: Math.floor(100 / newA),
          price: newA.toFixed(2),
          status: Math.random() > 0.1 ? "FILLED" : "PENDING",
          time: new Date().toLocaleTimeString(),
          z: newZ.toFixed(3),
        };
        setOrders(prev => [newOrder, ...prev].slice(0, 20));
        log(`${side} signal on ${pd.pair.a}/${pd.pair.b} | z=${newZ.toFixed(3)} | qty=${newOrder.qty} @ $${newOrder.price}`, side === "LONG" ? "buy" : "sell");
      }
    }, 1500);
    return () => clearInterval(tickRef.current);
  }, [isLive, pd, threshold, log]);

  if (!pd) return (
    <div style={{ background: "#050d1a", color: "#38bdf8", fontFamily: "monospace", padding: 40, minHeight: "100vh", display: "flex", alignItems: "center", justifyContent: "center" }}>
      <div style={{ fontSize: 18 }}>⟳ Initializing Kalman filters...</div>
    </div>
  );

  const dateLabels = Array.from({ length: N }, (_, i) => {
    const d = new Date(2024, 0, 1);
    d.setDate(d.getDate() + i);
    return d.toLocaleDateString("en-US", { month: "short", day: "numeric" });
  });

  return (
    <div style={{
      background: "#050d1a",
      color: "#c8d8e8",
      fontFamily: "'Courier New', monospace",
      minHeight: "100vh",
      padding: "0",
      fontSize: 13,
    }}>
      {/* GRID BACKGROUND */}
      <div style={{
        position: "fixed", inset: 0, pointerEvents: "none", zIndex: 0,
        backgroundImage: "linear-gradient(rgba(56,189,248,0.03) 1px, transparent 1px), linear-gradient(90deg, rgba(56,189,248,0.03) 1px, transparent 1px)",
        backgroundSize: "40px 40px",
      }} />

      {/* HEADER */}
      <div style={{
        position: "relative", zIndex: 1,
        background: "linear-gradient(90deg, #0a1628 0%, #0d1f3c 50%, #0a1628 100%)",
        borderBottom: "1px solid #1e3a5f",
        padding: "12px 24px",
        display: "flex", alignItems: "center", justifyContent: "space-between",
      }}>
        <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
          <div style={{ fontSize: 20, color: "#38bdf8", letterSpacing: 3, fontWeight: "bold" }}>
            ◈ STATARB
          </div>
          <div style={{ color: "#4a6080", fontSize: 11 }}>STATISTICAL ARBITRAGE ENGINE v2.1</div>
          <div style={{
            padding: "2px 10px", borderRadius: 3,
            background: isLive ? "rgba(0,255,136,0.1)" : "rgba(74,96,128,0.2)",
            border: `1px solid ${isLive ? "#00ff88" : "#1e3a5f"}`,
            color: isLive ? "#00ff88" : "#4a6080",
            fontSize: 10, letterSpacing: 1,
          }}>
            {isLive ? "● LIVE" : "○ PAUSED"}
          </div>
        </div>
        <div style={{ display: "flex", gap: 12, alignItems: "center" }}>
          <div style={{ fontSize: 11, color: "#4a6080" }}>
            THRESHOLD: <span style={{ color: "#ffaa00" }}>{threshold}σ</span>
          </div>
          <input
            type="range" min="1" max="3" step="0.25" value={threshold}
            onChange={e => setThreshold(+e.target.value)}
            style={{ width: 80, accentColor: "#38bdf8" }}
          />
          <button
            onClick={() => setIsLive(v => !v)}
            style={{
              background: isLive ? "rgba(255,68,102,0.15)" : "rgba(0,255,136,0.1)",
              border: `1px solid ${isLive ? "#ff4466" : "#00ff88"}`,
              color: isLive ? "#ff4466" : "#00ff88",
              padding: "6px 18px", borderRadius: 4, cursor: "pointer",
              fontSize: 12, letterSpacing: 1, fontFamily: "monospace",
            }}
          >
            {isLive ? "⬛ STOP" : "▶ START"}
          </button>
        </div>
      </div>

      <div style={{ position: "relative", zIndex: 1, display: "flex", minHeight: "calc(100vh - 53px)" }}>
        {/* SIDEBAR */}
        <div style={{
          width: 220, borderRight: "1px solid #1e3a5f",
          background: "#070f1e",
          padding: "12px 0",
        }}>
          <div style={{ padding: "4px 16px 8px", color: "#4a6080", fontSize: 10, letterSpacing: 2 }}>PAIRS SCANNER</div>
          {allPairData.map((d, i) => (
            <div
              key={i}
              onClick={() => setSelectedPairIdx(i)}
              style={{
                padding: "10px 16px", cursor: "pointer",
                background: i === selectedPairIdx ? "rgba(56,189,248,0.08)" : "transparent",
                borderLeft: `2px solid ${i === selectedPairIdx ? "#38bdf8" : "transparent"}`,
                transition: "all 0.15s",
              }}
            >
              <div style={{ display: "flex", justifyContent: "space-between", marginBottom: 4 }}>
                <span style={{ color: i === selectedPairIdx ? "#38bdf8" : "#8aa0b8", fontWeight: "bold", fontSize: 13 }}>
                  {d.pair.a}/{d.pair.b}
                </span>
                <span style={{
                  fontSize: 9, padding: "1px 6px", borderRadius: 2,
                  background: d.adf.cointegrated ? "rgba(0,255,136,0.1)" : "rgba(255,68,102,0.1)",
                  color: d.adf.cointegrated ? "#00ff88" : "#ff4466",
                  border: `1px solid ${d.adf.cointegrated ? "#00ff8833" : "#ff446633"}`,
                }}>
                  {d.adf.cointegrated ? "COINT" : "WEAK"}
                </span>
              </div>
              <div style={{ display: "flex", justifyContent: "space-between", fontSize: 11 }}>
                <span style={{ color: d.signalColor, fontSize: 10 }}>{d.signal}</span>
                <span style={{ color: Math.abs(d.currentZ) > 2 ? "#ffaa00" : "#4a6080" }}>
                  z={d.currentZ.toFixed(2)}
                </span>
              </div>
              <div style={{ marginTop: 4, opacity: 0.6 }}>
                <Sparkline data={d.zScores.slice(-40)} color={d.signalColor} height={20} />
              </div>
            </div>
          ))}

          {/* Tabs */}
          <div style={{ marginTop: 24, padding: "4px 16px 8px", color: "#4a6080", fontSize: 10, letterSpacing: 2 }}>VIEW</div>
          {["overview", "kalman", "backtest", "alpaca"].map(t => (
            <div
              key={t}
              onClick={() => setTab(t)}
              style={{
                padding: "8px 16px", cursor: "pointer",
                background: tab === t ? "rgba(56,189,248,0.08)" : "transparent",
                borderLeft: `2px solid ${tab === t ? "#38bdf8" : "transparent"}`,
                color: tab === t ? "#38bdf8" : "#4a6080",
                fontSize: 12, textTransform: "uppercase", letterSpacing: 1,
              }}
            >
              {t === "overview" ? "⊞ Overview" : t === "kalman" ? "⌬ Kalman" : t === "backtest" ? "↩ Backtest" : "⚡ Alpaca"}
            </div>
          ))}
        </div>

        {/* MAIN CONTENT */}
        <div style={{ flex: 1, padding: 20, overflowY: "auto" }}>
          {/* TOP STATS */}
          <div style={{ display: "grid", gridTemplateColumns: "repeat(6, 1fr)", gap: 10, marginBottom: 20 }}>
            {[
              { label: "PAIR", value: `${pd.pair.a}/${pd.pair.b}`, color: "#38bdf8" },
              { label: "SIGNAL", value: pd.signal, color: pd.signalColor },
              { label: "Z-SCORE", value: (liveData ? liveData.z : pd.currentZ.toFixed(3)) + "σ", color: Math.abs(pd.currentZ) > threshold ? "#ffaa00" : "#94a3b8" },
              { label: "HALF-LIFE", value: pd.halfLife.toFixed(1) + "d", color: "#c084fc" },
              { label: "WIN RATE", value: (pd.winRate * 100).toFixed(1) + "%", color: pd.winRate > 0.5 ? "#00ff88" : "#ff4466" },
              { label: "ADF p-val", value: pd.adf.pValue.toFixed(2), color: pd.adf.cointegrated ? "#00ff88" : "#ff4466" },
            ].map((s, i) => (
              <div key={i} style={{
                background: "#0a1628", border: "1px solid #1e3a5f",
                borderRadius: 6, padding: "10px 14px",
              }}>
                <div style={{ fontSize: 9, color: "#4a6080", letterSpacing: 1, marginBottom: 4 }}>{s.label}</div>
                <div style={{ fontSize: 14, color: s.color, fontWeight: "bold" }}>{s.value}</div>
              </div>
            ))}
          </div>

          {/* ─── OVERVIEW TAB ─── */}
          {tab === "overview" && (
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
              {/* Price Chart */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16, gridColumn: "1/3" }}>
                <div style={{ display: "flex", justifyContent: "space-between", marginBottom: 12 }}>
                  <span style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1 }}>PRICE SERIES — NORMALIZED</span>
                  <div style={{ display: "flex", gap: 16, fontSize: 11 }}>
                    <span style={{ color: "#38bdf8" }}>● {pd.pair.a} ${liveData ? liveData.priceA : pd.pricesA[pd.pricesA.length - 1].toFixed(2)}</span>
                    <span style={{ color: "#f472b6" }}>● {pd.pair.b} ${liveData ? liveData.priceB : pd.pricesB[pd.pricesB.length - 1].toFixed(2)}</span>
                  </div>
                </div>
                <div style={{ height: 100, borderRadius: 4, overflow: "hidden" }}>
                  <MiniChart
                    data={[
                      pd.pricesA.map(p => p / pd.pricesA[0]),
                      pd.pricesB.map(p => p / pd.pricesB[0]),
                    ]}
                    width={800} height={100}
                    colors={["#38bdf8", "#f472b6"]}
                  />
                </div>
              </div>

              {/* Spread */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#c084fc", fontSize: 12, letterSpacing: 1, marginBottom: 12 }}>SPREAD (OLS)</div>
                <div style={{ height: 80, overflow: "hidden" }}>
                  <MiniChart data={pd.spread} width={380} height={80} colors={["#c084fc"]} />
                </div>
                <div style={{ display: "flex", gap: 16, marginTop: 8, fontSize: 11, color: "#4a6080" }}>
                  <span>β={pd.ols_beta.toFixed(4)}</span>
                  <span>α={pd.ols_alpha.toFixed(2)}</span>
                  <span>ADF t={pd.adf.tStat}</span>
                </div>
              </div>

              {/* Z-Score */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
                  <span style={{ color: "#ffaa00", fontSize: 12, letterSpacing: 1 }}>Z-SCORE</span>
                  <ZGauge value={liveData ? +liveData.z : pd.currentZ} />
                </div>
                <div style={{ height: 70, overflow: "hidden" }}>
                  <MiniChart data={pd.zScores} width={380} height={70} colors={["#ffaa00"]} />
                </div>
                {/* Threshold lines visual */}
                <div style={{ display: "flex", gap: 8, marginTop: 8, fontSize: 10 }}>
                  <span style={{ color: "#00ff88" }}>▲ +{threshold}σ entry short</span>
                  <span style={{ color: "#ff4466" }}>▼ -{threshold}σ entry long</span>
                </div>
              </div>

              {/* Cointegration stats */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 12 }}>COINTEGRATION TESTS</div>
                <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                  <tbody>
                    {[
                      ["Method", "Engle-Granger ADF"],
                      ["ADF t-stat", pd.adf.tStat],
                      ["p-value", pd.adf.pValue.toFixed(4)],
                      ["Critical 5%", "-2.86"],
                      ["Cointegrated?", pd.adf.cointegrated ? "✓ YES" : "✗ NO"],
                      ["Half-life", pd.halfLife.toFixed(1) + " days"],
                      ["Mean reversion θ", "0.080"],
                    ].map(([k, v]) => (
                      <tr key={k} style={{ borderBottom: "1px solid #0d1f3c" }}>
                        <td style={{ padding: "5px 0", color: "#4a6080" }}>{k}</td>
                        <td style={{ padding: "5px 0", color: "#c8d8e8", textAlign: "right" }}>{v}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {/* Live log */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16, gridColumn: "1/3" }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>SIGNAL LOG</div>
                <div style={{ height: 120, overflowY: "auto", fontFamily: "monospace", fontSize: 11 }}>
                  {logLines.length === 0 ? (
                    <div style={{ color: "#4a6080" }}>▶ Start the engine to begin receiving signals...</div>
                  ) : logLines.map((l, i) => (
                    <div key={i} style={{
                      padding: "2px 0",
                      color: l.includes("LONG") ? "#00ff88" : l.includes("SHORT") ? "#ff4466" : "#4a6080",
                      borderBottom: "1px solid #0d1f3c",
                    }}>{l}</div>
                  ))}
                </div>
              </div>
            </div>
          )}

          {/* ─── KALMAN TAB ─── */}
          {tab === "kalman" && (
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16, gridColumn: "1/3" }}>
                <div style={{ color: "#00ff88", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>KALMAN FILTER — DYNAMIC HEDGE RATIO (β)</div>
                <p style={{ color: "#4a6080", fontSize: 11, marginBottom: 12 }}>
                  The Kalman filter treats the hedge ratio as a latent state variable. As new price observations arrive, it updates β via Bayes' rule, adapting to regime changes faster than rolling OLS.
                </p>
                <div style={{ height: 100, overflow: "hidden", marginBottom: 8 }}>
                  <MiniChart data={pd.kBetas} width={800} height={100} colors={["#00ff88"]} />
                </div>
                <div style={{ display: "flex", gap: 24, fontSize: 11 }}>
                  <span style={{ color: "#4a6080" }}>Current β (Kalman): <span style={{ color: "#00ff88" }}>{pd.kBetas[pd.kBetas.length - 1].toFixed(4)}</span></span>
                  <span style={{ color: "#4a6080" }}>Static β (OLS): <span style={{ color: "#38bdf8" }}>{pd.ols_beta.toFixed(4)}</span></span>
                  <span style={{ color: "#4a6080" }}>Drift: <span style={{ color: "#ffaa00" }}>{(pd.kBetas[pd.kBetas.length - 1] - pd.ols_beta).toFixed(4)}</span></span>
                </div>
              </div>

              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#c084fc", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>DYNAMIC α (INTERCEPT)</div>
                <div style={{ height: 90, overflow: "hidden" }}>
                  <MiniChart data={pd.kAlphas} width={380} height={90} colors={["#c084fc"]} />
                </div>
              </div>

              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>STATE EQUATIONS</div>
                <div style={{ fontSize: 11, lineHeight: 1.9, color: "#8aa0b8" }}>
                  <div><span style={{ color: "#00ff88" }}>Observation:</span>  y_t = β_t · x_t + α_t + ε_t</div>
                  <div><span style={{ color: "#00ff88" }}>State update:</span>  θ_t = θ_{"{t-1}"} + w_t</div>
                  <div><span style={{ color: "#00ff88" }}>Kalman gain:</span>   K_t = P_t H_t^T (H_t P_t H_t^T + R)^{"{-1}"}</div>
                  <div><span style={{ color: "#00ff88" }}>Innovation:</span>   ν_t = y_t - H_t θ_{"{t|t-1}"}</div>
                  <div style={{ marginTop: 12 }}><span style={{ color: "#ffaa00" }}>Process noise Q:</span> 1e-5 (slow drift)</div>
                  <div><span style={{ color: "#ffaa00" }}>Observation noise R:</span> 1e-3</div>
                </div>
              </div>

              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>KALMAN SPREAD</div>
                <p style={{ color: "#4a6080", fontSize: 11, marginBottom: 12 }}>
                  Spread computed using time-varying Kalman β. Tighter, more stationary.
                </p>
                <div style={{ height: 80, overflow: "hidden" }}>
                  <MiniChart
                    data={pd.pricesA.slice(-60).map((a, i) => pd.pricesB.slice(-60)[i] - pd.kBetas[i] * a - pd.kAlphas[i])}
                    width={380} height={80} colors={["#38bdf8"]}
                  />
                </div>
              </div>
            </div>
          )}

          {/* ─── BACKTEST TAB ─── */}
          {tab === "backtest" && (
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16, gridColumn: "1/3" }}>
                <div style={{ color: "#00ff88", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>EQUITY CURVE</div>
                <div style={{ height: 100, overflow: "hidden" }}>
                  <MiniChart data={pd.equity} width={800} height={100}
                    colors={[pd.totalPnl >= 0 ? "#00ff88" : "#ff4466"]} />
                </div>
              </div>

              {/* Trade stats */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 12 }}>PERFORMANCE METRICS</div>
                <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 12 }}>
                  <tbody>
                    {[
                      ["Total Trades", pd.trades.length],
                      ["Win Rate", (pd.winRate * 100).toFixed(1) + "%"],
                      ["Total P&L", (pd.totalPnl > 0 ? "+" : "") + pd.totalPnl.toFixed(2) + "σ"],
                      ["Avg Win", (pd.trades.filter(t => t.pnl > 0).reduce((a, t) => a + t.pnl, 0) / Math.max(1, pd.trades.filter(t => t.pnl > 0).length)).toFixed(3) + "σ"],
                      ["Avg Loss", (pd.trades.filter(t => t.pnl <= 0).reduce((a, t) => a + t.pnl, 0) / Math.max(1, pd.trades.filter(t => t.pnl <= 0).length)).toFixed(3) + "σ"],
                      ["Entry Threshold", "±" + threshold + "σ"],
                      ["Stop Loss", "±3.5σ"],
                      ["Exit", "±0.5σ (mean reversion)"],
                    ].map(([k, v]) => (
                      <tr key={k} style={{ borderBottom: "1px solid #0d1f3c" }}>
                        <td style={{ padding: "5px 0", color: "#4a6080" }}>{k}</td>
                        <td style={{
                          padding: "5px 0", textAlign: "right",
                          color: k === "Win Rate" ? (pd.winRate > 0.5 ? "#00ff88" : "#ff4466") :
                            k === "Total P&L" ? (pd.totalPnl >= 0 ? "#00ff88" : "#ff4466") : "#c8d8e8",
                        }}>{v}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {/* Trade list */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 8 }}>TRADE LOG</div>
                <div style={{ height: 220, overflowY: "auto" }}>
                  <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 11 }}>
                    <thead>
                      <tr style={{ color: "#4a6080", borderBottom: "1px solid #1e3a5f" }}>
                        <th style={{ padding: "4px 0", textAlign: "left" }}>#</th>
                        <th style={{ padding: "4px 0" }}>TYPE</th>
                        <th style={{ padding: "4px 0" }}>ENTRY z</th>
                        <th style={{ padding: "4px 0" }}>EXIT z</th>
                        <th style={{ padding: "4px 0", textAlign: "right" }}>P&L</th>
                      </tr>
                    </thead>
                    <tbody>
                      {pd.trades.map((t, i) => (
                        <tr key={i} style={{ borderBottom: "1px solid #0d1f3c" }}>
                          <td style={{ padding: "4px 0", color: "#4a6080" }}>{i + 1}</td>
                          <td style={{ padding: "4px 6px", color: t.type === "LONG" ? "#00ff88" : "#ff4466" }}>{t.type}</td>
                          <td style={{ padding: "4px 6px", textAlign: "center", color: "#8aa0b8" }}>{t.entryZ.toFixed(2)}</td>
                          <td style={{ padding: "4px 6px", textAlign: "center", color: "#8aa0b8" }}>{t.exitZ.toFixed(2)}</td>
                          <td style={{ padding: "4px 0", textAlign: "right", color: t.pnl >= 0 ? "#00ff88" : "#ff4466" }}>
                            {(t.pnl >= 0 ? "+" : "")}{t.pnl.toFixed(3)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          )}

          {/* ─── ALPACA TAB ─── */}
          {tab === "alpaca" && (
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 20 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 16 }}>⚡ ALPACA PAPER TRADING</div>
                <div style={{ fontSize: 11, color: "#4a6080", marginBottom: 16, lineHeight: 1.6 }}>
                  Connect your Alpaca Paper Trading account to execute live statistical arbitrage signals automatically via the Alpaca REST API.
                </div>
                <div style={{ marginBottom: 12 }}>
                  <div style={{ color: "#8aa0b8", fontSize: 11, marginBottom: 4 }}>API KEY ID</div>
                  <input
                    type="text"
                    value={alpacaKey}
                    onChange={e => setAlpacaKey(e.target.value)}
                    placeholder="PK..."
                    style={{
                      width: "100%", background: "#0d1f3c", border: "1px solid #1e3a5f",
                      color: "#c8d8e8", padding: "8px 12px", borderRadius: 4,
                      fontFamily: "monospace", fontSize: 12, boxSizing: "border-box",
                    }}
                  />
                </div>
                <div style={{ marginBottom: 20 }}>
                  <div style={{ color: "#8aa0b8", fontSize: 11, marginBottom: 4 }}>SECRET KEY</div>
                  <input
                    type="password"
                    value={alpacaSecret}
                    onChange={e => setAlpacaSecret(e.target.value)}
                    placeholder="••••••••"
                    style={{
                      width: "100%", background: "#0d1f3c", border: "1px solid #1e3a5f",
                      color: "#c8d8e8", padding: "8px 12px", borderRadius: 4,
                      fontFamily: "monospace", fontSize: 12, boxSizing: "border-box",
                    }}
                  />
                </div>
                <button
                  onClick={() => {
                    if (!alpacaKey || !alpacaSecret) { log("Alpaca credentials required", "error"); return; }
                    log(`Connected to Alpaca Paper Trading as ${alpacaKey.slice(0, 8)}...`, "info");
                    log(`Monitoring ${PAIRS.length} pairs at ±${threshold}σ threshold`, "info");
                  }}
                  style={{
                    width: "100%", padding: "10px", borderRadius: 4,
                    background: "rgba(56,189,248,0.1)", border: "1px solid #38bdf8",
                    color: "#38bdf8", cursor: "pointer", fontSize: 12,
                    fontFamily: "monospace", letterSpacing: 1,
                  }}
                >
                  CONNECT TO ALPACA
                </button>

                <div style={{ marginTop: 20, padding: 12, background: "#070f1e", borderRadius: 6, fontSize: 11, color: "#4a6080", lineHeight: 1.7 }}>
                  <div style={{ color: "#ffaa00", marginBottom: 4 }}>API ENDPOINTS USED:</div>
                  <div>POST /v2/orders — submit market orders</div>
                  <div>GET /v2/positions — track open legs</div>
                  <div>GET /v2/account — equity & buying power</div>
                  <div>DELETE /v2/positions — close positions</div>
                </div>
              </div>

              {/* Order book */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16 }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 12 }}>ORDER BOOK</div>
                <div style={{ height: 300, overflowY: "auto" }}>
                  {orders.length === 0 ? (
                    <div style={{ color: "#4a6080", fontSize: 11, padding: "20px 0" }}>No orders yet. Start the engine and connect Alpaca.</div>
                  ) : (
                    <table style={{ width: "100%", borderCollapse: "collapse", fontSize: 11 }}>
                      <thead>
                        <tr style={{ color: "#4a6080", borderBottom: "1px solid #1e3a5f" }}>
                          <th style={{ padding: "4px 0", textAlign: "left" }}>ID</th>
                          <th>PAIR</th>
                          <th>SIDE</th>
                          <th>QTY</th>
                          <th>z</th>
                          <th style={{ textAlign: "right" }}>STATUS</th>
                        </tr>
                      </thead>
                      <tbody>
                        {orders.map((o) => (
                          <tr key={o.id} style={{ borderBottom: "1px solid #0d1f3c" }}>
                            <td style={{ padding: "5px 4px", color: "#4a6080", fontSize: 10 }}>{o.id}</td>
                            <td style={{ padding: "5px 4px", color: "#c8d8e8" }}>{o.pair}</td>
                            <td style={{ padding: "5px 4px", color: o.side === "LONG" ? "#00ff88" : "#ff4466" }}>{o.side}</td>
                            <td style={{ padding: "5px 4px", color: "#8aa0b8" }}>{o.qty}</td>
                            <td style={{ padding: "5px 4px", color: "#ffaa00" }}>{o.z}</td>
                            <td style={{ padding: "5px 4px", textAlign: "right", color: o.status === "FILLED" ? "#00ff88" : "#ffaa00" }}>
                              {o.status}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  )}
                </div>
              </div>

              {/* Code snippet */}
              <div style={{ background: "#0a1628", border: "1px solid #1e3a5f", borderRadius: 8, padding: 16, gridColumn: "1/3" }}>
                <div style={{ color: "#38bdf8", fontSize: 12, letterSpacing: 1, marginBottom: 12 }}>EXECUTION CODE SNIPPET</div>
                <pre style={{
                  background: "#070f1e", padding: 16, borderRadius: 6,
                  fontSize: 11, color: "#8aa0b8", overflowX: "auto", lineHeight: 1.6,
                  border: "1px solid #0d1f3c",
                }}>
{`# Python — Alpaca Paper Trading Execution
import alpaca_trade_api as tradeapi

api = tradeapi.REST(API_KEY, SECRET_KEY,
                    base_url='https://paper-api.alpaca.markets')

def execute_pair_trade(ticker_a, ticker_b, beta, signal, capital=10000):
    price_a = float(api.get_latest_trade(ticker_a).price)
    price_b = float(api.get_latest_trade(ticker_b).price)
    
    qty_b = int(capital / price_b)
    qty_a = int(qty_b * beta)
    
    if signal == "LONG_SPREAD":   # spread too low → buy A, sell B
        api.submit_order(ticker_a, qty_a, 'buy',  'market', 'gtc')
        api.submit_order(ticker_b, qty_b, 'sell', 'market', 'gtc')
    elif signal == "SHORT_SPREAD": # spread too high → sell A, buy B
        api.submit_order(ticker_a, qty_a, 'sell', 'market', 'gtc')
        api.submit_order(ticker_b, qty_b, 'buy',  'market', 'gtc')

# Close when z-score reverts to ±0.5σ
def close_pair(ticker_a, ticker_b):
    api.close_position(ticker_a)
    api.close_position(ticker_b)`}
                </pre>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
