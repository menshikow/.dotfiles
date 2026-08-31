# Order Book Visualization — Plan

## Approach

**Terminal live view + Python offline analysis.** Chosen over WebSocket/web dashboards for low infrastructure overhead; covers both live development and benchmark/portfolio reporting.

## 1. Data layer (decouples viz from the hot path)

- Event log (already in scope): `OrderAdded / Cancelled / Matched / OrderBookSnapshot` records.
- Snapshot dumping: a `BookSnapshot` (`timestamp, bids[[price,size]...], asks[[price,size]...], top-of-book, last trade`) serialized as **JSONL** to `snapshots.jsonl`.
  - Emitted by a subscriber thread consuming a lock-free queue — visualization never touches the matching hot path.
- `replay` mode: rebuild the book from the event log and re-dump snapshots, so viz works both live and offline.

## 2. Terminal live view — FTXUI

- Build system: `CMakeLists.txt` with `FetchContent` pulling `ftxui` (and GoogleTest later). Project currently has no CMake.
- Live demo binary (`orderbook_demo`): synthetic order-flow generator feeds the book; subscriber thread renders:
  - **Depth bars** — bid/ask levels as horizontal size-proportional bars, spread gap visible in the middle
  - **Top-of-book panel** — best bid/ask, spread, mid, last trade price
  - **Trades feed** — scrolling recent executions
  - **Stats row** — orders/sec, depth count, live p50/p99 latency
- FTXUI re-renders only on snapshot updates (event-driven, not busy-poll).

## 3. Python offline analysis — `scripts/plot_book.py`

- Deps: `pip install matplotlib` (+ optional `plotly`).
- From `snapshots.jsonl`, render:
  - **Depth chart** — classic step plot of cumulative bid/ask depth
  - **Depth heatmap** — price × time × size, the portfolio-ready artifact
  - **Mid-price & spread time series**
  - Optional **animated GIF/MP4** via `matplotlib.animation` (HackaTUM-demo-friendly)

## 4. Phasing (book comes first)

1. Core book (existing scope, not viz)
2. Event log + replay + JSONL snapshot dump
3. CMake + FTXUI live view
4. Python plotting scripts

## Open choices

- JSONL vs CSV for snapshots (chosen: JSONL — nested price/size arrays, simpler code).
- Tick grid density for the heatmap: re-use the book's bounded tick grid so heatmap axes are exact ticks.
