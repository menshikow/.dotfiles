The highest bid and the lowest ask are called the top of book or inside market. If someone's bidding $99.50 and someone else is asking $100.10, there's a gap — that gap is the spread — and no trade happens yet.


This is matching: whenever the best bid ≥ the best ask, a trade is possible, and the book's job is to execute it and update itself.

Whoever bid first

Price-time priority

So, concretely, the book needs to answer three questions at every moment:

What's the best bid and best ask right now? (top of book)
If a new order arrives, does it cross the book — and if so, against whom, in what order, at what price(s)? (matching)
If someone wants to walk away before being filled, how do we find and remove their specific order out of a queue that might have hundreds of others in it? (cancel)

**Lines of code — rough, honest estimate, because LOC is a bad proxy for difficulty but useful for scoping your time:**

| Scope | Approx. LOC | What's in it |
|---|---|---|
| Bare minimum functional book | 200–400 | One side's book as a `std::map<Price, Queue>`, add/cancel/match, top-of-book query. No optimization, one order type (limit). |
| Optimized single-symbol engine | 600–1000 | Array/vector-indexed price levels on a bounded tick grid, intrusive doubly-linked list per level, order-ID → node hashmap, proper O(1) cancel |
| + multiple order types | +300–600 | Market, IOC, FOK, stop, stop-limit, amend-in-place semantics |
| + testing & benchmarking harness | +300–500 | Unit tests (correctness of matching/priority), synthetic order flow generator, latency/throughput measurement |
| + event log / replay / basic "distribution" | +300–500 | Serialized event log, a replica that reconstructs state from it |

So a genuinely solid version of what you scoped — multi-order-type, benchmarked, with a replay-based redundancy story — lands somewhere around **2,000–3,000 lines** of your own C++ before you add the visual interface or the ML layer. That's very achievable over a few months of serious part-time work; it's not a small project, but it's not a research-lab-scale one either. The matching-engine core itself (the hard, interesting part) is usually under 1,000 lines — the rest is testing, tooling, and polish.

**Terms and topics to go look into**, grouped so you know which bucket each one belongs to:

*Core data structures & algorithms*
- Price-time priority (and the alternative, pro-rata matching)
- Intrusive linked lists (vs. `std::list` — why the intrusive version avoids extra allocation)
- Hash map from order ID to a list node/iterator, for O(1) lookup
- Sparse/bounded array indexing by price ("price as array index" vs. `std::map`/red-black tree, and why bounded discrete ticks make this possible)
- FIFO matching algorithm

*Order types & book mechanics*
- Market order, Limit order, IOC (Immediate-or-Cancel), FOK (Fill-or-Kill), GTC (Good-Till-Cancelled)
- Stop order, Stop-limit order
- Iceberg / hidden order (partially visible size)
- Order amendment rules — when an amend preserves queue priority vs. loses it
- Self-trade prevention

*Performance / systems engineering*
- Mechanical sympathy, cache locality, false sharing, cache-line padding
- Memory pools / slab allocators / object pooling (avoiding `new`/`delete` in the hot path)
- Single-writer principle, lock-free ring buffers
- LMAX Disruptor architecture (the thing behind the "distributed the production way" question from earlier)
- Event sourcing and deterministic replay

*Market microstructure (the "why," not the "how")*
- Bid-ask spread, top of book vs. depth of book
- Order book imbalance, microprice
- Tick size, market impact, slippage

*Real-world protocols worth skimming, for realism*
- NASDAQ ITCH/OUCH message formats — actual exchange feed/order protocols, good for grounding your book in something real
- FIX protocol, at least at a conceptual level

*Testing/tooling*
- Latency percentiles (p50/p99/p99.9) as the right way to report performance, not averages
- Google Benchmark / GoogleTest, for structuring correctness and performance tests

Want to start actually drafting the core `OrderBook`/`PriceLevel`/order-ID-map structs in C++ now that the concept and scope are clear, or spend more time on the matching algorithm's exact logic (walking the opposite side, partial fills, what happens when an order exhausts multiple price levels) first?
