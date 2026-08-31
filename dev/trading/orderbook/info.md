# Order Book — Project Notes

## Goal

Build an order book as a high-leverage pet project for quant dev (career target: quant dev; prep for Optiver HackaTUM / Optibook). Canonical data structure in market microstructure — every matching engine, exchange simulator, and HFT system has one at its core.

## Why it's a strong project

- Signals domain understanding, not just coding ability.
- Forces real engineering decisions with actual trade-offs (see below).
- Great vehicle for C++ fluency quant dev roles expect: intrusive data structures, custom allocators/memory pools, lock-free / single-threaded-per-core designs, profiling with `perf`.
- Scales naturally in difficulty: correctness → orders/sec → p99 latency — exactly the axis quant infra roles care about.

## Core challenges (where the real difficulty is)

Matching logic itself is straightforward; the interesting problems are:

1. **Data structure choice** — a naive `std::map<Price, std::deque<Order>>` works but isn't fast. Better: fixed-size array or hash map for price levels (tick sizes are discrete), with intrusive linked lists for FIFO order queues per level → O(1) insert / cancel / best-bid-ask.
2. **Memory management** — allocating/deallocating per event kills throughput; use object pools.
3. **Benchmarking properly** — realistic synthetic order flow, measure throughput and latency percentiles, not "it compiles and looks right."

## Design decisions to make

- Price-time priority vs. pro-rata matching.
- Price level representation: array-indexed by tick vs. `std::map`/red-black tree vs. custom flat structure.
- O(1) order cancellation instead of O(n).
- Memory layout for cache locality.

## Scope to go beyond a toy

- [ ] Feed handler: replay historical or synthetic tick data.
- [ ] Metrics: orders/sec, latency percentiles.
- [ ] (Optional) Minimal matching-engine + strategy loop — doubles as Optibook prep; the mental model (limit orders, best bid/ask, market impact) transfers directly.

## Caution

Don't let "orderbook" become "hash map wrapped in a class." The project is only as valuable as the performance engineering behind it:

- Build it → benchmark it → explain *why* one design beats another, with numbers → strong quant dev portfolio piece.
- Just functionally correct and stop → decent but unremarkable exercise.

## Next step

Sketch the design before coding: data structures, API surface, what to benchmark.



https://rustquant.dev/blog/limit-order-book/
https://gist.github.com/halfelf/db1ae032dc34278968f8bf31ee999a25
