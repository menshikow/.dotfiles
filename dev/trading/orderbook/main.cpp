#include <cstdint>
#include <expected>

std::expected<Trade, OrderError> matchOrder(const Order &incoming);
std::expected<void, OrderError> cancelOrder(OrderId id);

enum class OrderError {
  NotFound,
  AlreadyFilled,
  InvalidQuantity,
  InvalidPrice
};

enum class OrderType : uint8_t {
  Limit,
  Market,
  IOC,
  FOK,
  Stop,
  Stop_Limit,
};
enum class Side : uint8_t { Buy, Sell };

struct OrderId {};
struct Quantity {};
struct Timestamp {};

struct Price {
  uint64_t
      ticks; // e.g. price in cents or in units of the instrument's tick size
  explicit Price(int64_t t) : ticks(t) {}
  auto operator<=>(const Price &) const = default;
};

struct Trade {
  const OrderId restingOrderId;
  const OrderId incomingOrderId;
  const Price price;
  const Quantity quantity;
  const Timestamp timestamp;
};

struct Order {
  OrderId id;
  Side side;
  OrderType type;
  Price price;
  Quantity quantity;
  Quantity remaining_quantity;
  Timestamp timestamp;
};

int main() { return 0; }
