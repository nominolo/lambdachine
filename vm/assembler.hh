#ifndef _ASSEMBLER_H_
#define _ASSEMBLER_H_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

class RegCost {
public:
  inline RegCost(uint16_t cost, uint16_t ref) {
    cost_ = ((uint32_t)cost << 16) + (uint32_t)ref;
  }
  ~RegCost() {}

  inline uint16_t ref() const { return cost_; }
  inline uint16_t cost() const { return cost_; }
private:
  uint32_t cost_;
};

LC_STATIC_ASSERT(sizeof(RegCost) == sizeof(uint32_t));

typedef uint32_t Reg;

class RegSet {
public:
  RegSet() : data_(0) {}
  static inline RegSet fromReg(Reg r) { return RegSet(kOne << r); }
  // Returns a RegSet with for registers in the range [lo,hi); i.e.,
  // 'hi' is *not* included.
  static inline RegSet range(Reg lo, Reg hi) {
    uint32_t ones = (1 << (hi - lo)) - 1;
    return RegSet(ones << lo);
  }
  inline bool test(Reg r) const { return (data_ >> r) & 1; }
  inline void set(Reg r) { data_ |= kOne << r; }
  inline void clear(Reg r) { data_ &= ~(kOne << r); }
  inline RegSet exclude(Reg r) { return RegSet(data_ & ~(kOne << r)); }
  inline Reg pickTop() const { return (Reg)lc_fls(data_); }
  inline Reg pickBot() const { return (Reg)lc_ffs(data_); }
private:
  explicit RegSet(uint32_t raw) : data_(raw) {}
  static const uint32_t kOne = 1;
  uint32_t data_;
};

LC_STATIC_ASSERT(sizeof(RegSet) == sizeof(uint32_t));

_END_LAMBDACHINE_NAMESPACE

#endif /* _ASSEMBLER_H_ */
