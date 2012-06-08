#ifndef _ASSEMBLER_H_
#define _ASSEMBLER_H_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

#define GPRDEF(_) \
  _(EAX) _(ECX) _(EDX) _(EBX) _(ESP) _(EBP) _(ESI) _(EDI) \
  _(R8D) _(R9D) _(R10D) _(R11D) _(R12D) _(R13D) _(R14D) _(R15D)
#define FPRDEF(_) \
  _(XMM0) _(XMM1) _(XMM2) _(XMM3) _(XMM4) _(XMM5) _(XMM6) _(XMM7) \
  _(XMM8) _(XMM9) _(XMM10) _(XMM11) _(XMM12) _(XMM13) _(XMM14) _(XMM15)

#define RIDENUM(name)	RID_##name,

enum {
  GPRDEF(RIDENUM)		/* General-purpose registers (GPRs). */
  FPRDEF(RIDENUM)		/* Floating-point registers (FPRs). */
  RID_MAX,
  RID_MRM = RID_MAX,		/* Pseudo-id for ModRM operand. */

  /* Fixed registers */
  RID_BASE = RID_EBP,
  RID_HP   = RID_R12D,

  /* Calling conventions. */
  RID_RET = RID_EAX,
  RID_FPRET = RID_XMM0,

  /* Register ranges [min, max) and number of registers. */
  RID_MIN_GPR = RID_EAX,
  RID_MIN_FPR = RID_XMM0,
  RID_MAX_GPR = RID_MIN_FPR,
  RID_MAX_FPR = RID_MAX,
  RID_NUM_GPR = RID_MAX_GPR - RID_MIN_GPR,
  RID_NUM_FPR = RID_MAX_FPR - RID_MIN_FPR,
};


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
  inline RegSet include(Reg r) { return RegSet(data_ | (kOne << r)); }
  inline Reg pickTop() const { return (Reg)lc_fls(data_); }
  inline Reg pickBot() const { return (Reg)lc_ffs(data_); }
private:
  explicit RegSet(uint32_t raw) : data_(raw) {}
  static const uint32_t kOne = 1;
  uint32_t data_;
};

// The general purpose registers
static const RegSet kGPR =
  RegSet::range(RID_MIN_GPR, RID_MAX_GPR).exclude(RID_ESP)
  .exclude(RID_BASE).exclude(RID_HP);

LC_STATIC_ASSERT(sizeof(RegSet) == sizeof(uint32_t));

_END_LAMBDACHINE_NAMESPACE

#endif /* _ASSEMBLER_H_ */
