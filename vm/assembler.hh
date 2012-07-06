#ifndef _ASSEMBLER_H_
#define _ASSEMBLER_H_

#include "common.hh"
#include "jit.hh"


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

#define RID_NONE		0x80
#define RID_MASK		0x7f
#define RID_INIT		(RID_NONE|RID_MASK)

inline bool isNoReg(Reg r) { return r & RID_NONE; }
inline bool isReg(Reg r) { return !(r & RID_NONE); }

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


/* Macros to construct variable-length x86 opcodes. -(len+1) is in LSB. */
#define XO_(o)		((uint32_t)(0x0000fe + (0x##o<<24)))
#define XO_FPU(a,b)	((uint32_t)(0x00fd + (0x##a<<16)+(0x##b<<24)))
#define XO_0f(o)	((uint32_t)(0x0f00fd + (0x##o<<24)))
#define XO_66(o)	((uint32_t)(0x6600fd + (0x##o<<24)))
#define XO_660f(o)	((uint32_t)(0x0f66fc + (0x##o<<24)))
#define XO_f20f(o)	((uint32_t)(0x0ff2fc + (0x##o<<24)))
#define XO_f30f(o)	((uint32_t)(0x0ff3fc + (0x##o<<24)))

/* This list of x86 opcodes is not intended to be complete. Opcodes are only
** included when needed. Take a look at DynASM or jit.dis_x86 to see the
** whole mess.
*/
typedef enum {
  /* Fixed length opcodes. XI_* prefix. */
  XI_NOP =	0x90,
  XI_CALL =	0xe8,
  XI_JMP =	0xe9,
  XI_JMPs =	0xeb,
  XI_PUSH =	0x50, /* Really 50+r. */
  XI_JCCs =	0x70, /* Really 7x. */
  XI_JCCn =	0x80, /* Really 0f8x. */
  XI_LEA =	0x8d,
  XI_MOVrib =	0xb0, /* Really b0+r. */
  XI_MOVri =	0xb8, /* Really b8+r. */
  XI_ARITHib =	0x80,
  XI_ARITHi =	0x81,
  XI_ARITHi8 =	0x83,
  XI_PUSHi8 =	0x6a,
  XI_TEST =	0x85,
  XI_RET =      0xc3,
  XI_MOVmi =	0xc7,
  XI_GROUP5 =	0xff,

  /* Note: little-endian byte-order! */
  XI_FLDZ =	0xeed9,
  XI_FLD1 =	0xe8d9,
  XI_FLDLG2 =	0xecd9,
  XI_FLDLN2 =	0xedd9,
  XI_FDUP =	0xc0d9,  /* Really fld st0. */
  XI_FPOP =	0xd8dd,  /* Really fstp st0. */
  XI_FPOP1 =	0xd9dd,  /* Really fstp st1. */
  XI_FRNDINT =	0xfcd9,
  XI_FSIN =	0xfed9,
  XI_FCOS =	0xffd9,
  XI_FPTAN =	0xf2d9,
  XI_FPATAN =	0xf3d9,
  XI_FSCALE =	0xfdd9,
  XI_FYL2X =	0xf1d9,

  /* Variable-length opcodes. XO_* prefix. */
  XO_MOV =	XO_(8b),
  XO_MOVto =	XO_(89),
  XO_MOVtow =	XO_66(89),
  XO_MOVtob =	XO_(88),
  XO_MOVmi =	XO_(c7),
  XO_MOVmib =	XO_(c6),
  XO_LEA =	XO_(8d),
  XO_ARITHib =	XO_(80),
  XO_ARITHi =	XO_(81),
  XO_ARITHi8 =	XO_(83),
  XO_ARITHiw8 =	XO_66(83),
  XO_SHIFTi =	XO_(c1),
  XO_SHIFT1 =	XO_(d1),
  XO_SHIFTcl =	XO_(d3),
  XO_IMUL =	XO_0f(af),
  XO_IMULi =	XO_(69),
  XO_IMULi8 =	XO_(6b),
  XO_CMP =	XO_(3b),
  XO_TEST =	XO_(85),
  XO_GROUP3b =	XO_(f6),
  XO_GROUP3 =	XO_(f7),
  XO_GROUP5b =	XO_(fe),
  XO_GROUP5 =	XO_(ff),
  XO_MOVZXb =	XO_0f(b6),
  XO_MOVZXw =	XO_0f(b7),
  XO_MOVSXb =	XO_0f(be),
  XO_MOVSXw =	XO_0f(bf),
  XO_MOVSXd =	XO_(63),
  XO_BSWAP =	XO_0f(c8),
  XO_CMOV =	XO_0f(40),

  XO_MOVSD =	XO_f20f(10),
  XO_MOVSDto =	XO_f20f(11),
  XO_MOVSS =	XO_f30f(10),
  XO_MOVSSto =	XO_f30f(11),
  XO_MOVLPD =	XO_660f(12),
  XO_MOVAPS =	XO_0f(28),
  XO_XORPS =	XO_0f(57),
  XO_ANDPS =	XO_0f(54),
  XO_ADDSD =	XO_f20f(58),
  XO_SUBSD =	XO_f20f(5c),
  XO_MULSD =	XO_f20f(59),
  XO_DIVSD =	XO_f20f(5e),
  XO_SQRTSD =	XO_f20f(51),
  XO_MINSD =	XO_f20f(5d),
  XO_MAXSD =	XO_f20f(5f),
  XO_ROUNDSD =	0x0b3a0ffc,  /* Really 66 0f 3a 0b. See asm_fpmath. */
  XO_UCOMISD =	XO_660f(2e),
  XO_CVTSI2SD =	XO_f20f(2a),
  XO_CVTSD2SI =	XO_f20f(2d),
  XO_CVTTSD2SI=	XO_f20f(2c),
  XO_CVTSI2SS =	XO_f30f(2a),
  XO_CVTSS2SI =	XO_f30f(2d),
  XO_CVTTSS2SI=	XO_f30f(2c),
  XO_CVTSS2SD =	XO_f30f(5a),
  XO_CVTSD2SS =	XO_f20f(5a),
  XO_ADDSS =	XO_f30f(58),
  XO_MOVD =	XO_660f(6e),
  XO_MOVDto =	XO_660f(7e),

  XO_FLDd =	XO_(d9), XOg_FLDd = 0,
  XO_FLDq =	XO_(dd), XOg_FLDq = 0,
  XO_FILDd =	XO_(db), XOg_FILDd = 0,
  XO_FILDq =	XO_(df), XOg_FILDq = 5,
  XO_FSTPd =	XO_(d9), XOg_FSTPd = 3,
  XO_FSTPq =	XO_(dd), XOg_FSTPq = 3,
  XO_FISTPq =	XO_(df), XOg_FISTPq = 7,
  XO_FISTTPq =	XO_(dd), XOg_FISTTPq = 1,
  XO_FADDq =	XO_(dc), XOg_FADDq = 0,
  XO_FLDCW =	XO_(d9), XOg_FLDCW = 5,
  XO_FNSTCW =	XO_(d9), XOg_FNSTCW = 7
} x86Op;

/* x86 opcode groups. */
typedef uint32_t x86Group;

typedef enum {
  XM_OFS0 = 0x00, XM_OFS8 = 0x40, XM_OFS32 = 0x80, XM_REG = 0xc0,
  XM_SCALE1 = 0x00, XM_SCALE2 = 0x40, XM_SCALE4 = 0x80, XM_SCALE8 = 0xc0,
  XM_MASK = 0xc0
} x86Mode;

#define MODRM(mode, r1, r2)	((MCode)((mode)+(((r1)&7)<<3)+((r2)&7)))

#define FORCE_REX		0x200
#define REX_64			(FORCE_REX|0x080000)

class Assembler {
public:
  Assembler(Jit *);
  ~Assembler();

  void move(Reg dst, Reg src);
  void loadi_u32(Reg dst, uint32_t i);
  void loadi_i32(Reg dst, int32_t i);
  void loadi_u64(Reg dst, uint64_t i);
  void ret();

  // mov dst, qword ptr [base + offset]
  void load_u64(Reg dst, Reg base, int32_t offset);

  // mov qword ptr [base + offset], src
  void store_u64(Reg src, Reg base, int32_t offset);

  // mov qword ptr [base + offset], imm32
  void storei_u64(Reg base, int32_t offset, int32_t i);

  MCode *finish();

private:
  inline Jit *jit() { return jit_; }

  inline void emit_i8(uint8_t i) { *--mcp = (MCode)i; }
  inline void emit_i32(int32_t i) { *(int32_t *)(mcp - 4) = i; mcp -= 4; }
  inline void emit_u32(uint32_t i) { *(uint32_t *)(mcp - 4) = i; mcp -= 4; }
  // Emit REX prefix if necessary.
  inline void emit_rex(MCode *&p, Reg rr, Reg rb) {
    MCode rex = 0x40 + ((rr >> 1) & 4) + ((rb >> 3) & 1);
    if (rex != 0x40) *--p = rex;
  }

  // Generic emitting code for *multi-byte* instructions.
  MCode *emit_op(x86Op xo, Reg rr, Reg rb, Reg rx, MCode *p, int delta);

  // op r, [base + offset]
  void emit_rmro(x86Op xo, Reg rr, Reg rb, int32_t offset);

  // Opcode + ModRM encoding
  inline MCode *emit_opm(x86Op xo, x86Mode mode, Reg rr, Reg rb, MCode *p, int delta) {
    p[delta - 1] = MODRM(mode, rr, rb);
    return emit_op(xo, rr, rb, 0, p, delta);
  }

  // Opcode + ModRM + SIB encoding
  inline MCode *emit_opmx(x86Op xo, x86Mode mode, x86Mode scale,
                          Reg rr, Reg rb, Reg rx, MCode *p) {
    p[-1] = MODRM(scale, rx, rb);
    p[-2] = MODRM(mode, rr, RID_ESP);
    return emit_op(xo, rr, rb, rx, p, 0);
  }

  inline void emit_rr(x86Op xo, Reg r1, Reg r2) {
    MCode *p = mcp;
    mcp = emit_opm(xo, XM_REG, r1, r2, p, 0);
  }

  MCode *mcend;  // End of generated machine code.
  MCode *mcp;   // Current MCode pointer (grows down).
  MCode *mclim; // Lower limit for MCode memory + red zone

  MCode *mcbot; // Bottom of reserved MCode
  MCode *mctop; // Top of generated MCode

  Jit *jit_;

};


inline MCode *Assembler::emit_op(x86Op xo, Reg rr, Reg rb, Reg rx,
                                 MCode *p, int delta) {
  // The LSB of the opcode is -(len + 1)
  int n = (int8_t)xo;

#if defined(__GNUC__)
  if (__builtin_constant_p(xo) && n == -2)
    p[delta-2] = (MCode)(xo >> 24);
  else if (__builtin_constant_p(xo) && n == -3)
    *(uint16_t *)(p+delta-3) = (uint16_t)(xo >> 16);
  else
#endif
    *(uint32_t *)(p+delta-5) = (uint32_t)xo;
  p += n + delta;
  {
    uint32_t rex = 0x40 + ((rr>>1)&(4+(FORCE_REX>>1)))+((rx>>2)&2)+((rb>>3)&1);
    if (rex != 0x40) {
      rex |= (rr >> 16);
      if (n == -4) { *p = (MCode)rex; rex = (MCode)(xo >> 8); }
      else if ((xo & 0xffffff) == 0x6600fd) { *p = (MCode)rex; rex = 0x66; }
      *--p = (MCode)rex;
    }
  }
  return p;
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _ASSEMBLER_H_ */
