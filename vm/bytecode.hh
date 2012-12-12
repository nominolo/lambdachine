#ifndef _BYTECODE_H_
#define _BYTECODE_H_

#include "common.hh"
#include <iostream>

_START_LAMBDACHINE_NAMESPACE

#define BCDEF(_) \
  /* Comparison ops. Order significant. */ \
  _(ISLT,    RRJ) \
  _(ISGE,    RRJ) \
  _(ISLE,    RRJ) \
  _(ISGT,    RRJ) \
  _(ISEQ,    RRJ) \
  _(ISNE,    RRJ) \
  /* Unsigned comparisons, order significant */ \
  _(ISLTU,   RRJ) \
  _(ISGEU,   RRJ) \
  _(ISLEU,   RRJ) \
  _(ISGTU,   RRJ) \
  /* Unary ops */ \
  _(NEG,     RR) \
  /* Updates */ \
  _(MOV,     RR) \
  _(MOV_RES, RN) \
  _(UPDATE,  RR) \
  _(LOADF,   RRN) \
  _(LOADFV,  RN) \
  _(LOADBH,  R) \
  _(LOADSLF, R) \
  _(INITF,   RRN) /* Write to field (for initialisation) */   \
  /* Binary ops. */ \
  _(ADDRR,   RRR) \
  _(SUBRR,   RRR) \
  _(MULRR,   RRR) \
  _(DIVRR,   RRR) \
  _(REMRR,   RRR) \
  /* Bitwise ops */ \
  _(BNOT,    RR) \
  _(BAND,    RRR) \
  _(BOR,     RRR) \
  _(BXOR,    RRR) \
  _(BSHL,    RRR) \
  _(BSHR,    RRR) \
  _(BSAR,    RRR) \
  _(BROL,    RRR) \
  _(BROR,    RRR) \
  /* Primops */ \
  _(PTROFSC, RRR) /* indexCharOffAddr# :: Addr# -> Int# -> Char# */ \
  _(GETTAG,  RR) /* dataToTag# :: a -> Int# */ \
  /* Constants */ \
  _(LOADK,   RN) \
  _(KINT,    RS) \
  _(NEW_INT, RS) \
  /* Allocation */ \
  _(ALLOC1,  ___) \
  _(ALLOC,   ___) \
  _(ALLOCAP, ___) \
  /* Calls and jumps */ \
  _(CALL,    ___) \
  _(CALLT,   ___) \
  _(RET1,    R) \
  _(RETN,    R) \
  _(JMP,     J) \
  _(EVAL,    ___) \
  _(CASE,    ___) \
  _(CASE_S,  ___) \
  /* Function headers */ \
  _(FUNC,    R) \
  _(IFUNC,   R) \
  _(JFUNC,   RN) \
  _(FUNCPAP, ___) \
  _(JRET,    RN) \
  _(IRET,    RN) \
  _(SYNC, ___) \
  _(STOP, ___)

/**
 * A bytecode instruction.
 *
 * Bytecode instructions are usually 4 bytes and must be aligned at a
 * 4 byte boundary.  Some instructions need more than 4 bytes and in
 * this case are encoded using multiple 4-byte chunks.
 *
 * Instructions are of the following formats optionally followed by
 * additional payload chunks.
 *
 *     MSB                   LSB
 *     +-----+-----+-----+-----+
 *     |  B  |  C  |  A  | OPC |  ABC format
 *     +-----+-----+-----+-----+
 *     |     D     |  A  | OPC |  AD format
 *     +-----------+-----+-----+
 *
 * OPC, A, B and C are 8 bit fields.  D is 16 bits wide and overlaps B
 * and C.  We write SD when treating D as a signed field.
 *
 * The payload format depends on the instruction opcode.
 */
class BcIns {
 public:

  BcIns() : raw_(kSTOP) {}
  
  static const int32_t kBranchBias = 0x8000;
  static const u4 kMaxCallArgs = 8;

  typedef enum {
#define DEF_BCINS_OPCODE(ins,format) k##ins,
    BCDEF(DEF_BCINS_OPCODE)
#undef DEF_BCINS_OPCODE
  } Opcode;

  typedef enum {
    IFM_J,
    IFM_R,
    IFM_RR,
    IFM_RRR,
    IFM_RN,
    IFM_RRN,
    IFM_RS,
    IFM_RRJ,
    IFM____
  } InsFormat;

  /**
   * Encodes an instruction in the ABC format.
   */
  static inline BcIns abc(Opcode opcode, uint8_t a, uint8_t b, uint8_t c) {
    return BcIns(static_cast<uint32_t>(opcode) |
                 (static_cast<uint32_t>(a) << 8) |
                 (static_cast<uint32_t>(b) << 24) |
                 (static_cast<uint32_t>(c) << 16));
  }

  static inline BcIns ad(Opcode opcode, uint8_t a, uint16_t d) {
    return BcIns(static_cast<uint32_t>(opcode) |
                 (static_cast<uint32_t>(a) << 8) |
                 (static_cast<uint32_t>(d) << 16));
  }

  static inline BcIns asd(Opcode opcode, uint8_t a, int16_t sd) {
    return BcIns(static_cast<uint32_t>(opcode) |
                 (static_cast<uint32_t>(a) << 8) |
                 (static_cast<uint32_t>(sd) << 16));
  }

  /**
   * Encode a branch instruction.  Branch offsets are always relative
   * to the instruction following the branch instruction itself.
   * I.e., an offset of 0 is a no-op. An offset is in instruction
   * counts, i.e., an offset of 1 means, skip exactly one
   * instruction.
   */
  static inline BcIns aj(Opcode opcode, uint8_t a, int16_t offset) {
    uint16_t d = static_cast<uint16_t>(kBranchBias + (int32_t)offset);
    return ad(opcode, a, d);
  }

  static inline BcIns bitmapOffset(int32_t n) {
    return BcIns(static_cast<uint32_t>(n));
  }

  static inline BcIns args(uint8_t arg1, uint8_t arg2, uint8_t arg3,
                           uint8_t arg4) {
    LC_ASSERT(arch::kEndianness == arch::ENDIAN_LITTLE);
    return BcIns(static_cast<uint32_t>(arg1) |
                 (static_cast<uint32_t>(arg2) << 8) |
                 (static_cast<uint32_t>(arg3) << 16) |
                 (static_cast<uint32_t>(arg4) << 24));
  }

  inline uint32_t raw() const { return raw_; }
  inline uint8_t a() const { return (raw_ >> 8) & 0xff; }
  inline uint8_t b() const { return (raw_ >> 24); }
  inline uint8_t c() const { return (raw_ >> 16) & 0xff; }
  inline uint16_t d() const { return (raw_ >> 16); }
  inline int16_t sd() const { return static_cast<int16_t>(raw_ >> 16); }
  inline int16_t j() const {
    return static_cast<int16_t>
      (static_cast<int32_t>(raw_ >> 16) - kBranchBias); 
  }
  const char *name() const;
  InsFormat format() const;

  inline Opcode opcode() const {
    return static_cast<Opcode> (raw_ & 0xff);
  }

  static inline const u2 *offsetToBitmask(const BcIns *pc) {
    uint32_t offset = pc->raw_;
    if (offset == 0)
      return NULL;
    else
      return (const u2*)((u1*)pc + offset);
  }

  static const BcIns *debugPrint(
    std::ostream& out, const BcIns *ins,
    bool oneline, const BcIns *baseaddr, const struct _Code*);

  static inline const BcIns *debugPrint(std::ostream& out,
                                        const BcIns *ins) {
    return debugPrint(out, ins, false, NULL, NULL);
  }

 private:
  BcIns(uint32_t raw) : raw_(raw) {}

  uint32_t raw_;

  friend class Loader;
};

#define BC_ROUND(bytes) \
  (((bytes) + (sizeof(BcIns) - 1)) / sizeof(BcIns))

_END_LAMBDACHINE_NAMESPACE

#endif /* _BYTECODE_H_ */
