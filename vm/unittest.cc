#include "gtest/gtest.h"
#include "thread.hh"
#include "memorymanager.hh"
#include "loader.hh"
#include "assembler.hh"
#include "capability.hh"
#include "objects.hh"
#include "miscclosures.hh"
#include "jit.hh"

#include <iostream>
#include <sstream>
#include <fstream>

using namespace std;
using namespace lambdachine;

TEST(ThreadTest, StartStop) {
  MemoryManager m;
  Loader l(&m, NULL);
  Thread *T = Thread::createThread(NULL, 20);
  ASSERT_TRUE(T != NULL);
  ASSERT_TRUE(T->isValid());
  delete T;
}

TEST(MMTest, AllocRegion) {
  Region *region = Region::newRegion(Region::kSmallObjectRegion);
  ASSERT_TRUE(region != NULL);
  delete region;
}

TEST(MMTest, AllocBasic) {
  MemoryManager m;
  void *p = m.allocInfoTable(10);
  ASSERT_TRUE(p != NULL);
}

TEST(MMTest, AllocBasic2) {
  MemoryManager m;
  // Fill up at least one block.
  size_t i = 0;
  for ( ; i < (Block::kBlockSize / 10) + 10; i++) {
    void *p = m.allocInfoTable(10);
    ASSERT_TRUE(p != NULL);
  }
  //cout << i << "," << i * 10 * sizeof(Word) << endl << m;
  ASSERT_GT(m.infoTables(), sizeof(Word));
}

TEST(LoaderTest, Simple) {
  MemoryManager mm;
  Loader l(&mm, "/usr/bin");
  ASSERT_STREQ("/usr/bin", l.basePath(0));
}

TEST(LoaderTest, DefaultBasePath) {
  MemoryManager mm;
  Loader l(&mm, NULL);
  ASSERT_TRUE(l.basePath(0) != NULL);
}

TEST(LoaderTest, Load1) {
  MemoryManager mm;
  Loader l(&mm, "tests");
  ASSERT_TRUE(l.loadModule("GHC.Bool"));
  const Module *m = l.module("GHC.Bool");
  ASSERT_TRUE(m != NULL);
  ASSERT_STREQ("GHC.Bool", m->name());
}

TEST(LoaderTest, LoadIdempotent) {
  MemoryManager mm;
  Loader l(&mm, "tests");
  const char *modname = "GHC.Bool";
  ASSERT_TRUE(l.loadModule(modname));
  const Module *m = l.module(modname);

  // Try loading the same module again.  Make sure the requested
  // module name is not pointer identical.
  char *modname2 = new char[strlen(modname) + 1];
  strcpy(modname2, modname);
  ASSERT_TRUE(modname != modname2);

  ASSERT_TRUE(l.loadModule(modname2));
  const Module *m2 = l.module(modname2);

  ASSERT_TRUE(m == m2);         // pointer identity!
}

TEST(LoaderTest, DebugPrint) {
  MemoryManager mm;
  Loader l(&mm, "tests");
  ASSERT_TRUE(l.loadModule("GHC.Base"));
  // We don't really specify the debug output.  It shouldn't cause
  // crashes, though.
  l.printInfoTables(cerr);
  l.printClosures(cerr);
}

TEST(LoaderTest, BuiltinClosures) {
  MemoryManager mm;
  ASSERT_TRUE(NULL == MiscClosures::stg_STOP_closure_addr);
  ASSERT_TRUE(NULL == MiscClosures::stg_UPD_closure_addr);
  ASSERT_TRUE(NULL == MiscClosures::stg_UPD_return_pc);
  ASSERT_TRUE(NULL == MiscClosures::stg_IND_info);

  Loader l(&mm, "tests");
  ASSERT_TRUE(l.loadModule("GHC.Base"));
  ASSERT_TRUE(NULL != MiscClosures::stg_STOP_closure_addr);
  ASSERT_TRUE(NULL != MiscClosures::stg_UPD_closure_addr);
  ASSERT_TRUE(NULL != MiscClosures::stg_UPD_return_pc);
  ASSERT_TRUE(NULL != MiscClosures::stg_IND_info);
}

TEST(RegSetTest, fromReg) {
  RegSet rs = RegSet::fromReg(4);
  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ(i == 4, rs.test(i));
  }
}

TEST(RegSetTest, range) {
  RegSet rs = RegSet::range(4, 10);
  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ(4 <= i && i < 10, rs.test(i));
  }
}

TEST(RegSetTest, setClear) {
  RegSet rs = RegSet::range(4, 10);
  rs.set(12);
  ASSERT_TRUE(rs.test(12));
  rs.clear(12);
  ASSERT_FALSE(rs.test(12));
}

TEST(RegSetTest, excludeInclude) {
  RegSet rs = RegSet::range(4, 10).exclude(8);;
  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ(4 <= i && i < 10 && i != 8, rs.test(i));
  }
  rs = RegSet::range(4, 10).include(18);;
  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ((4 <= i && i < 10) || i == 18, rs.test(i));
  }
}

TEST(RegSetTest, pickBotTop) {
  RegSet rs = RegSet::range(4, 10);
  rs.set(15);
  ASSERT_EQ((Reg)4, rs.pickBot());
  ASSERT_EQ((Reg)15, rs.pickTop());
}

TEST(Flags, setVal) {
  Flags32 f;
  for (int i = 0; i < 32; ++i) {
    EXPECT_FALSE(f.get(i));
  }
  f.set(4);
  for (int i = 0; i < 32; ++i) {
    EXPECT_TRUE(i == 4 ? f.get(i) : !f.get(i));
  }

  f.set(4, false);
  EXPECT_FALSE(f.get(4));
  f.set(4, true);
  EXPECT_TRUE(f.get(4));
}

TEST(AllocMachineCode, Simple) {
  Prng prng;
  MachineCode mcode(&prng);
  MCode *from, *to;
  to = mcode.reserve(&from);
  EXPECT_TRUE(to != NULL && from != NULL);
  EXPECT_LT((void*)from, (void*)to);
  cerr << "asmExit @ " << (void*)&asmExit << "  code @ " << (void*)to << endl;
  ptrdiff_t dist = (char*)to - (char*)&asmExit;
  if (dist < 0) dist = -dist;
  EXPECT_TRUE(dist < (ptrdiff_t)1 << 31);
}

class AsmTest : public ::testing::Test {
protected:
  virtual void SetUp() {
    jit = new Jit();
    as = new Assembler(jit);
    as->setupMachineCode(jit->mcode());
  }

  virtual void TearDown() {
    if (as) delete as;
    if (jit) delete jit;
    as = NULL;
    jit = NULL;
  }

  // To debug a failing assembler test:
  //
  //  1. Add a call to Dump() after as->finish().
  //  2. Run the test (`make asmtest`)
  //  3. Run `./utils/showasmdump.sh dump_<testname>.s`
  //  4. Find and fix the bug.
  //
  void Dump() {
    const ::testing::TestInfo* const test_info =
      ::testing::UnitTest::GetInstance()->current_test_info();
    ofstream out;
    string filename("dump_");
    filename += test_info->name();
    filename += ".s";
    out.open(filename.c_str());
    jit->mcode()->dumpAsm(out);
    out.close();
  }

  AsmTest() : jit(NULL), as(NULL) {}

  virtual ~AsmTest() {
    if (as) delete as;
    if (jit) delete jit;
    as = NULL;
    jit = NULL;
  }

  Jit *jit;
  Assembler *as;
};

typedef Word (*anon_fn_1)(Word);

// Note: the assembler works backwards!

// TODO: Most of these tests will break on Windows, because it uses a
// different calling convention.

// TODO: These tests won't work for a cross-compiler.

TEST_F(AsmTest, Move) {
  // This is the identity function.
  as->ret();
  as->move(RID_EAX, RID_EDI);

  MCode *code = as->finish();
  EXPECT_EQ(1234, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, MoveHiReg) {
  // Tests moving to/from/among registers >= r8
  as->ret();
  as->move(RID_EAX, RID_R8D);
  as->move(RID_R8D, RID_R10D);
  as->move(RID_R10D, RID_EDI);

  MCode *code = as->finish();
  EXPECT_EQ(1234, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmU32Pos) {
  as->ret();
  as->loadi_u32(RID_EAX, 6789);

  MCode *code = as->finish();
  EXPECT_EQ(6789, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmU32Neg) {
  as->ret();
  as->loadi_u32(RID_EAX, -6789);

  MCode *code = as->finish();
  EXPECT_EQ((Word)(uint32_t)-6789, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmI32Pos) {
  as->ret();
  as->loadi_i32(RID_EAX, 6789);

  MCode *code = as->finish();
  EXPECT_EQ(6789, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmI32Neg) {
  as->ret();
  as->loadi_i32(RID_EAX, -6789);

  MCode *code = as->finish();
  EXPECT_EQ((Word)-6789, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmU64) {
  as->ret();
  as->loadi_u64(RID_EAX, 0x123456789abcdef0);

  MCode *code = as->finish();
  EXPECT_EQ((uint64_t)0x123456789abcdef0UL,
            cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadImmU64_I32Range) {
  as->ret();
  as->loadi_u64(RID_EAX, (uint64_t)-6789);

  MCode *code = as->finish();
  EXPECT_EQ((uint64_t)-6789, cast(anon_fn_1, code)(1234));
}

TEST_F(AsmTest, LoadMemU64) {
  as->ret();
  as->load_u64(RID_EAX, RID_EDI, 8);
  
  Word data[2] = { 0x1000000400010003UL, 0x8000000700080005UL };
  MCode *code = as->finish();

  EXPECT_EQ(0x8000000700080005UL, cast(anon_fn_1, code)((Word)data));
}

TEST_F(AsmTest, StoreMemU64) {
  as->ret();
  as->store_u64(RID_EDI, 8, RID_EDI);
  MCode *code = as->finish();

  Word data[2] = { 0, 0 };
  cast(anon_fn_1, code)((Word)data);

  EXPECT_EQ(0, data[0]);
  EXPECT_EQ((Word)data, data[1]);
}

TEST_F(AsmTest, StoreMemImmPos) {
  as->ret();
  as->storei_u64(RID_EDI, 8, 5);
  MCode *code = as->finish();

  Word data[2] = { 0, 0 };
  cast(anon_fn_1, code)((Word)data);

  EXPECT_EQ(0, data[0]);
  EXPECT_EQ((Word)5, data[1]);
}

TEST_F(AsmTest, StoreMemImmNeg) {
  as->ret();
  as->storei_u64(RID_EDI, 8, -5);
  MCode *code = as->finish();

  Word data[2] = { 0, 0 };
  cast(anon_fn_1, code)((Word)data);

  EXPECT_EQ(0, data[0]);
  EXPECT_EQ((Word)-5, data[1]);
}

class IRTest : public ::testing::Test {
protected:
  virtual void SetUp() {
    stack = new Word[256];
    buf = new IRBuffer();
    buf->reset(&stack[11], &stack[18]);
    buf->disableOptimisation(IRBuffer::kOptFold);
  }
  virtual void TearDown() {
    if (buf) delete buf;
    buf = NULL;
    if (stack) delete[] stack;
    stack = NULL;
  }
  IRTest() : buf(NULL), stack(NULL) {}
  virtual ~IRTest() {
    TearDown();
  }

  IRBuffer *buf;
  Word *stack;
};

TEST_F(IRTest, Simple) {
  EXPECT_EQ(1, buf->size());
  EXPECT_EQ(IR::kBASE, buf->ir(REF_BASE)->opcode());
}

TEST_F(IRTest, Slots) {
  TRef tr = buf->slot(0);
  IRRef ref = tr.ref();
  EXPECT_EQ(ref, (uint16_t)REF_FIRST);
  EXPECT_EQ(2, buf->size());
  EXPECT_EQ(IR::kSLOAD, buf->ir(ref)->opcode());
  EXPECT_EQ(0, buf->ir(ref)->op1());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Slots2) {
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->emit(IRT(IR::kADD, IRT_I64), tr1.ref(), tr1.ref());
  IRRef ref = tr2.ref();
  EXPECT_EQ(ref, (uint16_t)REF_FIRST + 1);
  EXPECT_EQ(3, buf->size());
  EXPECT_EQ(IR::kADD, buf->ir(ref)->opcode());
  EXPECT_EQ((uint16_t)tr1.ref(), buf->ir(ref)->op1());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals1) {
  TRef tr1 = buf->literal(IRT_I64, 1234);
  TRef tr2 = buf->literal(IRT_I64, 1234);
  EXPECT_EQ((uint16_t)REF_BASE - 1, tr1.ref());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ(tr1, tr2);
  EXPECT_EQ(2, buf->size());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals2) {
  TRef tr1 = buf->literal(IRT_I64, 1234);
  TRef tr2 = buf->literal(IRT_I32, 1234);
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  EXPECT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ((uint8_t)IRT_I32, tr2.t());
  EXPECT_NE(tr1.ref(), tr2.ref());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals3) {
  TRef tr1 = buf->literal(IRT_I64, 1234);
  TRef tr2 = buf->literal(IRT_I64, 12345);
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  EXPECT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ((uint8_t)IRT_I64, tr2.t());
  EXPECT_NE(tr1.ref(), tr2.ref());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals4) {
  TRef tr1 = buf->literal(IRT_I64, 5000000000);
  TRef tr2 = buf->literal(IRT_I64, 5000000000);
  EXPECT_EQ((uint16_t)REF_BASE - 1, tr1.ref());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ(tr1, tr2);
  EXPECT_EQ(3, buf->size());
  EXPECT_EQ(5000000000, buf->literalValue(tr1));
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals5) {
  TRef tr1 = buf->literal(IRT_I64, 5000000000);
  TRef tr2 = buf->literal(IRT_CLOS, 5000000000);
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  EXPECT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ((uint8_t)IRT_CLOS, tr2.t());
  EXPECT_NE(tr1.ref(), tr2.ref());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Literals6) {
  TRef tr1 = buf->literal(IRT_I64, 5000000000);
  TRef tr2 = buf->literal(IRT_I64, 5000000001);
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  EXPECT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ((uint8_t)IRT_I64, tr1.t());
  EXPECT_EQ((uint8_t)IRT_I64, tr2.t());
  EXPECT_NE(tr1.ref(), tr2.ref());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, BaseLiterals) {
  TRef tr1 = buf->baseLiteral(&stack[15]);
  TRef tr2 = buf->baseLiteral(&stack[3]);
  TRef tr3 = buf->baseLiteral(&stack[15]);
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  EXPECT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ(tr1, tr3);
  EXPECT_NE(tr1.ref(), tr2.ref());
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTest, Snapshot1) {
  Snapshot snap[4];
  SnapshotData *snapmap = buf->snapmap();

  TRef tr1 = buf->slot(0);
  snap[0] = buf->snap(buf->snapshot(NULL));
  EXPECT_EQ(0, snap[0].entries());
  EXPECT_EQ(tr1.ref() + 1, snap[0].ref());
  EXPECT_EQ(0, snap[0].relbase());

  TRef tr2 = buf->emit(IR::kADD, IRT_I64, tr1, tr1);
  buf->setSlot(0, tr2);
  snap[1] = buf->snap(buf->snapshot(NULL));
  EXPECT_EQ(1, snap[1].entries());
  EXPECT_EQ(tr2.ref() + 1, snap[1].ref());
  EXPECT_EQ(0, snap[1].relbase());
  EXPECT_EQ(tr2.ref(), snap[1].slot(0, snapmap));

  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  buf->setSlot(1, tr3);
  TRef tr4 = buf->slot(2);
  buf->setSlot(3, tr3);
  buf->setSlot(4, tr1);
  snap[2] = buf->snap(buf->snapshot(NULL));
  EXPECT_EQ(4, snap[2].entries());
  EXPECT_EQ(tr4.ref() + 1 - REF_BASE, snap[2].ref() - REF_BASE);
  EXPECT_EQ(0, snap[2].relbase());
  EXPECT_EQ(tr2.ref(), snap[2].slot(0, snapmap));
  EXPECT_EQ(tr3.ref(), snap[2].slot(1, snapmap));
  EXPECT_EQ(0,         snap[2].slot(2, snapmap));
  EXPECT_EQ(tr3.ref(), snap[2].slot(3, snapmap));
  EXPECT_EQ(tr1.ref(), snap[2].slot(4, snapmap));

  buf->setSlot(1, TRef());
  snap[3] = buf->snap(buf->snapshot(NULL));
  EXPECT_EQ(3, snap[3].entries());

  buf->debugPrint(cerr, 1);
  snap[0].debugPrint(cerr, snapmap, 0);
  snap[1].debugPrint(cerr, snapmap, 1);
  snap[2].debugPrint(cerr, snapmap, 2);
  snap[3].debugPrint(cerr, snapmap, 3);

  //  TRef tr2 = buf->emit(IR::kADD, IRT_I64, tr1, tr1);
}

class IRTestFold : public IRTest {
protected:
  virtual void SetUp() {
    IRTest::SetUp();
    buf->enableOptimisation(IRBuffer::kOptFold);
  }
};

TEST_F(IRTestFold, FoldAdd) {
  TRef tr1 = buf->literal(IRT_I64, -1234);
  TRef tr2 = buf->emit(IRT(IR::kADD, IRT_I64), tr1.ref(), tr1.ref());
  EXPECT_TRUE(!tr1.isNone() && tr1.isLiteral());
  ASSERT_TRUE(!tr2.isNone() && tr2.isLiteral());
  EXPECT_EQ((uint64_t)-2468, buf->literalValue(tr2.ref()));
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTestFold, FoldComm) {
  TRef tr1 = buf->literal(IRT_I64, 1234);
  TRef tr2 = buf->slot(0);
  TRef tr3 = buf->emit(IRT(IR::kADD, IRT_I64), tr1, tr2);
  TRef tr4 = buf->emit(IRT(IR::kADD, IRT_I64), tr1, tr3);
  
  // Constant should be moved to the right.
  EXPECT_EQ(tr1.ref(), buf->ir(tr3.ref())->op2());
  EXPECT_EQ(tr2.ref(), buf->ir(tr3.ref())->op1());
  EXPECT_EQ(tr2.ref(), buf->ir(tr4.ref())->op1());
  IRRef ref = buf->ir(tr4.ref())->op2();
  ASSERT_TRUE(irref_islit(ref));
  EXPECT_EQ((int)2468, buf->literalValue(ref));
  buf->debugPrint(cerr, 1);
}

TEST_F(IRTestFold, FoldSub) {
  TRef zero = buf->literal(IRT_I64, 0);
  TRef lit1 = buf->literal(IRT_I64, 1234);
  TRef lit2 = buf->literal(IRT_I64, -345);
  TRef opaque = buf->slot(0);
  TRef opaque2 = buf->slot(1);
  
  TRef tr1 = buf->emit(IR::kSUB, IRT_I64, lit1, lit2);
  ASSERT_TRUE(tr1.isLiteral());
  EXPECT_EQ((uint64_t)1579, buf->literalValue(tr1.ref()));

  TRef tr2 = buf->emit(IR::kSUB, IRT_I64, opaque, zero);
  EXPECT_EQ(tr2, opaque);

  TRef tr3 = buf->emit(IR::kSUB, IRT_I64, opaque, lit1);
  IR *tir = buf->ir(tr3);
  EXPECT_EQ(IR::kADD, tir->opcode());
  EXPECT_EQ((uint64_t)-1234, buf->literalValue(tir->op2()));

  TRef tr4 = buf->emit(IR::kSUB, IRT_I64, zero, opaque);
  IR *tir2 = buf->ir(tr4);
  EXPECT_EQ(IR::kNEG, tir2->opcode());
  EXPECT_EQ((IRRef1)opaque, tir2->op1());

  TRef tr5 = buf->emit(IR::kSUB, IRT_I64, opaque, opaque2);
  TRef tr6 = buf->emit(IR::kSUB, IRT_I64, tr5, opaque);
  IR *tir3 = buf->ir(tr6);
  EXPECT_EQ(IR::kNEG, tir3->opcode());
  EXPECT_EQ((IRRef1)opaque2, tir3->op1());

  TRef tr7 = buf->emit(IR::kSUB, IRT_I64, opaque, opaque);
  EXPECT_EQ(tr7, zero);

  buf->debugPrint(cerr, 1);
}

class CodeTest : public ::testing::Test {
protected:
  virtual void SetUp() {
    for (size_t i = 0; i < countof(code_); ++i) {
      code_[i] = stop();
    }

    T = Thread::createTestingThread(&code_[0], framesize_);
    for (size_t i = 0; i < framesize_; ++i) {
      T->setSlot(i, 3 + (i * 10));
    }
    cap_->enableBytecodeTracing();
  }

  virtual void TearDown() {
    if (T != NULL) {
      T->destroy();
      delete T;
    }
    T = NULL;
  }

  CodeTest() : framesize_(8) {
    cap_ = new Capability(&mm);
    l_ = new Loader(&mm, NULL);
  }

  virtual ~CodeTest() {
    if (T != NULL) {
      T->destroy();
      delete T;
    }
    delete cap_;
    delete l_;
  }

  Word arithABC(BcIns ins, Word slot1, Word slot2) {
    T->setPC(&code_[0]);
    T->setSlot(ins.b(), slot1);
    T->setSlot(ins.c(), slot2);
    code_[0] = ins;
    EXPECT_TRUE(cap_->run(T));
    return T->slot(ins.a());
  }

  Word arithAD(BcIns ins, Word slot) {
    T->setPC(&code_[0]);
    T->setSlot(ins.d(), slot);
    code_[0] = ins;
    EXPECT_TRUE(cap_->run(T));
    return T->slot(ins.a());
  }

  // Returns true iff branch was taken.
  bool branchTest(BcIns::Opcode opc, Word oper1, Word oper2) {
    T->setPC(&code_[0]);
    T->setSlot(0, 0);
    T->setSlot(1, oper1);
    T->setSlot(2, oper2);
    T->setSlot(3, 0xdeadbeef);
    code_[0] = BcIns::ad(opc, 1, 2);
    code_[1] = BcIns::aj(BcIns::kJMP, 0, +1);
    code_[2] = BcIns::ad(BcIns::kMOV, 0, 3);
    code_[3] = BcIns();
    EXPECT_TRUE(cap_->run(T));
    return T->slot(0) == 0;
  }

  BcIns stop() { return BcIns::ad(BcIns::kSTOP, 0, 0); }

  MemoryManager mm;
  Loader *l_;
  Capability *cap_;
  Thread *T;
  BcIns code_[32];
  u4 framesize_;
};

class ArithTest : public CodeTest {
};

TEST_F(ArithTest, Add) {
  ASSERT_EQ(0x579, arithABC(BcIns::abc(BcIns::kADDRR, 0, 1, 2),
                            0x123, 0x456));
  ASSERT_EQ(0x12345678, arithABC(BcIns::abc(BcIns::kADDRR, 0, 1, 2),
                                 0x12340000, 0x5678));
  ASSERT_EQ((Word)-5, arithABC(BcIns::abc(BcIns::kADDRR, 0, 1, 2),
                               -23, 18));
  ASSERT_EQ((Word)-5, arithABC(BcIns::abc(BcIns::kADDRR, 0, 1, 2),
                               -1, -4));
}

TEST_F(ArithTest, Sub) {
  ASSERT_EQ(0x333, arithABC(BcIns::abc(BcIns::kSUBRR, 0, 1, 2),
                            0x456, 0x123));
  ASSERT_EQ(0x12340000, arithABC(BcIns::abc(BcIns::kSUBRR, 0, 1, 2),
                                 0x12345678, 0x5678));
  ASSERT_EQ((Word)-41, arithABC(BcIns::abc(BcIns::kSUBRR, 0, 1, 2),
                               -23, 18));
  ASSERT_EQ((Word)3, arithABC(BcIns::abc(BcIns::kSUBRR, 0, 1, 2),
                               -1, -4));
}

TEST_F(ArithTest, Mul) {
  ASSERT_EQ(56088, arithABC(BcIns::abc(BcIns::kMULRR, 0, 1, 2),
                            123, 456));
  ASSERT_EQ((Word)-356136, arithABC(BcIns::abc(BcIns::kMULRR, 0, 1, 2),
                              -456, 781));
  ASSERT_EQ((Word)-356136, arithABC(BcIns::abc(BcIns::kMULRR, 0, 1, 2),
                              781, -456));
  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kMULRR, 0, 1, 2),
                        0, 781));
  ASSERT_EQ(356136, arithABC(BcIns::abc(BcIns::kMULRR, 0, 1, 2),
                             -456, -781));
}

TEST_F(ArithTest, Div) {
  // Testing for divide-by-0 crashes the test suite, so lets not do
  // that.
  ASSERT_EQ(123, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), 123, 1));
  ASSERT_EQ(-123, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), 123, -1));
  ASSERT_EQ(-123, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), -123, 1));
  ASSERT_EQ(123, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), -123, -1));

  ASSERT_EQ(2, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), -8, -3));
  ASSERT_EQ(2, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), 8, 3));
  ASSERT_EQ(-2, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), -8, 3));
  ASSERT_EQ(-2, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), 8, -3));

  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kDIVRR, 0, 1, 2), 0, 3));
}

TEST_F(ArithTest, Rem) {
  // Just like for division, we can't test for second arg of 0.
  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), 123, 1));
  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), 123, -1));
  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), -123, 1));
  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), -123, -1));

  ASSERT_EQ(-2, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), -8, -3));
  ASSERT_EQ(2, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), 8, 3));
  ASSERT_EQ(-2, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), -8, 3));
  ASSERT_EQ(2, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), 8, -3));

  ASSERT_EQ(0, arithABC(BcIns::abc(BcIns::kREMRR, 0, 1, 2), 0, 3));
}

TEST_F(ArithTest, Mov) {
  ASSERT_EQ(0x123456, arithAD(BcIns::ad(BcIns::kMOV, 0, 1), 0x123456));
}

TEST_F(ArithTest, Neg) {
  ASSERT_EQ(-0x123456, arithAD(BcIns::ad(BcIns::kNEG, 0, 1), 0x123456));
}

TEST_F(ArithTest, Not) {
  ASSERT_EQ(~(Word)0x123456, arithAD(BcIns::ad(BcIns::kNOT, 0, 1), 0x123456));
}

TEST_F(ArithTest, BranchLt) {
  ASSERT_FALSE(branchTest(BcIns::kISLT, 1, 0));
  ASSERT_TRUE(branchTest(BcIns::kISLT, 4, 5));
  ASSERT_TRUE(branchTest(BcIns::kISLT, -4, 5));
  ASSERT_TRUE(branchTest(BcIns::kISLT, 0, 1));
  ASSERT_TRUE(branchTest(BcIns::kISLT, -1, 0));
  ASSERT_FALSE(branchTest(BcIns::kISLT, 4, 4));
  ASSERT_FALSE(branchTest(BcIns::kISLT, -4, -4));
  ASSERT_FALSE(branchTest(BcIns::kISLT, 4, -4));
  ASSERT_FALSE(branchTest(BcIns::kISLT, -4, -5));
}

TEST_F(ArithTest, BranchGe) {
  ASSERT_TRUE(branchTest(BcIns::kISGE, 1, 0));
  ASSERT_FALSE(branchTest(BcIns::kISGE, 4, 5));
  ASSERT_FALSE(branchTest(BcIns::kISGE, -4, 5));
  ASSERT_FALSE(branchTest(BcIns::kISGE, 0, 1));
  ASSERT_FALSE(branchTest(BcIns::kISGE, -1, 0));
  ASSERT_TRUE(branchTest(BcIns::kISGE, 4, 4));
  ASSERT_TRUE(branchTest(BcIns::kISGE, -4, -4));
  ASSERT_TRUE(branchTest(BcIns::kISGE, 4, -4));
  ASSERT_TRUE(branchTest(BcIns::kISGE, -4, -5));
}

TEST_F(ArithTest, BranchEq) {
  ASSERT_FALSE(branchTest(BcIns::kISEQ, 1, 0));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, 4, 5));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, -4, 5));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, 0, 1));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, -1, 0));
  ASSERT_TRUE(branchTest(BcIns::kISEQ, 4, 4));
  ASSERT_TRUE(branchTest(BcIns::kISEQ, -4, -4));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, 4, -4));
  ASSERT_FALSE(branchTest(BcIns::kISEQ, -4, -5));
}

TEST_F(ArithTest, Alloc1) {
  uint64_t alloc_before = mm.allocated();
  T->setPC(&code_[0]);
  T->setSlot(1, 0x1234);
  T->setSlot(2, 0x5678);
  code_[0] = BcIns::abc(BcIns::kALLOC1, 0, 1, 2);
  code_[1] = BcIns::bitmapOffset(0);  // no bitmap
  ASSERT_TRUE(cap_->run(T));
  uint64_t alloc_after = mm.allocated();
  Closure *cl = (Closure*)T->slot(0);
  ASSERT_EQ((InfoTable*)0x1234, cl->info());
  ASSERT_EQ((Word)0x5678, cl->payload(0));
  ASSERT_EQ((uint64_t)(2 * sizeof(Word)), alloc_after - alloc_before);
}

TEST_F(ArithTest, AllocN_3) {
  uint64_t alloc_before = mm.allocated();
  T->setPC(&code_[0]);
  T->setSlot(1, 1111);
  T->setSlot(2, 2222);
  T->setSlot(3, 3333);
  code_[0] = BcIns::abc(BcIns::kALLOC, 0, 1, 2);
  code_[1] = BcIns::args(2, 3, 0, 0);
  code_[2] = BcIns::bitmapOffset(0);  // no bitmap
  ASSERT_TRUE(cap_->run(T));
  uint64_t alloc_after = mm.allocated();
  Closure *cl = (Closure*)T->slot(0);
  ASSERT_EQ((InfoTable*)1111, cl->info());
  ASSERT_EQ((Word)2222, cl->payload(0));
  ASSERT_EQ((Word)3333, cl->payload(1));
  ASSERT_EQ((uint64_t)(3 * sizeof(Word)), alloc_after - alloc_before);
}

TEST_F(ArithTest, AllocN_5) {
  uint64_t alloc_before = mm.allocated();
  T->setPC(&code_[0]);
  T->setSlot(1, 1111);
  T->setSlot(2, 2222);
  T->setSlot(3, 3333);
  T->setSlot(4, 4444);
  T->setSlot(5, 5555);
  code_[0] = BcIns::abc(BcIns::kALLOC, 0, 1, 4);
  code_[1] = BcIns::args(2, 3, 4, 5);
  code_[2] = BcIns::bitmapOffset(0);  // no bitmap
  code_[3] = BcIns::ad(BcIns::kMOV, 1, 2); // must not be skipped
  ASSERT_TRUE(cap_->run(T));
  uint64_t alloc_after = mm.allocated();
  Closure *cl = (Closure*)T->slot(0);
  ASSERT_EQ((InfoTable*)1111, cl->info());
  ASSERT_EQ((Word)2222, cl->payload(0));
  ASSERT_EQ((Word)3333, cl->payload(1));
  ASSERT_EQ((Word)4444, cl->payload(2));
  ASSERT_EQ((Word)5555, cl->payload(3));
  ASSERT_EQ((uint64_t)(5 * sizeof(Word)), alloc_after - alloc_before);
  ASSERT_EQ((Word)2222, T->slot(1));
}

class RunFileTest : public ::testing::Test {
protected:
  RunFileTest() : mm(NULL), loader(NULL), cap(NULL), T(NULL) { }
  virtual ~RunFileTest() {
    if (T) delete T;
    if (cap) delete cap;
    if (loader) delete loader;
    if (mm) delete mm;
  }

  virtual void SetUp() {
    mm = new MemoryManager();
    loader = new Loader(mm, "tests");
    cap = new Capability(mm);
    T = Thread::createThread(cap, 1U << 13);
  }

  virtual void TearDown() {
    delete T;
    T = NULL;
    delete cap;
    cap = NULL;
    delete loader;
    loader = NULL;
    delete mm;
    mm = NULL;
  }

  void run(const char *moduleName) {
    EXPECT_TRUE(loader->loadWiredInModules());
    EXPECT_TRUE(loader->loadModule(moduleName));
    size_t len = strlen(moduleName);
    char *entryClosure = new char[len + 20 + 1];
    strcpy(entryClosure, moduleName);
    strcat(entryClosure, ".test`closure");
    Closure *entry = loader->closure(entryClosure);
    ASSERT_TRUE(entry != NULL);
    if (DEBUG_COMPONENTS & DEBUG_INTERPRETER) {
      cap->enableBytecodeTracing();
    }
    Word *base = T->base();
    ASSERT_TRUE(cap->eval(T, entry));
    ASSERT_EQ(base, T->base());
    Closure *result = (Closure*)T->slot(0);
    ASSERT_TRUE(result != NULL);
    stringstream out;
    printClosure(out, result, true);
    EXPECT_EQ(string("IND -> GHC.Bool.True`con_info "), out.str());
  }

private:
  MemoryManager *mm;
  Loader *loader;
  Capability *cap;
  Thread *T;
};

TEST_F(RunFileTest, eval) {
  run("Bc.Bc0016");
}

TEST_F(RunFileTest, Bc0014) {
  run("Bc.Bc0014");
}

TEST_F(RunFileTest, Bc0017) {
  run("Bc.Bc0017");
}

TEST_F(RunFileTest, TailCallExact) {
  run("Bc.TailCallExact");
}

TEST_F(RunFileTest, TailCallOverapply) {
  run("Bc.TailCallOverapply");
}

TEST_F(RunFileTest, TailCallPap) {
  run("Bc.TailCallPap");
}

TEST_F(RunFileTest, Paps) {
  run("Bc.Paps");
}

TEST_F(RunFileTest, Gc01) {
  run("Bc.Gc01");
}

TEST_F(RunFileTest, Gc02) {
  run("Bc.Gc02");
}

TEST_F(RunFileTest, Gc03) {
  run("Bc.Gc03");
}

TEST_F(RunFileTest, Primes) {
  run("Bench.Primes");
}

TEST_F(RunFileTest, Append) {
  run("Bench.Append");
}

TEST_F(RunFileTest, SumFromTo1) {
  run("Bench.SumFromTo1");
}

TEST_F(RunFileTest, SumFromTo2) {
  run("Bench.SumFromTo2");
}

TEST_F(RunFileTest, SumFromTo3) {
  run("Bench.SumFromTo3");
}

TEST_F(RunFileTest, SumFromTo4) {
  run("Bench.SumFromTo4");
}

TEST_F(RunFileTest, SumSquare1) {
  run("Bench.SumSquare1");
}

TEST_F(RunFileTest, SumNoAlloc) {
  run("Bc.SumNoAlloc");
}

TEST_F(RunFileTest, SumMemLoad) {
  run("Bc.SumMemLoad");
}

TEST(HotCounters, Simple) {
  HotCounters counters(5);
  BcIns pc[] = { BcIns::ad(BcIns::kFUNC, 3, 0) };
  for (int i = 0; i < 4; ++i) {
    EXPECT_FALSE(counters.tick(pc));
  }
  EXPECT_TRUE(counters.tick(pc));
  // Counter should have been reset now.
  for (int i = 0; i < 4; ++i) {
    EXPECT_FALSE(counters.tick(pc));
  }
  EXPECT_TRUE(counters.tick(pc));
}

class RegAlloc : public ::testing::Test {
protected:
  IRBuffer *buf;
  Jit *jit;
  Word *stack;
  Word *spill;
  Assembler *as;
  MemoryManager *mm;
  Loader *loader;
  Thread *T;
protected:
  virtual void SetUp() {
    stack = new Word[256];
    jit = new Jit();
    buf = jit->buffer();
    buf->reset(&stack[11], &stack[28]);
    buf->disableOptimisation(IRBuffer::kOptFold);
    as = NULL;
    mm = NULL;
    loader = NULL;
    T = NULL;
    spill = NULL;
  }
  virtual void TearDown() {
    buf = NULL;
    if (stack) delete[] stack;  stack = NULL;
    if (spill) delete[] spill;  spill = NULL;
    if (jit) delete jit;  jit = NULL;
    if (as) delete as;  as = NULL;
    if (T) delete T;  T = NULL;
    if (loader) delete loader;  loader = NULL;
    if (mm) delete mm;  mm = NULL;
  }
  void Compile() {
    buf->debugPrint(cerr, 1);
    as = new Assembler(jit);
    as->setup(buf);
    as->assemble(jit->buffer(), jit->mcode());
  }
  Word *SetupThread() {
    spill = new Word[255];
    mm = new MemoryManager();
    loader = new Loader(mm, "tests");
    T = Thread::createThread(NULL, 1000);
    return T->base();
  }
  Word *RunAsm() {
    Word *base = T->base();
    asmEnter(NULL, T, spill, NULL, NULL, T->stackLimit(), jit->mcode()->start());
    return base;
  }
  virtual Word *Run(Word arg1, Word arg2) {
    Compile();
    Word *base = SetupThread();    
    base[0] = arg1;
    base[1] = arg2;
    return RunAsm();
  }
  virtual Word *Run(Word arg1, Word arg2, Word arg3, Word arg4) {
    Compile();
    Word *base = SetupThread();    
    base[0] = arg1;
    base[1] = arg2;
    base[2] = arg3;
    base[3] = arg4;
    return RunAsm();
  }

  void Dump() {
    const ::testing::TestInfo* const test_info =
      ::testing::UnitTest::GetInstance()->current_test_info();
    ofstream out;
    string filename("dump_");
    filename += test_info->name();
    filename += ".s";
    out.open(filename.c_str());
    jit->mcode()->dumpAsm(out);
    out.close();
  }

  RegAlloc() : buf(NULL), jit(NULL), stack(NULL) {}
  virtual ~RegAlloc() {
    TearDown();
  }
};

TEST_F(RegAlloc, Simple1) {
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, 1234);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  TRef tr4 = buf->emit(IR::kADD, IRT_I64, tr3, tr2);
  buf->setSlot(0, tr4);
  buf->setSlot(1, tr2);
  SnapNo snapno = buf->snapshot(NULL);
  //  buf->snap(snapno).debugPrint(cerr, buf->snapmap(), snapno);
  buf->emit(IR::kSAVE, IRT_VOID, snapno, 0);

  Word *base = Run(7, 0);

  EXPECT_EQ(1234 + 1234 + 7, base[0]);
  EXPECT_EQ(1234, base[1]);
  buf->debugPrint(cerr, 1);
  Dump();
}

TEST_F(RegAlloc, Simple2) {
  Word lit = 0x500001234;
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, lit);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  TRef tr4 = buf->emit(IR::kADD, IRT_I64, tr3, tr2);
  buf->setSlot(0, tr4);
  buf->setSlot(1, tr2);
  SnapNo snapno = buf->snapshot(NULL);
  //  buf->snap(snapno).debugPrint(cerr, buf->snapmap(), snapno);
  buf->emit(IR::kSAVE, IRT_VOID, snapno, 0);

  Word *base = Run(7, 0);

  EXPECT_EQ(lit + lit + 7, base[0]);
  EXPECT_EQ(lit, base[1]);
  buf->debugPrint(cerr, 1);
  Dump();
}

TEST_F(RegAlloc, Simple3) {
  Word lit1 = 0x500001234;
  Word lit2 = 0x500001236;
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, lit1);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  TRef tr4 = buf->literal(IRT_I64, lit2);
  TRef tr5 = buf->emit(IR::kADD, IRT_I64, tr3, tr4);
  buf->setSlot(0, tr5);
  buf->setSlot(1, tr2);
  buf->setSlot(2, tr4);
  SnapNo snapno = buf->snapshot(NULL);
  //  buf->snap(snapno).debugPrint(cerr, buf->snapmap(), snapno);
  buf->emit(IR::kSAVE, IRT_VOID, snapno, 0);

  Word *base = Run(7, 0);

  EXPECT_EQ(lit1 + lit2 + 7, base[0]);
  EXPECT_EQ(lit1, base[1]);
  EXPECT_EQ(lit2, base[2]);
  buf->debugPrint(cerr, 1);
  Dump();
}

TEST_F(RegAlloc, Simple4) {
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, 1234);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  TRef tr4 = buf->emit(IR::kADD, IRT_I64, tr3, tr2);
  buf->setSlot(0, tr4);
  buf->setSlot(1, tr2);
  buf->setSlot(2, tr1);
  buf->setSlot(3, tr3);
  SnapNo snapno = buf->snapshot(NULL);
  //  buf->snap(snapno).debugPrint(cerr, buf->snapmap(), snapno);
  buf->emit(IR::kSAVE, IRT_VOID, snapno, 0);

  Word *base = Run(7, 0);

  EXPECT_EQ(1234 + 1234 + 7, base[0]);
  EXPECT_EQ(1234, base[1]);
  EXPECT_EQ(7, base[2]);
  EXPECT_EQ(1234 + 7, base[3]);
  buf->debugPrint(cerr, 1);
  Dump();
}

TEST_F(RegAlloc, ManyRegs) {
  Word lit1 = 0x100000000 + 1234;
  Word lit2 = 0x100000000 + 8642;
  TRef l1 = buf->literal(IRT_I64, lit1);
  TRef l2 = buf->literal(IRT_I64, lit2);
  TRef a = buf->slot(0);
  TRef b = buf->slot(1);
  TRef c = buf->slot(2);
  TRef d = buf->slot(3);
  TRef e = buf->emit(IR::kADD, IRT_I64, a, b);
  TRef f = buf->emit(IR::kADD, IRT_I64, a, c);
  TRef g = buf->emit(IR::kADD, IRT_I64, a, d);
  TRef h = buf->emit(IR::kADD, IRT_I64, b, c);
  TRef i = buf->emit(IR::kADD, IRT_I64, b, d);
  TRef j = buf->emit(IR::kADD, IRT_I64, c, d);
  TRef k = buf->emit(IR::kADD, IRT_I64, a, e);
  TRef l = buf->emit(IR::kADD, IRT_I64, a, f);
  TRef m = buf->emit(IR::kADD, IRT_I64, a, g);
  TRef n = buf->emit(IR::kADD, IRT_I64, a, i);

  buf->setSlot(0, n);
  buf->setSlot(1, m);
  buf->setSlot(2, l);
  buf->setSlot(3, k);
  buf->setSlot(4, j);
  buf->setSlot(5, i);
  buf->setSlot(6, h);
  buf->setSlot(7, g);
  buf->setSlot(8, f);
  buf->setSlot(9, e);
  // buf->setSlot(10, l1);
  // buf->setSlot(11, l2);
  buf->setSlot(12, a);
  buf->setSlot(13, b);
  buf->setSlot(14, c);
  buf->setSlot(15, d);
  SnapNo snapno = buf->snapshot(NULL);
  buf->emit(IR::kSAVE, IRT_VOID, snapno, 0);

  Word s0 = 10, s1 = 100, s2 = 1000, s3 = 10000;
  Word *base = Run(s0, s1, s2, s3);

  EXPECT_EQ(s0 + s1 + s3, base[0]);
  EXPECT_EQ(s0 + s0 + s3, base[1]);
  EXPECT_EQ(s0 + s0 + s2, base[2]);
  EXPECT_EQ(s0 + s0 + s1, base[3]);
  EXPECT_EQ(s2 + s3, base[4]);
  EXPECT_EQ(s1 + s3, base[5]);
  EXPECT_EQ(s1 + s2, base[6]);
  EXPECT_EQ(s0 + s3, base[7]);
  EXPECT_EQ(s0 + s2, base[8]);
  EXPECT_EQ(s0 + s1, base[9]);
  EXPECT_EQ(s0, base[12]);
  EXPECT_EQ(s1, base[13]);
  EXPECT_EQ(s2, base[14]);
  EXPECT_EQ(s3, base[15]);
  // EXPECT_EQ(1234, base[1]);
  // EXPECT_EQ(7, base[2]);
  // EXPECT_EQ(1234 + 7, base[3]);
  buf->debugPrint(cerr, 1);
  buf->snap(snapno).debugPrint(cerr, buf->snapmap(), snapno);
  Dump();
}

TEST_F(RegAlloc, SnapTwice) {
  Word lit1 = 0x50001234;
  Word lit2 = 0x50001236;
  TRef l1 = buf->literal(IRT_I64, lit1);
  TRef l2 = buf->literal(IRT_I64, lit2);

  TRef s[4];
  for (int i = 0; i < 4; ++i)
    s[i] = buf->slot(i);

  TRef t[16];
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j)
      t[i * 4 + j] = buf->emit(IR::kADD, IRT_I64, s[i], s[j]);
  
  for (int i = 0; i < 16; ++i)
    buf->setSlot(i, t[i]);
  buf->emit(IR::kLT, IRT_I64|IRT_GUARD, s[0], l1);
  
  TRef u[16];
  for (int i = 0; i < 16; ++i) {
    u[i] = buf->emit(IR::kADD, IRT_I64, t[(i + 3) % 16], t[(i + 7) % 16]);
  }

  for (int i = 0; i < 16; ++i)
    buf->setSlot(i, u[i]);
  buf->emit(IR::kLT, IRT_I64|IRT_GUARD, s[1], l1);

  TRef v[4];
  for (int i = 0; i < 4; ++i)
    v[i] = buf->emit(IR::kADD, IRT_I64, u[i], u[7-i]);

  TRef x[2];
  for (int i = 0; i < 2; ++i) {
    x[i] = buf->emit(IR::kADD, IRT_I64, v[i], v[3-i]);
  }

  TRef y1 = buf->emit(IR::kADD, IRT_I64, s[0], s[1]);
  TRef y2 = buf->emit(IR::kADD, IRT_I64, s[2], s[3]);
  TRef z = buf->emit(IR::kADD, IRT_I64, y1, y2);
  for (int i = 0; i < 16; ++i)
    buf->setSlot(i, TRef());

  buf->setSlot(0, z);
  buf->setSlot(1, x[0]);
  buf->setSlot(2, x[1]);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Word s0 = 10, s1 = 100, s2 = 1000, s3 = 10000;
  Word *base = Run(s0, s1, s2, s3);

  EXPECT_EQ((s0 + s1) + (s2 + s3), base[0]);
  buf->debugPrint(cerr, 1);
  Dump();
}

class TestFragment : public ::testing::Test {
protected:
  MemoryManager mm;
  Loader loader;
  Capability cap;
  Thread *T;
  Jit jit;
  IRBuffer *buf;
  Word stack[200];
  Fragment *F;

public:
  TestFragment() : mm(), loader(&mm, "tests"), cap(&mm),
                   T(NULL), jit(), buf(NULL), F(NULL) {
  }
  ~TestFragment() { TearDown(); }
  virtual void SetUp() {
    T = Thread::createThread(&cap, 1000);
    buf = jit.buffer();
    buf->reset(&stack[10], &stack[18]);
  }

  virtual void TearDown() {
    if (T) delete T; T = NULL;
    if (F) delete F; F = NULL;
    buf = NULL;
  }

  void Assemble() {
    buf->debugPrint(cerr, 1);
    Assembler *as = jit.assembler();
    as->setup(buf);
    as->assemble(buf, jit.mcode());
    buf->debugPrint(cerr, 1);
    F = jit.saveFragment();
    Dump();
  }

  void Dump() {
    const ::testing::TestInfo* const test_info =
      ::testing::UnitTest::GetInstance()->current_test_info();
    ofstream out;
    string filename("dump_TestFragment_");
    filename += test_info->name();
    filename += ".s";
    out.open(filename.c_str());
    jit.mcode()->dumpAsm(out);
    out.close();
  }

  void Run() {
    Word *base = T->base();
    asmEnter(F, T, base + F->spillOffset(), NULL, NULL,
             T->stackLimit(), F->entry());
  }

  void RunWithHeap(Word *hp, Word *hplim) {
    Word *base = T->base();
    asmEnter(F, T, base + F->spillOffset(), hp, hplim,
             T->stackLimit(), F->entry());
  }
};

TEST(TestFragment2, TestFragment2) {
  MemoryManager mm;
  Loader loader(&mm, "tests");
  Capability cap(&mm);
  Thread *T = Thread::createThread(&cap, 1000);
  Jit jit;
  IRBuffer *buf = jit.buffer();
  Word stack[200];
  buf->reset(&stack[10], &stack[18]);
  //  buf->disableOptimisation(IRBuffer::kOptFold);

  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, 5);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  buf->setSlot(0, tr3);
  buf->setSlot(1, tr2);
  buf->emit(IR::kLT, IRT_VOID|IRT_GUARD, tr1, tr2);
  TRef tr4 = buf->emit(IR::kADD, IRT_I64, tr3, tr2);
  TRef tr5 = buf->baseLiteral(&stack[12]);
  buf->setSlot(0, tr4);
  buf->setSlot(1, tr5);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  buf->debugPrint(cerr, 1);
  Assembler *as = jit.assembler();
  as->setup(buf);
  as->assemble(buf, jit.mcode());
  buf->debugPrint(cerr, 1);
  Fragment *F = jit.saveFragment();

  Word *base = T->base();
  // Should abort at the first guard.
  base[0] = 10;
  base[1] = 0;
  asmEnter(F, T, base + 10, NULL, NULL, T->stackLimit(), F->entry());
  EXPECT_EQ(15, base[0]);
  EXPECT_EQ(5, base[1]);

  // Should run to the end.
  base[0] = 4;
  base[1] = 0;
  asmEnter(F, T, base + 10, NULL, NULL, T->stackLimit(), F->entry());
  EXPECT_EQ(14, base[0]);
  EXPECT_EQ((Word)(base + 2), base[1]);
}

TEST_F(TestFragment, Test1) {
  TRef tr1 = buf->slot(0);
  TRef tr2 = buf->literal(IRT_I64, 5);
  TRef tr3 = buf->emit(IR::kADD, IRT_I64, tr1, tr2);
  buf->setSlot(0, tr3);
  buf->setSlot(1, tr2);
  buf->emit(IR::kLT, IRT_VOID|IRT_GUARD, tr1, tr2);
  TRef tr4 = buf->emit(IR::kADD, IRT_I64, tr3, tr2);
  TRef tr5 = buf->baseLiteral(&stack[12]);
  buf->setSlot(0, tr4);
  buf->setSlot(1, tr5);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word *base = T->base();
  // Should abort at the first guard.
  base[0] = 10;
  base[1] = 0;
  Run();
  EXPECT_EQ(15, base[0]);
  EXPECT_EQ(5, base[1]);

  EXPECT_EQ(T->base(), base);
  // Should run to the end.
  base[0] = 4;
  base[1] = 0;
  Run();
  EXPECT_EQ(14, base[0]);
  EXPECT_EQ((Word)(base + 2), base[1]);
}

TEST_F(TestFragment, Test2) {
  // Program:
  //   f(x, y): if (y <= 0) return x; else f(x + 5, y - 1);
  //  

  TRef x = buf->slot(0);
  TRef y = buf->slot(1);
  TRef five = buf->literal(IRT_I64, 5);
  TRef one = buf->literal(IRT_I64, 1);
  TRef zero = buf->literal(IRT_I64, 0);
  buf->emit(IR::kGT, IRT_VOID|IRT_GUARD, y, zero);
  TRef x1 = buf->emit(IR::kADD, IRT_I64, x, five);
  buf->setSlot(0, x1);
  TRef y1 = buf->emit(IR::kSUB, IRT_I64, y, one);
  buf->setSlot(1, y1);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, IR_SAVE_LOOP, 0);  // loop

  Assemble();

  Word *base = T->base();
  base[0] = 0;
  base[1] = 5;
  Run();
  EXPECT_EQ(5 * 5, base[0]);
  EXPECT_EQ(0, base[1]);
}

TEST_F(TestFragment, RestoreSnapSpill) {
  buf->disableOptimisation(IRBuffer::kOptFold);
  TRef s[5];
  for (int i = 0; i < 5; ++i) {
    s[i] = buf->slot(i);
  }
  TRef t[16];
  for (int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      t[i * 4 + j] = buf->emit(IR::kADD, IRT_I64, s[i], s[j]);
    }
  }
  TRef stop = buf->slot(5);  // non-zero = exit at snapshot

  TRef u[16];
  for (int i = 0; i < 16; ++i) {
    u[i] = buf->emit(IR::kSUB, IRT_I64, t[i], s[4]);
    buf->setSlot(i, ((i % 2) == 1) ? t[i] : u[i]);
  }
  TRef zero = buf->literal(IRT_I64, 0);
  buf->emit(IR::kEQ, IRT_VOID|IRT_GUARD, stop, zero);
  TRef v[8];
  for (int i = 0; i < 8; ++i) {
    v[i] = buf->emit(IR::kADD, IRT_I64, u[2 * i], u[2 * i + 1]);
  }
  TRef w[4];
  for (int i = 0; i < 4; ++i) {
    w[i] = buf->emit(IR::kADD, IRT_I64, v[2 * i], v[2 * i + 1]);
  }
  for (int i = 0; i < 2; ++i) {
    TRef x = buf->emit(IR::kADD, IRT_I64, w[2 * i], w[2 * i + 1]);
    buf->setSlot(i, x);
  }
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word *base = T->base();
  Word s0, s1, s2, s3, s4, s5;
  base[0] = s0 = 1;
  base[1] = s1 = 10;
  base[2] = s2 = 100;
  base[3] = s3 = 1000;
  base[4] = s4 = 7;
  base[5] = s5 = 1;  // exit at snapshot;
  Run();
  EXPECT_EQ((Word)(s0 + s0 - s4), base[0]);
  EXPECT_EQ((Word)(s0 + s1), base[1]);
  EXPECT_EQ((Word)(s0 + s2 - s4), base[2]);
  EXPECT_EQ((Word)(s0 + s3), base[3]);
  EXPECT_EQ((Word)(s1 + s0 - s4), base[4]);
  EXPECT_EQ((Word)(s1 + s1), base[5]);
  EXPECT_EQ((Word)(s1 + s2 - s4), base[6]);
  EXPECT_EQ((Word)(s1 + s3), base[7]);
  EXPECT_EQ((Word)(s2 + s0 - s4), base[8]);
  EXPECT_EQ((Word)(s2 + s1), base[9]);
  EXPECT_EQ((Word)(s2 + s2 - s4), base[10]);
  EXPECT_EQ((Word)(s2 + s3), base[11]);
  EXPECT_EQ((Word)(s3 + s0 - s4), base[12]);
  EXPECT_EQ((Word)(s3 + s1), base[13]);
  EXPECT_EQ((Word)(s3 + s2 - s4), base[14]);
  EXPECT_EQ((Word)(s3 + s3), base[15]);

  base[0] = 1;
  base[1] = 10;
  base[2] = 100;
  base[3] = 1000;
  base[4] = 7;
  base[5] = 0;  // exit at snapshot;
  Run();
  EXPECT_EQ(2210, base[0]);
  EXPECT_EQ(6566, base[1]);
}

TEST_F(TestFragment, ItblGuard) {
  TRef clos1 = buf->slot(0);
  TRef clos2 = buf->slot(1);
  TRef lit1 = buf->literal(IRT_I64, 5);
  TRef lit2 = buf->literal(IRT_I64, 15);
  TRef lit3 = buf->literal(IRT_I64, 25);
  TRef itbl1 = buf->literal(IRT_I64, 1234);
  TRef itbl2 = buf->literal(IRT_I64, 500000001234);
  buf->setSlot(0, lit1);
  buf->emit(IR::kEQINFO, IRT_VOID|IRT_GUARD, clos1, itbl1);
  buf->setSlot(0, lit2);
  buf->emit(IR::kEQINFO, IRT_VOID|IRT_GUARD, clos2, itbl2);
  buf->setSlot(0, lit3);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word *base = T->base();

  Word heap[2];
  heap[0] = 1234;
  heap[1] = 500000001234;
  // Should abort at the first guard.
  base[0] = (Word)&heap[0];
  base[1] = (Word)&heap[1];
  Run();
  EXPECT_EQ(25, base[0]);

  base[0] = (Word)&heap[0];
  base[1] = (Word)&heap[0];  // Second guard should fail.
  Run();
  EXPECT_EQ(15, base[0]);

  base[0] = (Word)&heap[1];  // First guard should fail.
  base[1] = (Word)&heap[1];
  Run();
  EXPECT_EQ(5, base[0]);
}

TEST_F(TestFragment, LoadField) {
  TRef clos1 = buf->slot(0);
  TRef lit1 = buf->literal(IRT_I64, 5);
  buf->setSlot(0, lit1);  // Set to some default value.
  TRef ref = buf->emit(IR::kFREF, IRT_PTR, clos1, 1);
  TRef val = buf->emit(IR::kFLOAD, IRT_UNKNOWN, ref, 0);
  buf->setSlot(0, val);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word *base = T->base();

  Word heap[2];
  heap[0] = 1234;
  // Use 64 bit literal, to ensure we're loading the full 64 bits.
  heap[1] = 500000001234;
  base[0] = (Word)&heap[0];
  Run();
  EXPECT_EQ((Word)500000001234, base[0]);
}

TEST_F(TestFragment, Alloc1) {
  TRef itbl = buf->literal(IRT_INFO, 0x123456789);
  TRef lit1 = buf->literal(IRT_I64, 5);
  TRef lit2 = buf->literal(IRT_I64, 500000001234);
  TRef lit3 = buf->literal(IRT_I64, 23);
  TRef lit4 = buf->literal(IRT_I64, 34);
  buf->setSlot(0, lit3);
  buf->emitHeapCheck(3);  // Alloc three words
  IRBuffer::HeapEntry he = 0;
  TRef alloc = buf->emitNEW(itbl, 2, &he);
  buf->setField(he, 0, lit1);
  buf->setField(he, 1, lit2);
  buf->setSlot(0, lit4);
  buf->setSlot(1, alloc);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word heap[10];

  // Run 1: Allocate normally
  memset(heap, 0, sizeof(heap));
  Word *base = T->base();
  base[0] = 0;
  RunWithHeap(&heap[0], &heap[10]);
  EXPECT_EQ(34, base[0]);
  EXPECT_EQ(&heap[3], cap.traceExitHp());
  EXPECT_EQ(&heap[10], cap.traceExitHpLim());
  EXPECT_EQ((Word)&heap[0], base[1]);
  EXPECT_EQ(0x123456789, heap[0]);
  EXPECT_EQ(5, heap[1]);
  EXPECT_EQ(500000001234, heap[2]);
  EXPECT_EQ(0, heap[3]);  // Should not be touched.

  // Run 2: Heap check should fail. We have only 2 words.
  memset(heap, 0, sizeof(heap));
  base = T->base();
  base[0] = 0;
  RunWithHeap(&heap[0], &heap[2]);
  EXPECT_EQ(23, base[0]);
  EXPECT_EQ(&heap[3], cap.traceExitHp());
  EXPECT_EQ(&heap[2], cap.traceExitHpLim());
  
  // Run 3: Heap check should succeed.  We have exactly 3 words.
  memset(heap, 0, sizeof(heap));
  base = T->base();
  base[0] = 0;
  RunWithHeap(&heap[0], &heap[3]);
  EXPECT_EQ(34, base[0]);
  EXPECT_EQ(&heap[3], cap.traceExitHp());
  EXPECT_EQ(&heap[3], cap.traceExitHpLim());
}

TEST_F(TestFragment, Alloc2) {
  TRef itbl = buf->literal(IRT_INFO, 0x123456789);
  TRef lit1 = buf->literal(IRT_I64, 5);
  TRef lit2 = buf->literal(IRT_I64, 7);
  TRef field1 = buf->slot(0);
  TRef field2 = buf->slot(1);
  buf->emitHeapCheck(6);
  IRBuffer::HeapEntry he = 0;
  TRef alloc1 = buf->emitNEW(itbl, 2, &he);
  buf->setField(he, 0, field1);
  buf->setField(he, 1, field2);

  TRef field3 = buf->emit(IR::kADD, IRT_I64, field1, lit1);
  TRef field4 = buf->emit(IR::kADD, IRT_I64, field2, lit2);

  TRef alloc2 = buf->emitNEW(itbl, 2, &he);
  cerr << "he " << he << endl;
  buf->setField(he, 0, field3);
  buf->setField(he, 1, field4);

  buf->setSlot(0, alloc1);
  buf->setSlot(1, alloc2);
  buf->emit(IR::kSAVE, IRT_VOID|IRT_GUARD, 0, 0);

  Assemble();

  Word heap[10];

  memset(heap, 0, sizeof(heap));
  Word *base = T->base();
  base[0] = 123;
  base[1] = 37;
  RunWithHeap(&heap[0], &heap[10]);
  EXPECT_EQ(&heap[6], cap.traceExitHp());
  EXPECT_EQ(&heap[10], cap.traceExitHpLim());
  EXPECT_EQ((Word)&heap[0], base[0]);
  EXPECT_EQ((Word)&heap[3], base[1]);
  EXPECT_EQ(0x123456789, heap[0]);
  EXPECT_EQ(123, heap[1]);
  EXPECT_EQ(37, heap[2]);
  EXPECT_EQ(0x123456789, heap[3]);
  EXPECT_EQ(123 + 5, heap[4]);
  EXPECT_EQ(37 + 7, heap[5]);
}

int main(int argc, char *argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
