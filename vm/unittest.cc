#include "gtest/gtest.h"
#include "thread.hh"
#include "memorymanager.hh"
#include "loader.hh"
#include "assembler.hh"
#include "capability.hh"
#include "objects.hh"
#include "miscclosures.hh"

#include <iostream>
#include <sstream>

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
    delete T;
    T = NULL;
  }

  CodeTest() : framesize_(8) {
    cap_ = new Capability(&mm);
    l_ = new Loader(&mm, NULL);
  }

  virtual ~CodeTest() {
    delete T;
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

int main(int argc, char *argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
