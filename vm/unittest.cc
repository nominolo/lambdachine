#include "gtest/gtest.h"
#include "thread.hh"
#include "memorymanager.hh"
#include "loader.hh"
#include "assembler.hh"
#include <iostream>

using namespace std;
using namespace lambdachine;

TEST(ThreadTest, StartStop) {
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

TEST(RegSetTest, exclude) {
  RegSet rs = RegSet::range(4, 10).exclude(8);;
  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ(4 <= i && i < 10 && i != 8, rs.test(i));
  }
}

TEST(RegSetTest, pickBotTop) {
  RegSet rs = RegSet::range(4, 10);
  rs.set(15);
  ASSERT_EQ((Reg)4, rs.pickBot());
  ASSERT_EQ((Reg)15, rs.pickTop());
}

int main(int argc, char *argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
