#include "gtest/gtest.h"
#include "thread.hh"

using namespace lambdachine;

TEST(ThreadTest, StartStop) {
  Thread *T = Thread::createThread(NULL, 20);
  ASSERT_TRUE(T != NULL);
  ASSERT_TRUE(T->isValid());
  delete T;
}


int main(int argc, char *argv[]) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
