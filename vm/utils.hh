#ifndef _UTILS_H_
#define _UTILS_H_

template <typename A> bool within(A low, A high, A value) {
  return low <= value && value <= high;
};

#endif /* _UTILS_H_ */
