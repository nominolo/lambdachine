# Lambdachine To-Dos

## Garbage Collector

### Large Objects

Byte arrays and arrays of pointers should not be managed using the
copying collector.

## Missing Primitives

Status: (M)issing, (I)ncomplet
Difficulty: (1) Easy, (2) Medium, (3) Hard, (?) Unknown

### Arrays

### Exceptions
  
  - I,2 raise#
  - M,2 catch#
  - M,2 raiseIO#, M
  - M,? maskAsyncExceptions#, M
  - M,? maskUninterruptible#, M
  - M,? unmaskAsyncExceptions#, M
  - M,? getMaskingState#
