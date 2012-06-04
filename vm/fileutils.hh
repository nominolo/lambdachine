#ifndef _FILEUTILS_H_
#define _FILEUTILS_H_

#include "common.hh"

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>

_START_LAMBDACHINE_NAMESPACE

/* TODO: make Windows compatible */
bool fileExists(const char *path);

_END_LAMBDACHINE_NAMESPACE


#endif /* _FILEUTILS_H_ */
