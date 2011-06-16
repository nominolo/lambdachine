#ifndef _LAMBDACHINE_STORAGE_MANAGER_H
#define _LAMBDACHINE_STORAGE_MANAGER_H

#include "Common.h"

void initStorageManager();
void dumpStorageManagerState();

void* allocInfoTable(u4 nwords);
void* allocStaticClosure(u4 nwords);
void* allocClosure(u4 nwords);

int looksLikeInfoTable(void *);
int looksLikeClosure(void *);
int looksLikeStaticClosure(void *);
int isClosure(void *p);

#endif
