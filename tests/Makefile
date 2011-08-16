TOP=..

LIT=$(TOP)/utils/lit/lit.py
TESTS ?= .
CLEAN_DIRS=Bc Bench Toys

# Verbosity
Q=

check:
	$(Q) $(LIT) $(TESTS) $(LITARGS)

clean:
	$(Q) find $(CLEAN_DIRS) -name "*.lcbc" -delete
	$(Q) find $(CLEAN_DIRS) -name "*.o"    -delete
	$(Q) find $(CLEAN_DIRS) -name "*.hi"   -delete
