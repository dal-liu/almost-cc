PL_CLASS := IR
DST_PL_CLASS := L3
COMPILER := ../target/release/$(PL_CLASS)
EXT_CLASS := $(PL_CLASS)
CC_CLASS := $(PL_CLASS)c

test: $(COMPILER)
	../scripts/test.sh $(EXT_CLASS) $(CC_CLASS) "tests"

clean:
	rm -fr bin obj *.out *.o core.* `find tests -iname *.tmp`
	rm -fr *.$(DST_PL_CLASS)

.PHONY: test clean
