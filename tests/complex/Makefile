.PHONY: $(MAKECMDGOALS)

tests-y := $(wildcard *)

%:
	$(MAKE) -C $@

all: $(tests-y)
