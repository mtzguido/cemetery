.PHONY: clean all re test test-*
TARGET := cmt
SOURCE := Cemetery.hs

include common.mk

ifeq ($(filter all, $(MAKECMDGOALS)),)
all:
	@$(MAKE) --no-print-directory -f cmt.mk $@
endif

$(MAKECMDGOALS):
	@$(MAKE) --no-print-directory -f cmt.mk $@

test: test-all

test-%:
	./scripts/$@.sh

re: clean all
