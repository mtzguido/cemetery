.PHONY: clean all re test test-* prepare
TARGET := cmt
SOURCE := Cemetery.hs

include common.mk
ifneq ($(MAKECMDGOALS),clean)
-include .deps.mk
endif

obj-y          := AST.o Cemetery.o IR.o Common.o Translate.o Builtins.o \
		  CGen.o Optimize.o CPrint.o CLang.o

automods       := Lexer.hs Parser.hs Prologue.hs
obj-y          += $(patsubst %.hs,%.o,$(automods))

$(TARGET): $(obj-y)
	$(Q)$(SAY) "  LD	$@"
	$(Q)$(GHC) $(SOURCE) $(CFLAGS) -o $(TARGET)

.deps.mk: $(automods) *.hs
	$(Q)$(SAY) "  DEPS"
	$(Q)ghc -M $(SOURCE) -dep-makefile .deps.mk

prepare: $(automods)

%.hi: %.o ;
%.o: %.hs
	$(Q)$(SAY) "  GHC	$@"
	$(Q)$(GHC) -c $< $(CFLAGS)

all: $(TARGET)

Lexer.hs: Lexer.x
	$(Q)$(SAY) " ALEX	$<"
	$(Q)alex $< -o $@

Parser.hs: Parser.y
	$(Q)$(SAY) " HAPPY	$<"
	$(Q)happy -i $< -o $@

Prologue.hs: Prologue.c Prologue.h
	$(Q)$(SAY) " PROLOGUE"
	$(Q)ghc scripts/mk_prologue.hs -e main

clean:
	$(Q)$(SAY) " CLEAN"
	$(Q)rm -f $(TARGET)
	$(Q)rm -f *.o *.hi
	$(Q)rm -f $(automods)
	$(Q)rm -f Parser.info

test: test-all

test-%:
	./scripts/$@.sh
