.PHONY:clean all re test test-*
TARGET = cmt
SOURCE = Cemetery.hs
CFLAGS =
CC = ghc --make

SAY = echo
ifeq (${V},1)
	Q=
else
	Q=@
endif

automods=Lexer.hs

$(TARGET): $(wildcard *.hs) $(automods)
	$(Q)$(SAY) "  GHC	$@"
	$(Q)$(CC) --make $(SOURCE) $(CFLAGS) -o $(TARGET)

all: $(TARGET)

Lexer.hs: Lexer.x
	$(Q)$(SAY) " ALEX	$<"
	$(Q)alex $< -o $@

clean:
	$(Q)$(SAY) "CLEAN"
	$(Q)rm -f $(TARGET)
	$(Q)rm -f *.o *.hi
	$(Q)rm -f $(automods)
	$(Q)rm -f $(patsubst %,%.hi, $(mods))

test: test-parse

test-%: $(TARGET)
	./scripts/$@.sh

re: clean all
