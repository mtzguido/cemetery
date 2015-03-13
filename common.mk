CFLAGS :=
GHC := ghc

SAY := echo
ifeq (${V},1)
	Q :=
else
	Q := @
endif
