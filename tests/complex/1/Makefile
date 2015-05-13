.PHONY:all

all:
	../../../cmt crypt.cmt
	cat crypt.c driver.c > prog.c
	gcc prog.c
	./a.out > out.real
	diff out.real out.expected
	rm -f out.real
