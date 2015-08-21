.PHONY: clean all re bench

all: sha1sum

sha1sum: sha1.o sha1sum.o
	$(CC) -pg $^ -o $@

%.o: %.c
	$(CC) -c -pg $< -o $@

sha1.c sha1.h: sha1.cmt
	../cmt $<

clean:
	rm -f sha1.c *.o

bench: sha1sum
	dd if=/dev/urandom of=blob bs=1M count=1
	time ./sha1sum blob
	gprof sha1sum > prof
	gprof2dot.py prof > vprof

re: clean all
