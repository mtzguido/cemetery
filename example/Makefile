.PHONY: clean

sha1sum: sha1.o sha1sum.o
	gcc $^ -o $@

sha1.c: sha1.cmt
	cmt $<

clean:
	rm -f sha1.c *.o
