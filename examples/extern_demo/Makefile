CC ?= cc
CFLAGS ?= -O2 -fPIC

all: libextern_demo.a

extern_demo.o: extern_demo.c
	$(CC) $(CFLAGS) -c $< -o $@

libextern_demo.a: extern_demo.o
	ar rcs $@ $^

clean:
	rm -f extern_demo.o libextern_demo.a
