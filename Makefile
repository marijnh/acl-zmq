zeromq-thread.so: zeromq-thread.c
	gcc -fpic -shared -lc -o zeromq-thread.so zeromq-thread.c

clean:
	find * | grep fasl$ | xargs rm -f
	rm zeromq-thread.so
