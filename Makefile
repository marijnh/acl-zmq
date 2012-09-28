MACHINE = $(shell uname -m)
LIBRARY_NAME = acl-zmq-$(MACHINE).so

$(LIBRARY_NAME): zeromq-thread.c
	gcc -fpic -shared -lc -o $(LIBRARY_NAME) zeromq-thread.c

clean:
	find * | grep fasl$ | xargs rm -f
	rm -f $(LIBRARY_NAME)
