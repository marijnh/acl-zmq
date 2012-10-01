MACHINE = $(shell uname -m)

ifeq ($(MSYSTEM),MINGW32)
LIBRARY_NAME = acl-zmq-$(MACHINE).dll
PTHREAD_HOME ?= ./pthreads-win32
ARCH ?= $(PROCESSOR_ARCHITECTURE)
ZMQ_HOME ?= "C:/Program Files/ZeroMQ 2.2.0"
else
LIBRARY_NAME = acl-zmq-$(MACHINE).so
endif


$(LIBRARY_NAME): zeromq-thread.c
ifeq ($(MSYSTEM),MINGW32)
	gcc -shared \
            -L$(PTHREAD_HOME)/dll/$(ARCH) -lpthreadGC2 \
            -L$(ZMQ_HOME)/bin -llibzmq-v100-mt \
            -I$(ZMQ_HOME)/include -I$(PTHREAD_HOME)/include \
            -o $(LIBRARY_NAME) zeromq-thread.c
else
	gcc -fpic -shared -lc -o $(LIBRARY_NAME) zeromq-thread.c
endif

clean:
	find * | grep fasl$ | xargs rm -f
	rm -f $(LIBRARY_NAME)
