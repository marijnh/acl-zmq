# ZeroMQ bindings for Allegro Common Lisp

This is a fork (/rewrite) of Hans Hübner's [ACL-ZMQ][1], which was
based on Vitaly Mayatskikh's [CL-ZMQ][2].

The rewrite was motivated by the fact that ZeroMQ's API is blocking,
and Allegro CL is green-threaded. This means that all threads in the
CL image will halt as ZeroMQ is, for example, waiting for a message.

My solution is to spawn a 'companion' thread, whose code is written in
C, for each CL thread that intends to use ZeroMQ. The two threads then
communicate through a semaphore, a pipe, and a shared struct. Lisp,
when it wants to call a ZeroMQ API function, puts the parameters in
the struct, posts to the semaphore, and tries to read a byte from the
pipe. On the other side, the C thread sits around, waiting on the
semaphore. When that opens, it reads the parameters from the struct,
makes the call, puts the result back in the struct, and writes a byte
to the pipe. (The pipe is used because fds are the easiest way to hook
into ACL's thread-blocking and notification system.)

Some calls don't have to go through this hack -- most notably, msg
structs can be manipulated from Lisp with impunity. Sockets, however,
can only be created/closed/used on the C side, since ZeroMQ sockets
are tied to the thread that creates them. The Lisp side can happily
pass the pointer around and 'own' it, but all actions on it have to
happen in C.

This library exposes, basically, the ZeroMQ API. All calls that have
to be wrapped with magic are wrapped, and returned error codes are
automatically translated to CL conditions. There's a few with- forms
for resources (msg structs, sockets, the companion thread) that have
to be cleaned up, and glue code for getting Lisp strings and byte
arrays in and out of messages, and for marshalling Lisp strings and
integers to `getsockopt` and `setsockopt` calls.

The `with-zmq` macro creates the companion thread (whose association
happens through the lexical `*helper*` variable), and ensures that a
ZeroMQ context is created and accessible to these threads.

The API is intended not to be `:use`-ed, in that the package name is
short (`zmq`) and full of clashy names (`socket`, `connect`, etc).
Here's an example, a publish server that broadcasts random integers
every second:

    (defun random-publisher ()
      (zmq:with-zmq
        (zmq:with-socket (sock zmq:+pub+)
          (zmq:bind sock "tcp://*:11331")
          (zmq:with-msg (msg)
            (loop
             (sleep 1)
             (zmq:msg-init-string msg (princ-to-string (random 1000)))
             (zmq:send sock msg))))))

The C code is compiled with a simple makefile, and the library calls
out to `make` and `gcc` at load time. Thus, in its current form,
there's not much hope for running this out of the box on non-Linux/BSD
platforms. The code uses POSIX semaphores as well.

[1]: https://github.com/hanshuebner/acl-zmq
[2]: http://repo.or.cz/w/cl-zmq.git/
