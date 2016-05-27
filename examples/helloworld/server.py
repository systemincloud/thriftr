#! /usr/bin/env python

import sys, thriftpy, threading
from thriftpy.rpc import make_server
from subprocess import Popen, PIPE

hello_thrift = thriftpy.load("hello.thrift", module_name="hello_thrift")

class Hello(object):
    def say(self):
        return 'Hello World'

def serving(server):
    print("serving...")
    server.serve()

def print_subprocess(process):
    while True:
        out = process.stdout.read(1).decode("utf-8")
        if out == '' and process.poll() != None:
            break
        if out != '':
            sys.stdout.write(out)
            sys.stdout.flush()

def main():
    print("START")
    server = make_server(hello_thrift.HelloService, Hello(), '127.0.0.1', 6000)
    t1 = threading.Thread(target=serving, args = (server,))
    t1.setDaemon(True)
    t1.start()

    process = Popen(["./client.R"], stdout=PIPE)
    t2 = threading.Thread(target=print_subprocess, args = (process,))
    t2.setDaemon(True)
    t2.start()

    process.wait()

    print("END")

if __name__ == '__main__':
    main()