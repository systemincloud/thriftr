#! /usr/bin/env python

import sys, importlib, thriftpy, threading
from thriftpy.rpc import make_server
from subprocess import Popen, PIPE

PORT = 6000

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

def main(argv):
    print("PY:START")
    directory = argv[0]
    service_thrift = thriftpy.load(directory + "/service.thrift", module_name="service_thrift")
    mod = importlib.import_module(directory + '.server')
    server = make_server(service_thrift.Service, mod.Server(), '127.0.0.1', PORT)
    t1 = threading.Thread(target=serving, args = (server,))
    t1.setDaemon(True)
    t1.start()

    process = Popen([directory + "/client.R", str(PORT)], stdout=PIPE)
    t2 = threading.Thread(target=print_subprocess, args = (process,))
    t2.setDaemon(True)
    t2.start()

    process.wait()

    print("PY:END")

if __name__ == '__main__':
    main(sys.argv[1:])