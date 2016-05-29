#! /usr/bin/env python

import sys, importlib, thriftpy, threading
from thriftpy.rpc import make_server
from subprocess import Popen, PIPE

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
    print("PY:START")
    directory = sys.argv[1]
    service_thrift = thriftpy.load(directory + "/service.thrift", module_name="service_thrift")
    #exec(open(directory + "/server.py").read())
    #__import__(name="Server", fromlist=[directory + '.server'])
    mod = importlib.import_module(directory + '.server')
    server = make_server(service_thrift.Service, mod.Server(), '127.0.0.1', 6000)
    t1 = threading.Thread(target=serving, args = (server,))
    t1.setDaemon(True)
    t1.start()

    process = Popen([directory + "/client.R"], stdout=PIPE)
    t2 = threading.Thread(target=print_subprocess, args = (process,))
    t2.setDaemon(True)
    t2.start()

    process.wait()

    print("PY:END")

if __name__ == '__main__':
    main()