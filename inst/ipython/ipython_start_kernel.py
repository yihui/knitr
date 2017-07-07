#!/usr/bin/env python

from argparse import ArgumentParser
from jupyter_client import KernelManager
from time import sleep
import os
import sys

def parse_args(args):
    parser = ArgumentParser(description='Python kernel quick starter')

    parser.add_argument('output',
                        type=str,
                        nargs='?',
                        default=None,
                        help='')

    return parser.parse_args(args)

if __name__ == '__main__':
    config = parse_args(sys.argv[1:])

    km = KernelManager()
    km.start_kernel()

    km.write_connection_file()
    if config.output is not None:
        if not os.path.exists(os.path.dirname(config.output)):
            os.makedirs(os.path.dirname(config.output))
        with open(config.output, "w") as f:
            f.write(km.connection_file + '\n')
    else:
        print 'Kernel connection file: ' + km.connection_file

    kc = km.client()
    kc.execute("%matplotlib inline")

	# terminate kernel with 'ipython_exec quit'
    while(km.is_alive()):
        sleep(1)

    os.remove(km.connection_file)
    exit()
