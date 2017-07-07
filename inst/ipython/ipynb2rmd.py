#!/usr/bin/env python

from argparse import ArgumentParser
import sys
import subprocess
import re
import os

def parse_args(args):
    parser = ArgumentParser(description='Jupyter to markdown notebook converter')

    parser.add_argument('notebook',
                        type=str,
                        nargs=1,
                        help='Jupyter notebook')

    return parser.parse_args(args)

if __name__ == '__main__':
    config = parse_args(sys.argv[1:])

    nb  = config.notebook[0]
    md  = re.sub("\\.ipynb$", ".md", nb)
    Rmd = re.sub("\\.ipynb$", ".Rmd", nb)

    subprocess.call(["jupyter", "nbconvert", nb, "--to", "markdown",
                     "--ClearOutputPreprocessor.enabled=True"])

    print "[JupyterToRmd] Writing to %s" % Rmd
    subprocess.call(["sed", "-i", 's/```python/```{ipython}/', md])
    os.rename(md, Rmd)