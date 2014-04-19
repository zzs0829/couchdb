import os
import shutil
import sys

def rm_rf(path):
    if os.path.isdir(path):
        shutil.rmtree(path)
    elif os.path.exists(path):
        os.remove(path)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        [rm_rf(f) for f in sys.argv[1:]]

