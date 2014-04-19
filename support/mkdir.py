import os
import sys

if __name__ == "__main__":
    if len(sys.argv) == 1:
        os.makedirs(sys.argv[1].replace('/','\\'))


