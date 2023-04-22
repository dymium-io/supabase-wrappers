#!/usr/bin/python3
import sys
import time
def streamfile(fname):
    with open(fname, 'r') as fin:
        Lines = fin.readlines()
        print(f'NLines:{len(Lines)}:')
        count = 0
        # Strips the newline character
        for line in Lines:
            count += 1
            print(f'line:{count}:{line}')
            time.sleep(3/1000)

if __name__ == '__main__':
    streamfile(sys.argv[1])
    print("Done:")

