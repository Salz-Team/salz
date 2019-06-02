import  curses
from curses import wrapper
import curses.textpad
import time
import sys

# globals cuz im a bad programmer


if len(sys.argv) < 2:
    print("Need to enter a filename")
else:
    fname = sys.argv[1]

def main(stdscr):

    stdscr.clear()

    begin_x = 20
    begin_y = 7

    height = 30
    width = 40

    win = curses.newwin(height, width, begin_y, begin_x)

    # stdscr.addstr(4,1,text.encode('utf_8'))

    # Draw to screen

    with open(fname, 'r') as f:
        width, height = list(map(int, f.readline().split(' ')))
        p1, p2 = f.readline().split(' ')

        l = f.readline()

        while l:
            # blank screen
            for posx in range(width):
                for posy in range(height):
                    stdscr.addstr(posy, posx, "█")

            stdscr.refresh()
            coords = [] if not l else list(map(int, l.split(' ')))
            zipped = []

            for i in range(0, len(coords) - 2, 2):
                posx = coords[i]
                posy = coords[i+1]
                stdscr.addstr(posy, posx, " ")

            l = f.readline()
            stdscr.refresh()
            stdscr.getkey()







wrapper(main)
