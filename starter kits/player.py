import sys

def getInts():
  mystr = input()
  if mystr:
    return list(map(int,mystr.split(" ")))
  else:
    return []

name = "CoolPlayer"

##Map Height
##Map Width
h, w = getInts()

print (name)

def createGlider(start, current):
    if current == start:
      return [2,3,4,5,6]
    if current == start + 1:
      return[3,4,5,6]
    if current == start + 2:
      return[3,4]
    return []

##Game loop
turn = 0
while True:
    turn = turn + 1

    ##Read in new Map
    if turn != 1:
      gameMap = getInts()

    ##Turn Commands
    turncommands = []

    turncommands = turncommands+ createGlider(1, turn)

    ##Send Turn Commands
    print(*turncommands)

