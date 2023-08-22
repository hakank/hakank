# https://open.kattis.com/problems/piglatin
# 3s
# 2.2 Easy

# 205 chars: Top 10 place 3
import sys;v="aeiouy"
for s in[i.split()for i in sys.stdin.readlines()]:
 for w in s:
  if w[0]in v:t=w+"y"
  else:p=[i for i,c in enumerate(w)if c in v][0];t=w[p::]+w[0:p]
  print(t+"ay",end=" ")
 print()


"""
# 214 chars Top 10 place 5
import sys;v="aeiouy"
for s in[i.split()for i in sys.stdin.readlines()]:
 for w in s:
  if w[0]in v:print(w+"yay",end=" ")
  else:p=[i for i,c in enumerate(w)if c in v][0];print(w[p::]+w[0:p]+"ay",end=" ")  
 print()
"""

"""
# 260 chars
import sys
v="aeiouy"
for s in [i.split() for i in sys.stdin.readlines()]:
    for w in s:
        if w[0] in v:print(w+"yay",end=" ")
        else:
            p=[i for i,c in enumerate(w)if c in v][0]
            print(w[p::]+w[0:p]+"ay",end=" ")
    print()
"""
