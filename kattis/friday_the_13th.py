# https://open.kattis.com/problems/friday
# 1s
# 2.0 Easy

# It's correct
"""
import sys
v=sys.stdin.readlines()[1::]
for i in range(len(v)//2):
    s = [int(x) for x in v[2*i+1].split()]
    l=len(s)
    f = 0
    d = 0
    for m in s:
        if m >= 13:
            if (d + 13-1) % 7 == 5:
                f+=1
        d = (d+m)%7
    print(f)
"""

# And now let's golf it: 179 chars. Top 10, place 2 (it should be place 3)
import sys
v=sys.stdin.readlines()[1::]
for i in range(len(v)//2):
 s=[int(x)for x in v[2*i+1].split()];f=0;d=0
 for m in s:
  if m>=13 and(d+13-1)%7==5:f+=1
  d=(d+m)%7
 print(f)
