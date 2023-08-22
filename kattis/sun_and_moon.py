# https://open.kattis.com/problems/sunandmoon
# 1s
# 1.7 Easy


# 122 chars: Top 10 place 5
import sys;a,b,c,d=map(int,"".join(sys.stdin.readlines()).split());y=0
while 1:
 if(y+a)%b+(y+c)%d==0:print(y);break
 y+=1

"""
# A different approach of reading the numbers: 123 chars
def f():return list(map(int,input().split()))
a,b=f();c,d=f();y=0
while 1:
 if(y+a)%b==0 and(y+c)%d==0:print(y);break
 y+=1
"""

"""
# 128 chars: Top 10 place 6
import sys;a,b,c,d=map(int,"".join(sys.stdin.readlines()).split());y=0
while 1:
 if(y+a)%b==0 and(y+c)%d==0:print(y);break
 y+=1
"""

"""
# 140 chars. Top 10 place 8 (and removing my place 8 for the Prolog program)
import sys;a,b,c,d=[int(i)for i in"".join(sys.stdin.readlines()).split()];y=0
while 1:
 if(y+a)%b==0 and(y+c)%d==0:print(y);break
 y+=1
"""
