# https://open.kattis.com/problems/espresso
# 1s
# 2.1 Easy

import sys;d=sys.stdin.readlines();t=int(d[0].split()[1]);f=0;k=0
for v in d[1::]:
 n=int(v.split()[0][0])+1 if len(v)>2 else int(v)
 if k+n>t:f+=1;k=n
 else:k+=n 
print(f)


"""
# 174 chars: Top 10 place 3
import sys;d=sys.stdin.readlines();t=int(d[0].split()[1]);f=0;k=0
for v in d[1::]:
 if len(v)>2:n=int(v.split()[0][0])+1
 else:n=int(v)
 if k+n>t:f+=1;k=n
 else:k+=n
print(f)
"""
