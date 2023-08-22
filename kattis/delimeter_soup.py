# https://open.kattis.com/problems/delimitersoup
# 1s
# 2.1 Easy

# 205 chars: Top 10, still place 8
input();m={"}":"{",")":"(","]":"["};i=0;s=[];f=1
for c in input():
 if c==" ":pass
 elif c in"{[(":s.insert(0,c)
 else:
  if len(s)==0 or s.pop(0)!=m[c]:print(c,i);f=0;break
 i+=1
if f>0:print("ok so far")


"""
# 237 chars: Top 10 place 9
input();m={"}":"{",")":"(","]":"["};i=0;s=[];f=1
for c in input():
 if c==" ":pass
 elif c in "{[(":s.insert(0,c)
 else:
  if len(s)==0:print(c,i);f=0;break
  p=s.pop(0)
  if p!=m[c]:print(c,i);f=0;break
 i+=1
if f==1: print("ok so far")
"""

"""
input()
m={"}":"{",")":"(","]":"["}
i=0
s=[]
f=1
for c in input():
    if c==" ":pass
    elif c in "{[(":
        s.insert(0,c)
    else:
        if len(s)==0:print(c,i);f=0;break
        p=s.pop(0)
        if p!=m[c]:
            print(c,i);f=0;break
    i+=1
if f==1: print("ok so far")
"""
