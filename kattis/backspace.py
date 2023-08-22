# https://open.kattis.com/problems/backspace
# 1s
# 2.2 Easy

# It's interesting that SWI-Prolog is faster than Python3 on this: 0.38s

# Check if it's faster to work backwards instead
# Nope: Still TLE on 4/25
v="";i=0
for c in input()[::-1]:
 if c=='<':i+=1
 else:
  if i==0:v+=c
  else:i-=1
print(v[::-1])


# Unfortunately this does not work...
# print(input().replace("<",'\b'))

"""
# Time limit exceeded 4/25!
# 66 chars
v=""
for c in input():
 v=v[1::] if c=="<" else c+v
print(v[::-1])
"""

"""
# Time limit exceeded 4/25!
# 69 chars
v=""
for c in input():
 if c=='<':v=v[1::]
 else:v=c+v
print(v[::-1])
"""

"""
# Time Limit Exceeded 4/25!
# 92 chars
v=[]
for c in input():
 if c=="<":v.pop(0)
 else:v.insert(0,c)
v.reverse()
print("".join(v))
"""
