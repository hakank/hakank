# https://open.kattis.com/problems/nothanks
# 1s
# 2.0 Easy

# Shorter: 90 chars: Top 10 place 4.
input()
f,*s=sorted(map(int,input().split()));p=f
for i in s:
 if i>p+1:f+=i
 p=i
print(f)

# Without readlines: 98 chars: Should still be place 4 but it's not there...
"""
input()
f,*s=sorted([int(i)for i in input().split()]);p=f
for i in s:
 if i>p+1:f+=i
 p=i
print(f)
"""

# Faster than the Prolog version (0.08s vs 0.44s)
# And shorter: 118 chars. Should be in Top 10 place 4 but I cannot see it...
"""
import sys
f,*s=sorted([int(i)for i in sys.stdin.readlines()[1].split()]);p=f
for i in s:
 if i>p+1:f+=i
 p=i
print(f)
"""

