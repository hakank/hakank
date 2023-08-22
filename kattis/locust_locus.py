# https://open.kattis.com/problems/locustlocus
# 1s
# 2.0 Easy

# 163 chars (much longer than Top 10: 46 (Ruby), 121.. 122 Python3)
# This was not submitted...
"""
import sys,math
def lcm(a,b):return(a/math.gcd(a,b))*b
print(min([int(y+lcm(c1,c2))for(y,c1,c2)in[[int(i)for i in x.split()]for x in sys.stdin.readlines()][1::]]))
"""

"""
# Here is a shorter version that I found somewhere: 121 chars
from math import gcd
print(min((lambda a,b,c: a+b*c//gcd(b,c))(*map(int, input().split())) for _ in range(int(input()))))

# And I managed to squeeze a little more: 114 chars
import math
print(min((lambda a,b,c:a+b*c//math.gcd(b,c))(*map(int,input().split()))for _ in range(int(input()))))
"""
