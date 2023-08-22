# https://open.kattis.com/problems/ladder
# 1s
# 1.6 Easy

# 76 chars Top 10 for Python3 place 2 (my Prolog program is Top 10 overall place 4)
from math import *
l,d=map(int,input().split())
print(ceil(l/sin(d*pi/180)))

# 84 chars
# import math
# l,d=map(int,input().split())
# print(math.ceil(l/math.sin(d*math.pi/180)))
