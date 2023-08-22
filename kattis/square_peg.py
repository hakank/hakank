# https://open.kattis.com/problems/squarepeg
# 1s
# 1.7 Easy

# In [21]: 2**(1/2)
# Out[21]: 1.4142135623730951


# 63 chars
l,r=map(int,input().split())
print(["nope","fits"][l<=r*1.414])

# 66 chars
# l,r=map(int,input().split())
# print(["nope","fits"][l<=r*2**(1/2)])
                         
