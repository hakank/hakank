# https://open.kattis.com/problems/finalexam2
# 1s
# 1.7 Easy

# 88 chars
n=int(input());a=input();s=0
for i in range(n-1):
 v=input()
 if v==a:s+=1
 a=v
print(s)


"""
# 90 chars
n=int(input())
v=[input() for _ in range(n)]
print(sum([v[i]==v[i-1]for i in range(1,n)]))
"""

