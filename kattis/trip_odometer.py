# https://open.kattis.com/problems/tripodometer
# 1s
# 2.0 Easy

# 125 chars Top 10 place 3
input()
s=sorted(map(int,input().split()))
m=sum(s)
t=sorted(set([m-v for v in s]))
print(len(t))
for x in t:print(x,end=" ")
