# https://open.kattis.com/problems/password
# 1s
# 2.1 Easy

# 134 chars Place 10 at Top 10
import sys;s=[float(l.split()[1])for l in sys.stdin.readlines()[1::]];s=sorted(s,reverse=True);i=1;t=0
for p in s:t+=p*i;i+=1
print(t)
