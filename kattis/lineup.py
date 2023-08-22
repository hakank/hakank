# https://open.kattis.com/problems/lineup
# 1s
# 1.8 Easy

# From ChatGPT3.5: 132 chars. Does it work? Yes. It would have been at place 4
# (but my Prolog code is shorter 130 chars, and the Ruby code is 93 chars).
# I will not submitt this solution, but will remember the tricks used.
# import sys;a=sys.stdin.readlines()[1::];print(["NEITHER","INCREASING","DECREASING"][2*(a==sorted(a))-1*(a==sorted(a,reverse=True))])

# 145 chars Would have been in Top 10 place 8 (but my Prolog program is at place 4)
# (and my not committed Ruby program with 93 chars would take place 2)
import sys
a=sys.stdin.readlines()[1::]
p=sorted(a);q=list(reversed(p))
if a==p:t="INCREASING"
elif a==q:t="DECREASING"
else:t="NEITHER"
print(t)

"""
# 150 chars Would have been Top 10 place 9 (my Prolog program is at place 8)
import sys
a=sys.stdin.readlines()[1::]
s1=sorted(a);s2=list(reversed(s1))
if a==s1:t="INCREASING"
elif a==s2:t="DECREASING"
else:t="NEITHER"
print(t)
"""

"""
# 155 chars
n=int(input())
a=[input() for _ in range(n)]
s1=sorted(a);s2=list(reversed(s1))
if a==s1:t="INCREASING"
elif a==s2:t="DECREASING"
else:t="NEITHER"
print(t)
"""

"""
# 166 chars
n=int(input());a=[]
for _ in range(n):a.append(input())
s1=sorted(a);s2=list(reversed(s1))
if a==s1:t="INCREASING"
elif a==s2:t="DECREASING"
else:t="NEITHER"
print(t)
"""
