# https://open.kattis.com/problems/aliennumbers
# 1s
# 2.1 Easy

# d2b(n,b)
# convert decimal number n to a list of digits in base b

import sys
def d2b(n,b):
    a=[]
    while(n>0):
        r=n%b
        n=n//b
        a.insert(0,r)
    return a
i=1
for v in [l.split() for l in sys.stdin.readlines()][1::]:
    a,l,t=v
    alen=len(a)
    base_l=len(l)
    base_t=len(t)
    ax=[l.index(x) for x in a]
    ad=sum([x*base_l**(alen-i-1) for (x,i) in zip(ax,range(alen))])
    v=[t[i] for i in d2b(ad,len(t))]
    print(f"Case #{i}: {''.join(v)}")
    i+=1


"""
# Compressed 356 chars. Too long for Top 10 (197..282 chars)
import sys
def d2b(n,b):
 a=[]
 while(n>0):r=n%b;n=n//b;a.insert(0,r)
 return a
i=1
for v in [l.split() for l in sys.stdin.readlines()][1::]:
 a,l,t=v;alen=len(a);base_l=len(l); base_t=len(t);ax=[l.index(x) for x in a]
 ad=sum([x*base_l**(alen-i-1) for (x,i) in zip(ax,range(alen))])
 v=[t[i] for i in d2b(ad,len(t))];print(f"Case #{i}: {''.join(v)}");i+=1
"""
