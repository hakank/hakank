# https://open.kattis.com/problems/haypoints
# 1s
# 1.9 Easy


# 227 chars (Top 10, place 4)
import sys
d=sys.stdin.readlines()
m,n=[int(i)for i in d[0].split()];wds={};
for ws in d[1:m+1]:w,s=ws.split();wds[w]=int(s)
js=[t.split() for t in "".join(d[m+1:-1]).split(".")]
for j in js:print(sum([wds.get(b,0)for b in j]))


"""
# Python: 291 chars
import sys
d = sys.stdin.readlines();len = len(d)
m,n = [int(i) for i in d[0].split(" ")]
words0 = [ ws.split() for ws in d[1:m+1] ]
words = { w:int(s) for [w,s] in words0 }
jobs = [t.split() for t in "".join(d[m+1:-1]).split(".")]
for job in jobs: print(sum([words.get(j,0) for j in job]))
"""
