# https://open.kattis.com/problems/knotknowledge
# 1s
# 1.4 Easy

# 80 chars
_,l,k=[map(int,input().split())for _ in range(3)]
print(list(set(l)-set(k))[0])

"""
# 96 chars
f->map(int,input().split())
_,l,k=f(),f(),f()
print([q for q in l if not q in k][0])
"""

"""
# 101 chars: Much to large for Top 10 (29..62 chars)
def f():return map(int,input().split())
input();l,k=f(),f()
for q in l:
 if not q in k:print(q);break
"""
