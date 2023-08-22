# https://open.kattis.com/problems/communication
# 2s
# 2.1 Easy

# 134 chars: Should be in place 10 in Top 10, but perhaps they
# don't place the same person twice in the top list
# (my Prolog program is in place 5 with 123 chars).
# And it's not in Top 10 for Python3 either, should be
# in place 8.
# 
import sys
for x in[int(i)for i in sys.stdin.readlines()[1::][0].split()]:
 for i in range(256):
  if x==i^(i<<1)%256:print(i,end=" ")
