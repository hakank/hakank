# https://open.kattis.com/problems/socialrunning
# 1s
# 2.2 Easy

# 108 chars: Top 10 place 9 (before my Ruby program at place 2)
import sys;s=[int(i)for i in sys.stdin.readlines()][1::]*2
print(min([s[i]+s[i+2]for i in range(len(s)-2)]))

