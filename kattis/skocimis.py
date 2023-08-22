# https://open.kattis.com/problems/skocimis
# 1s
# 1.6 Easy

# 52 chars: It would have placed it as place 8 in Top 10, my Prolog program (52 chars) is there already
a,b,c=map(int,input().split())
print(max(b-a,c-b)-1)


