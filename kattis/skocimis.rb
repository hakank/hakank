# https://open.kattis.com/problems/skocimis
# 1s
# 1.6 Easy

# 46 chars: This would have placed me in Top 10 place 2, but I want my Prolog program (53 chars place 8) to be in the list..
a,b,c=gets.split.map(&:to_i)
p [b-a,c-b].max-1


