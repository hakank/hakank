# https://open.kattis.com/problems/pizza2
# 1s
# 1.7 Easy

# 43 chars Would be on Top 10 place 3 but I will keep my Prolog program there instead (47 chars)
# So this is not submitted...
r,c=gets.split.map(&:to_f)
p 100*(1-c/r)**2
