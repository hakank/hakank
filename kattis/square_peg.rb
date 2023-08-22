# https://open.kattis.com/problems/squarepeg
# 1s
# 1.7 Easy

# 2**(1/2)
# 1.4142135623730951

# 57 chars Would have been in Top 10 place 5 but I will not disturb place 8 for the Prolog program
l,r=gets.split.map(&:to_f)
puts (l<=r*1.414)?:fits: :nope
