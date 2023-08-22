# https://open.kattis.com/problems/ofugsnuid
# 1s
# 1.3-1.5 Easy


# From GPT4:
# 22 chars Top 10 place 3!
puts$<.drop(1).reverse

# 24 chars
# puts$<.to_a[1..].reverse

# 29 chars Top 10 place 9
# $><<readlines[1..].reverse*""


# 34 chars Top 10 place 10 (Thomas Feld's Ruby program is 16 chars!)
# x=readlines[1..]
# puts x.reverse*""

# From GPT4:
# 22 chars
# puts$<.drop(1).reverse
# 34 chars
# puts$<.drop(1).map(&:to_i).reverse

# 37 chars
# puts (1..gets.to_i).map{gets}.reverse
# puts (1..gets.to_i).map{gets.to_i}.reverse

