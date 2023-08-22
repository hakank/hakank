# https://open.kattis.com/problems/averagecharacter
# 1s 2048MB
# 1.9 Easy

# 45 chars Top 10 place 9  (in the Ruby Top 10 place 2, but there are only 3 entries)
x=$<.read[..-2]
$><<(x.sum/(x=~/$/)).to_i.chr

# 47 chars Top 10 place 10
# x=$<.read[0..-2]
# puts (x.sum/x.length).to_i.chr

