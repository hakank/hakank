# https://open.kattis.com/problems/socialrunning
# 1s
# 2.2 Easy
 
# 71 chars Top 10 still place 2
p ($<.read.split.map(&:to_i)[1..]*2).each_cons(3).map{|i|i[0]+i[2]}.min


# 76 chars Top 10 place 2 (Thomas Feld's Ruby solution is 47 chars!)
# x=$<.read.split.map(&:to_i)[1..]*2
# p (0..x.length-3).map{|i|x[i]+x[i+2]}.min

# 78 chars
# x=$<.read.split.map(&:to_i)[1..]*2
# p x.each_cons(3).map{|i|i.first+i.last}.min

