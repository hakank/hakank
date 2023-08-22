# https://open.kattis.com/problems/pauleigon
# 1s
# 1.7 Easy

n,r,q=gets.split.map(&:to_i)
$><<%w{paul opponent}[(r+q).div(n)%2]

# 67 chars. Top 10 place 2
# n,r,q=gets.split.map(&:to_i)
# puts %w{paul opponent}[(r+q).div(n)%2]


# 69 chars. Top 10 place 3
# Note the &:to_i and it should be map() not map{}
# n,r,q=gets.split.map(&:to_i)
# puts ['paul','opponent'][(r+q).div(n)%2]

# 77 chars. 
# n,r,q=gets.split(" ").map{|v|v.to_i}
# puts ['paul','opponent'][(r+q).div(n)%2]
