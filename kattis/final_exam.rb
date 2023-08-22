# https://open.kattis.com/problems/finalexam2
# 1s
# 1.7 Easy


# 57 chars Top 10 place 2 (Thomas Feld's Ruby program is 28 chars!)
x=readlines();c=i=0;x.each{|v|c+=1 if v==x[i-1];i+=1};p c

# 60 chars Top 10 place 5
# p readlines()[1..].each_cons(2).select{|i|i[0]==i[1]}.length

# 63 chars
# n=gets.to_i;x=gets;c=0
# (n-1).times{c+=1 if x==(v=gets);x=v}
# p c
