# https://open.kattis.com/problems/password
# 1s
# 2.1 Easy

"""
# From ChatGPT4: 93 chars. It's shorter and seems to work.
# I'm not committing this, though.
# x=$<.drop(1).map{|l|l.split}
# i=1;a=0;x.sort_by{|_,v|v}.reverse.map{|_,v|a+=i*v.to_f;i+=1}
# p a
#
# I'm testing a shorter variabt with map(&:split)
# This is 90 chars and seems to work. Not committed.
# x=$<.drop(1).map(&:split)
# i=1;a=0;x.sort_by{|_,v|v}.reverse.map{|_,v|a+=i*v.to_f;i+=1}
# p a
#
# I mentioned that it's possible to write a program as short as
# 52 chars but ChatGPT4 could not do that (it genereated far
# longer versions).
"""


# x=$<.read.split[1..] # Using $<.read doesn't work. Why?

# 96 chars Top 10 still place 2 (
# using map is shorter than each_with_index)
x=readlines[1..].map(&:split)
i=1;a=0;x.sort_by{|i|i[1]}.reverse.map{|v|a+=i*v[1].to_f;i+=1}
p a


# 105 chars Top 10 place 2 (Thomas Feld's Ruby program is 52 chars!)
# x=readlines[1..].map(&:split)
# a=0;x.sort_by{|i|i[1]}.reverse.each_with_index{|v,i|a+=(i+1)*v[1].to_f}
# p a
