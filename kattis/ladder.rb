# https://open.kattis.com/problems/ladder
# 1s
# 1.6 Easy

# irb(main):062:0> Math::PI/180
# => 0.017453292519943295

# 60 chars
# I don't want to test with a shorter version since it will kick out the Prolog program...
l,d=gets.split.map(&:to_f)
p (l/Math.sin(d*0.01745329)).ceil

# 64 chars Top 10 for Ruby programs place 3 (my Prolog program is Top 10 All place 4)
# l,d=gets.split.map(&:to_f)
# p (l/Math.sin(d*Math::PI/180)).ceil

