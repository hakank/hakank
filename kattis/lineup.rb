# https://open.kattis.com/problems/lineup
# 1s
# 1.8 Easy

# Generated by ChatGPT4. Trial 1: 98 chars
# a=$<.map(&:to_i)[1..]
# p=a.sort;q=p.reverse
# puts a==p ? :INCREASING : a==q ? :DECREASING : :NEITHER

# Generated by ChatGPT4. Trial 2: 94 chars. Does it work?
# Nope, parenthesis is needed aro a==p=a.sort
# a=$<.map(&:to_i)[1..-1]
# puts a==p=a.sort ? :INCREASING : a==p.reverse ? :DECREASING : :NEITHER

# I complained about the parenthesis around the second condition
# a=$<.map(&:to_i)[1..-1]
# puts (a==p=a.sort) ? :INCREASING : a==p.reverse ? :DECREASING : :NEITHER

# This is now 98 chars. And it's not correct: it outputs INCREASING on all the inputs.
# a=$<.map(&:to_i)[1..-1]
# puts (a==p=a.sort) ? :INCREASING : (a==p.reverse) ? :DECREASING : :NEITHER

# I now gave it the three examples with the proper output.
# Next trial: 89 chars. Does it work. Yes, it works for the three inputs.
# I will not submit this. The following are to remember:
# - use drop(1) to skip the first line
# - use (a==p=a.sort) to assign and check for equality. Note that the parenthesis are needed.
# a=$<.drop(1)
# puts (a==p=a.sort) ? :INCREASING : (a==q=p.reverse) ? :DECREASING : :NEITHER

# I'm now testing (without the help of GPT4:-)) to use strings instead of symbols.
# Shorter and seems to work: 84 chars.
# And 84 chars is also the size of Top 10 place 1 (by Thomas Feld)
# I will not submit this...
# a=$<.drop(1)
# puts (a==p=a.sort)?"INCREASING":(a==q=p.reverse)?"DECREASING":"NEITHER"



# 93 chars Not committed: Would be in Top 10 place 2 but I keep my Prolog prog at place 8
a=readlines[1..]
p=a.sort;q=p.reverse
puts (a==p)?:INCREASING : (a==q)?:DECREASING : :NEITHER


# Python:
# a=sys.stdin.readlines()[1::]
# s1=sorted(a);s2=list(reversed(s1))
# if a==s1:t="INCREASING"
# elif a==s2:t="DECREASING"
# else:t="NEITHER"
# print(t)