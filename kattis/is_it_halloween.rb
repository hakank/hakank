# https://open.kattis.com/problems/isithalloween
# 1s
# 1.5 Easy

# 38 chars. Top 10 place 4
puts gets=~/OCT 31|DEC 25/?:yup: :nope

# 39 chars: $><< requires () here
# $><<(gets=~/OCT 31|DEC 25/?:yup: :nope)

# 39 chars
# puts gets=~/OCT 31|DEC 25/?"yup":"nope"

# 40 chars
# puts [:yup,:nope][gets=~/OCT 31|DEC 25/]

# 45 chars Top 10 place 5
# puts gets.match(/OCT 31|DEC 25/)?"yup":"nope"

# 64 chars: Too long for Top 10 (33..53 chars, and my Python3 program is place 7
# $><<["nope","yup"][["OCT 31","DEC 25"].include?(gets.strip)?1:0]

