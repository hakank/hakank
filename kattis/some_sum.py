# https://open.kattis.com/problems/somesum
# 1s
# 2.0 Easy

# 56 chars: Top 10 place 6
e="Either";x=["Even",e,"Odd",e];print(x[int(input())%4])

# 59 chars: Top 10 (place 9)
# x=["Even","Either","Odd","Either"];print(x[int(input())%4])

# 75 char (not short enough for Top 10 (which currently has length 43..60)
# n=int(input());m=n%4;print("Even" if m==0 else ("Odd" if m==2 else "Even"))

# Original: 82 chars
"""
n=int(input());m=n%4;
if m==0:t="Even"
elif m==2:t="Odd"
else:t="Either"
print(t)
"""
