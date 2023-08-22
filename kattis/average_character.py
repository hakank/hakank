# https://open.kattis.com/problems/averagecharacter
# 1s 2048MB
# 1.9 Easy

# 55 chars
x=list(map(ord,input()))
print(chr(int(sum(x)/len(x))))


# 58 chars: Not short enough for Top 10 (32..48 chars)
# x=[ord(c)for c in input()]
# print(chr(int(sum(x)/len(x))))
