# https://open.kattis.com/problems/magictrick
# 1s
# 1.4 Easy

# 41 chars
print("01"[len(x:=input())==len(set(x))])


# 42 chars
# x=input()
# print("01"[len(x)==len(set(x))])


# 48 chars not short enough for Top 10 (16..39 chars)
# x=input()
# print(1 if len(x)==len(set(x)) else 0)
