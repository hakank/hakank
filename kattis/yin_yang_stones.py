# https://open.kattis.com/problems/yinyangstones
# 1s
# 1.7 Easy

# ChatGPT4: 47 chars. Not committed
# x=input()
# print(1*(x.count("W")==x.count("B")))

# 50 chars Top 10 place 8 (before my Ruby program was committed)
x=[*input()]
print(1*(x.count("W")==x.count("B")))

# 53 chars
# x=[*input()]
# print([0,1][x.count("W")==x.count("B")])


# 60 chars
# x=[*input()]
# print(1 if x.count("W")==x.count("B") else 0)

