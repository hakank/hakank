# https://open.kattis.com/problems/pizza2
# 1s
# 1.7 Easy

# 50 chars Would be on Top 10 place 5 but my Prolog program is in place 3.
r,c=map(int,input().split())
print(100*(1-c/r)**2)


# 52 chars
# r,c=map(int,input().split())
# print(100*((r-c)/r)**2)

# 55 chars
# r,c=map(int,input().split())
# print(100*((r-c)**2/r**2))

