# https://open.kattis.com/problems/pauleigon
# 1s
# 1.7 Easy

# 74 chars: Top 10 was at place 3 (but my Ruby program is now shorter: 67 chars)
n,p,q=map(int,input().split(" "));print(["paul","opponent"][((p+q)//n)%2])

# 76 chars: Top 10 (place 5) (63..81)
# [n,p,q]=map(int,input().split(" "));print(["paul","opponent"][((p+q)//n)%2])
