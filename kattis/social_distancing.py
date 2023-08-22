# https://open.kattis.com/problems/socialdistancing2
# 1s
# 2.2 Easy

# 128 chars: Top 10 Place 3 (shortest Python3 solution!). Thomas Feld has a Ruby solution of 60 chars!
n=int(input().split()[0])
s=list(map(int,input().split()));s+=[s[0]+n]
print(sum([(s[i+1]-s[i]-2)//2 for i in range(len(s)-1)]))


