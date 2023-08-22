# https://open.kattis.com/problems/npuzzle
# 1s
# 2.1 Easy

# 193 chars: Top 10 place 2!
def p(a):return(a//4,a%4)
import sys;i=0;t=0
for c in [c for c in"".join(sys.stdin.readlines()) if c!="\n"]:
 if c!='.':(cr,cc)=p(ord(c)-65);(ir,ic)=p(i);t+=abs(cr-ir)+abs(cc-ic)
 i+=1
print(t)
