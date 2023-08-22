# https://open.kattis.com/problems/anotherbrick
# 1s
# 2.1 Easy

# 173 chars. Top 10, still place 3
# To make it shorter, I have to rething the principal algorithm...
def f():return map(int,input().split())
h,w,_=f()
b=f()
p=0;l=0;f=1
for v in b:
 if p+v<w:p+=v
 elif p+v==w:p=0;l+=1
 else:f=0;break
r=((f==1)*(l>=h))
print(["NO","YES"][r])


"""
# Shorter: 175 chars: Top 10 Place 3
h,w,_=map(int,input().split())
b=map(int,input().split())
p=0;l=0;f=1
for v in b:
 if p+v<w:p+=v
 elif p+v==w:p=0;l+=1
 else:f=0;break
r=((f==1)*(l>=h))
print(["NO","YES"][r])
"""

"""
# Compressed: 205 chars: Top 10 Place 6
import sys;d=[list(map(int,l.split()))for l in sys.stdin.readlines()]
h,w,s=d[0];b=d[1]
p=0;l=0;f=1
for v in b:
 if p+v<w:p+=v
 elif p+v==w:p=0;l+=1
 else:f=0;break
r=((f==1)*(l>=h))
print(["NO","YES"][r])
"""


"""
# 257 chars
import sys;d=[list(map(int,l.split()))for l in sys.stdin.readlines()]
h,w,s=d[0];b=d[1]
p=0;l=0;f=1
for v in b:
    if l>=w and f==1:break
    if p+v<w:p+=v
    elif p+v==w:p=0;l+=1
    else:f=0;break
if f==1 and l>=h:
    print("YES")
else:
    print("NO")
"""

