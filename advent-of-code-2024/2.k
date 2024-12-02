\l u.k
x:.'0:"2.txt"               / read file
safe1:{0=+/0=(x>0)&(x<4)}   / between 1..3 safe differences
safe2:{(x~x[<x])|(x~x[>x])} / the line is increasing or decreasing

/ combine safe 1 and safe 2
safe:{$[safe1 diff sort x;(safe2 x);0]}

/ Part 1
+/safe'x

/ Part 2
+/{0<+/safe'(x_)'!(#x)}'x / remove one element and check if any is safe
