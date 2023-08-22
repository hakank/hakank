% https://open.kattis.com/problems/theplank
% 1s
% 1.8 Easy

/*
  CP approach in Picat. Also see the_plank.pi for some more excursions
Picat> L=findall(A,( member(N,1..4),X=new_list(N),X::1..3,sum(X)#=4,solve_all(X)=A)) 
L = [[[1,3],[2,2],[3,1]],[[1,1,2],[1,2,1],[2,1,1]],[[1,1,1,1]]]

Picat> member(P,1..24),println(P=findall(A,( member(N,1..P),X=new_list(N),X::1..3,sum(X)#=P,solve_all(X).len=A))),fail 
1 = [1]
2 = [1,1]
3 = [1,2,1]
4 = [3,3,1]
5 = [2,6,4,1]
6 = [1,7,10,5,1]
7 = [6,16,15,6,1]
8 = [3,19,30,21,7,1]
9 = [1,16,45,50,28,8,1]
10 = [10,51,90,77,36,9,1]
11 = [4,45,126,161,112,45,10,1]
12 = [1,30,141,266,266,156,55,11,1]
13 = [15,126,357,504,414,210,66,12,1]
14 = [5,90,393,784,882,615,275,78,13,1]
15 = [1,50,357,1016,1554,1452,880,352,91,14,1]
16 = [21,266,1107,2304,2850,2277,1221,442,105,15,1]
17 = [6,161,1016,2907,4740,4917,3432,1651,546,120,16,1]
18 = [1,77,784,3139,6765,9042,8074,5005,2184,665,136,17,1]
19 = [28,504,2907,8350,14355,16236,12727,7098,2835,800,153,18,1]
20 = [7,266,2304,8953,19855,28314,27742,19383,9828,3620,952,171,19,1]
21 = [1,112,1554,8350,24068,43252,52624,45474,28665,13328,4556,1122,190,20,1]
22 = [36,882,6765,25653,58278,87802,93093,71955,41328,17748,5661,1311,210,21,1]
23 = [8,414,4740,24068,69576,129844,168168,157950,110448,58276,23256,6954,1520,231,22,1]
24 = [1,156,2850,19855,73789,171106,270270,306735,258570,165104,80580,30039,8455,1750,253,23,1]

Picat> member(P,1..24),println(P=findall(A,( member(N,1..P),X=new_list(N),X::1..3,sum(X)#=P,solve_all(X).len=A)).sum),fail
1 = 1
2 = 2
3 = 4
4 = 7
5 = 13
6 = 24
7 = 44
8 = 81
9 = 149
10 = 274
11 = 504
12 = 927
13 = 1705
14 = 3136
15 = 5768
16 = 10609
17 = 19513
18 = 35890
19 = 66012
20 = 121415
21 = 223317
22 = 410744
23 = 755476
24 = 1389537

It's http://oeis.org/A000073
"Tribonacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3) for n >= 3 with a(0) = a(1) = 0 and a(2) = 1."

But this CP approach is way too hard for SWI-Prolog:
test_cp :-
    N = 14,
    writeln(N),
    findall(X,(between(1,N,M),length(X,M),X ins 1..3,sum(X,#=,N),labeling([ff,bisect],X)),Ls),
    writeln(Ls),
    length(Ls,Len),
    writeln(len=Len),
    nl.
for N=20 it take about 2.5s (ff/enum).

The (adjusted) Tribonacci function a/2 works. 
Note that the initial values are 
  a(0) = 1, a(1)=1, a(2)=2
insead of 0,0,1.

*/

% :- use_module(library(clpfd)).

main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    a(N,T),
    writeln(T).

:- table a/2.
a(0,1).
a(1,1).
a(2,2).
a(N,T) :-
    N >= 3,
    N1 is N-1,N2 is N-2,N3 is N-3,
    a(N1,T1),a(N2,T2),a(N3,T3),
    T is T1+T2+T3.
