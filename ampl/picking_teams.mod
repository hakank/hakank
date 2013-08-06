/*

  Picking teams in AMPL+CP.

  This model was inspired by David Curran's
  blog post "The Fairest Way to Pick a Team "
  http://liveatthewitchtrials.blogspot.se/2012/06/fairest-way-to-pick-team.html
  """
  What is the best way to pick a team? As kids we would always strictly alternate 
  between teams so team 1 had first team 2 the second pick and then team 1 again etc.
  
  Most things you can measure about people are on a bell curve. A small number of 
  people are bad, most are in the middle and a few are good. There are a few good 
  known metrics of ability. None are perfect, there is no one number that can sum up 
  ability. The simpler the sport the more one metric can tell you, in cycling VO2 max is 
  a very good indicator. Whereas in soccer VO2 max, kicking speed, vertical leap, number 
  of keep me ups you can do etc could all measure some part of football ability.
  
  So say there was one good metric for a task and teams were picked based on this. 
  Is the standard strict alteration, where Team 1 picks then Team 2 alternating, fair? 
  Fair here meaning both teams end up with a similar quality. 
  """
  
  For n = 10, where s = 1..n there are 20 optimal solutions with a diff of 1
  (with the symmetry breaking that x[1] is in team 1).
  
  Example:
   
  x: [1, 2, 2, 2, 1, 1, 1, 1, 2, 2]
  diff: 1
  team1: [1, 5, 6, 7, 8]  sum: 27
  team2: [2, 3, 4, 9, 10]  sum: 28
  ----------
  x: [1, 2, 2, 2, 1, 1, 1, 2, 1, 2]
  diff: 1
  team1: [1, 5, 6, 7, 9]  sum: 28
  team2: [2, 3, 4, 8, 10]  sum: 27


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param n2 := n div 2;
param s{1..n};
param ssum := sum{i in 1..n} s[i];


# decision variables

# the assignments
var x{1..n} >= 1 <= 2 integer;
# the different in strengths between the teams (to minimize)
var d >= 0 <= ssum div 2 integer;

minimize obj: d;

check: n mod 2 = 0;

#
# constraints
#
# the difference in strength between the teams
s.t. c1:  d = abs(
                 (sum{i in 1..n} ((if x[i] = 1 then 1)*s[i]))
                 - 
                 (sum{i in 1..n} ((if x[i] = 2 then 1)*s[i]))
                 )
;

# same size of team
s.t. c2:  n2 = count{i in 1..n} (x[i] = 1);

# symmetry breaking: assign first person to team 1
s.t. c3: x[1] = 1;

# divisibility of the sum
s.t. c4: d mod 2 = (sum{i in 1..n} s[i]) mod 2;


data;

param n := 100;

param s :=
1 35
2 52
3 17
4 26
5 90
6 55
7 57
8 54
9 41
10 9
11 75
12 24
13 17
14 23
15 62
16 74
17 100
18 67
19 40
20 48
21 7
22 6
23 44
24 19
25 16
26 14
27 2
28 66
29 70
30 2
31 43
32 45
33 76
34 53
35 90
36 12
37 88
38 96
39 30
40 30
41 36
42 93
43 74
44 1
45 52
46 45
47 38
48 7
49 24
50 96
51 17
52 21
53 12
54 12
55 23
56 90
57 77
58 64
59 37
60 79
61 67
62 62
63 24
64 11
65 74
66 82
67 51
68 17
69 72
70 18
71 37
72 94
73 43
74 44
75 32
76 86
77 94
78 33
79 97
80 27
81 38
82 38
83 29
84 92
85 35
86 82
87 22
88 66
89 80
90 8
91 62
92 72
93 25
94 13
95 94
96 42
97 51
98 31
99 69
100 66
;


option presolve 0;
# option show_stats 2;

option solver gecode;
# option gecode_options "var_branching=degree_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;

# display x;
display d;


for{team in 1..2} {
  printf "Team %d: ", team;
  for{j in 1..n} {
     if x[j] = team then
       printf "%2d ", j;
  }
  printf " (sum %d)\n", sum{j in 1..n} (if x[j] = team then 1)*s[j];
}
printf "\n";