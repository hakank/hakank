/*

A Nimatron
Martin J.Chlond

http://ite.pubs.informs.org/Vol3No3/ChlondAkyol/


Identify Winning move in Rosebushes - Xpress-Mosel model

! Rosebush.mos : Computes move to safe position (if available) in game of Rosebushes
! Written by : Martin J. Chlond
! Date written : 6 January 2002
! References : Vajda, S., Mathematical Games and how to play them (pp 59,59)
! Schuh, F., The Master Book of Mathematical Recreations (*109 pp 141,142)
! Berlekamp, E.R., Conway, J.H., Guy, R.K., Winning Ways for your Mathematical Plays


Output:
:   d   m    :=
1   0   1
2   0   1
3   1   1
4   1   2
5   0   4
;

s = 28

Cf nim.mod 


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param heap = 5; # number of heaps
param nmax = 16; # maximum number allowed in any heap
param k = 2; # maximum number of heaps to change


# data tables
param n{1..heap};
param z{1..4};

# variables
var d{1..heap} binary; # 1 if heap changed, 0 otherwise
var m{1..heap} integer >= 0; # number in each heap after move
var e{1..heap} binary; # 1 if heap odd after move, 0 if even
var b{1..heap} integer >= 0; # dummy variable for parity check of heaps
var w{1..4,1..3} binary; # dummy variables for safety check
var s integer>=0; # decimal equivalent of heap parities


# minimise number of heaps changed - if solution is zero then current position already safe
minimize heapch: sum{i in 1..heap} d[i];

# maximum of k rows to change
subject to maxh: sum{i in 1..heap} d[i] <= k;

# ensures safe position after move
subject to lca{i in 1..4}: s - ( 32 - z[i] ) * w[i,1] <= z[i] - 1;
subject to lcb{i in 1..4}: s-z[i]*w[i,1] >= 0;
subject to lcc{i in 1..4}: s+(z[i]+1)*w[i,2] >= z[i]+1;
subject to lcd{i in 1..4}: s+31*w[i,2] <= z[i]+31;
subject to lce{i in 1..4}: w[i,3] = w[i,1]+w[i,2]-1;


subject to es: sum{i in 1..4} w[i,3] = 1;

# positions before and after are consistent with move
subject to cons{i in 1..heap}:
             n[i]-d[i] = m[i];

# e[i] = 1 if m[i] odd, 0 otherwise
subject to eset{i in 1..heap}:
            m[i] = 2*b[i]+e[i];

# computes decimal equivalent of heap parities
subject to sset:
          sum{i in 1..heap} 2^(heap-i)*e[i] = s;

# ensures piles remain in increasing order 
subject to  ndec{i in 1..heap-1}:
              m[i+1] >= m[i];

# minimise number of heaps changed - if solution is zero then current position already safe
# minimise( heapch )

data;

# number in each heap before move (example from Vajda)
param n :=
        1 1
        2 1
        3 2
        4 3
        5 4
; 
# decimal equivalents of 'safe' heap parities (see Vajda)
param z:=
        1 0
        2 7
        3 19
        4 28
;

option solver cplex;

solve;

display d,m,s;
