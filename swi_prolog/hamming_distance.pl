/*

  Hamming distance in SWI Prolog

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

% 
% We can now either
% - Calculate the hamming distance from two arrays
% - Given the distance, generate all arrays which has the 
%   Hamming distance.
% 

%%
%% A is fixed, number of differences are free.
%%
go :-

   N = 6, % length of the arrays
   length(A,N),
   A ins 0..1,
   length(B,N), 
   B ins 0..1,

   Diffs in 0..N, % The number of differences 
   % indomain(Diffs),

   A = [1,1,1,1,1,1],

   hamming_distance(A,B,Diffs),

   flatten([A,B,Diffs], Vars),
   label(Vars),

   writeln(diffs=Diffs),
   writeln(a=A),
   writeln(b=B),nl,
   fail.

go.


%%
%% Both A and B are free, but the number of
%% differences is fixed (#= 2).
%%
go2 :-

   N = 6, % length of the arrays
   length(A,N),
   A ins 0..1,
   length(B,N), 
   B ins 0..1,

   Diffs in 0..N, % The number of differences 
   Diffs #= 2,

   hamming_distance(A,B,Diffs),

   flatten([A,B,Diffs], Vars),
   label(Vars),

   writeln(diffs=Diffs),
   writeln(a=A),
   writeln(b=B),nl,
   fail.

go2.


hamming_distance(As, Bs, Diffs) :-
        hamming_distance_(As,Bs,0,Diffs).

hamming_distance_([],[],S,S).
hamming_distance_([A|As],[B|Bs],S0,S) :-
        Bool in 0..1,
        A #= B #<==> Bool #= 1,
        S1 #= S0 + B,
        hamming_distance_(As,Bs,S1,S).
        



