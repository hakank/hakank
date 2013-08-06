/*

  Divisible by 9 through 1 puzzle in B-Prolog.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/


  This model is however generalized to handle any base 
  (for reasonable limits).

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

/* 

  Note: It seems that for B-Prolog yield 
  integer overflow for N=10.

  Here are the solutions for the lower bases (2..9):

   base:2
   x:[1]
   t:[1]

   base:3

   base:4
   x:[1,2,3]
   t:[27,6,1]

   x:[3,2,1]
   t:[57,14,3]


   base:5

   base:6
   x:[1,4,3,2,5]
   t:[2285,380,63,10,1]

   x:[5,4,3,2,1]
   t:[7465,1244,207,34,5]

   base:7

   base:8
   x:[3,2,5,4,1,6,7]
   t:[874615,109326,13665,1708,213,26,3]

   x:[5,2,3,4,7,6,1]
   t:[1391089,173886,21735,2716,339,42,5]

   x:[5,6,7,4,3,2,1]
   t:[1538257,192282,24035,3004,375,46,5]


*/

go :-
        % Solution for N= should be 381654729
        foreach(Base in 2..15, 
                [X,T],
                (
                    nl,
                    writeln(base:Base),
                    problem(Base, X, T) -> 
                        writeln(x:X),
                        writeln(t:T),nl
                ;
                        writeln('No solution'),
                        true
                )
               ),
        nl.


go2 :-
        foreach(Base in 2..16,
                [L],
                (nl,
                 writeln(base:Base),
                 (findall([X, T], problem(Base, X, T),L),
                  foreach([X, T] in L,
                          (
                              writeln(x:X),
                              writeln(t:T),
                              nl
                          ))
                 ; 
                  true
                 )
                )
               ).



go(N) :-
        problem(N, X, T),
        writeln(x:X),
        writeln(t:T),
        fail.


problem(Base, X, T) :-

        M is Base**(Base-1)-1, % largest value
        N is Base - 1,        % the digits are in 1..N , 
                              % N is also the length of X 
        length(X, N),
        X :: 1..N,
        alldifferent(X),

        length(T, N),
        T :: 1..M,

        foreach(I in 1..N,
                [Base_I, XI],
                (Base_I is Base - I,
                 XI @= [X[J] : J in  1..Base_I],
                 toNum(XI, Base, T[I]),
                 T[Base_I] mod I #= 0
                )
        ),
        term_variables([T], Vars),
        labeling([ff],Vars).
          


%
% converts a number Num to/from a list of integer List given a base Base
%
toNum(List, Base, Num) :-
        length(List, Len),      
        length(Xs, Len),
        exp_list2(Len, Base, Xs), % calculate exponents
        scalar_product(List,Xs,#=,Num).


%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list2(N, Base, ExpList) :-
        length(ExpList, N),
        ExpList1 @= [B : I in 0..N-1, [B], B is integer(Base**I)],
        reverse(ExpList1,ExpList).

