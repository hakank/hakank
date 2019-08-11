/*

  Divisible by 9 through 1 puzzle in SWI Prolog
  
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
  "Intel Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        time(once(problem(10,X,T))),
        writeln(X),
        writeln(T),
        nl.

/* 

  Here are the solutions for the bases (2..14):
   
   base=2
   [2,[1],[1]]
   
   base=3
   
   base=4
   [4,[1,2,3],[27,6,1]]
   [4,[3,2,1],[57,14,3]]
   
   base=5
   
   base=6
   [6,[1,4,3,2,5],[2285,380,63,10,1]]
   [6,[5,4,3,2,1],[7465,1244,207,34,5]]

   base=7

   base=8
   [8,[3,2,5,4,1,6,7],[874615,109326,13665,1708,213,26,3]]
   [8,[5,2,3,4,7,6,1],[1391089,173886,21735,2716,339,42,5]]
   [8,[5,6,7,4,3,2,1],[1538257,192282,24035,3004,375,46,5]]
   
   base=9
   
   base=10
   [10,[3,8,1,6,5,4,7,2,9],[381654729,38165472,3816547,381654,38165,3816,381,38,3]]
   
   base=11
   
   base=12
   
   base=13
   
   base=14
   [14,[9,12,3,10,5,4,7,6,11,8,1,2,13],[559922224824157,39994444630296,2856746045021,204053288930,14575234923,1041088208,74363443,5311674,379405,27100,1935,138,9]]

*/
go2 :-
        between(2,14,Base),
        writeln(base=Base),
        (
         time(findall([Base,X,T], problem(Base, X, T), All))
        ->
         maplist(writeln,All)
        ;
         true
        ),
        nl,
        fail,
        nl.

go2.

        
%%
%% Solve the Divisible by 9 through 1 puzzle in base Base.
%%
problem(Base, X, T) :-

        Base1 #= Base-1,
        M #= (Base^Base1)-1,    % largest value
        N #= Base - 1,          % the digits are in 1..N , 
                                % N is also the length of X 
        length(X,N),
        X ins 1..N,
        
        length(T,N),
        T ins 1..M,

        all_different(X),
        div_loop(1,N,Base,X, [],T),
        % decreasing_strict(T), % don't help
        
        append(T,X,Vars),
        labeling([ff],Vars).

%%
%% The divisible loop.
%%
div_loop(N1,N,_Base,_X, T, T) :- N1 #> N.
div_loop(I,N, Base, X, T0,[TI|T]) :-
        Base_I #= Base-I,
        findall(J,between(1,Base_I,J),Js),
        extract_from_indices(Js,X,XIs),
        to_num(XIs, Base, TI),
        TI mod Base_I #= 0,
        I1 #= I+1,
        div_loop(I1,N,Base,X,T0,T).
