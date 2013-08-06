/*

  Divisible by 9 through 1 puzzle in ECLiPSe.

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

  Compare with my MiniZinc model:
  * http://www.hakank.org/minizinc/divisible_by_9_trough_1.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_kernel). % for modulo
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
:-lib(propia).
% :-lib(ldsb).

:- op(500,xfy,'modulo').

go :-
        % Solution: 381654729
        % problem(10, X, T, Backtracks),
        problem(10, X, T, Backtracks),
        writeln(x:X),
        writeln(t:T),
        writeln(backtracks:Backtracks).

go2 :-
        ( for(Base,2,16) do
              writeln(base:Base),
              ( 
                  findall([Base, X, T, Backtracks], problem(Base, X, T, 
                                                         Backtracks),
                          L),
                  ( foreach([_Base, X, T, Backtracks], L) do
                        writeln(x:X),
                        writeln(t:T),
                        writeln(backtracks:Backtracks)
                  )
              ; 
                  true
              )
        ).

go3 :-
        ( for(Base,2,21) do
              writeln(base:Base),
              (
                  problem(Base, X, T, Backtracks),
                  writeln(x:X),
                  writeln(t:T),
                  writeln(backtracks:Backtracks)
              ;
                  true
              )
        ).


go(N) :-
        problem(N, X, T, Backtracks),
        writeln(x:X),
        writeln(t:T),
        writeln(backtracks:Backtracks),
        fail.


%
% Note: the mod operator is not supported by ic
% We would like to state
%    T[BaseI] mod I #= 0
% in the for loop
%
% Later note:
% I have now implemented a modulo propagator.
% 
problem(Base, X, T, Backtracks) :-

        M is Base^(Base-1)-1, % largest value
        N is Base - 1,        % the digits are in 1..N , 
                              % N is also the length of X 
        dim(X, [N]),
        X[1..N] :: 1..N,
        % ldsb_initialise(X, [variables_interchange]),
        ic:alldifferent(X),

        dim(T, [N]),
        T[1..N] :: 1..M,

        ( for(I,1,N),
          % param(Base,X,T,M) do
          param(Base,X,T) do
              Base_I is Base - I,
              XI is X[1..Base_I],
              toNum(XI, Base, T[I]),

              % It is easier with a modulo operator.
              T[Base_I] modulo I #= 0

              % For archiving purposes. This was the older method:
              % TBase_I is T[Base_I],
              % get_max(TBase_I, TBase_IMax), % get max domain value for
              %                               % T[Base_I]
              % MaxLimit is TBase_IMax // I,
              % Q :: 1..MaxLimit,
              % TBase_I #= Q*I
        ),

        term_variables([X,T], Vars),
        search(Vars,0,occurrence,indomain_max,complete,[backtrack(Backtracks)]).
          
        
% convert the arraynumbers in List (base Base) to a number
toNum(List, Base, Num) :-
        length(List, Len),
        length(Xs, Len),
        exp_list(Len, Base, Xs), % calculate exponents
        List :: [0..Base-1],
        Num #= List * Xs.

% helper for toNum/3
exp_list(N, Base, ExpList) :-
        ( for(I, 0, N-1), fromto([], In, Out, ExpList),  
          param(Base) do 
              B is Base^I,
              Out = [B|In]
        ).



%
% This is an implementation of a propagator for modulo.
%
% Heavily inspired by the MiniZinc definition of mod:
% http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    
% Also, see some comments on the MiniZinc Wiki:
% http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=tests:div_mod:wiki
% 
modulo(X1,Y1,R1) :-
        % These three evals is to be able to use expressions 
        % in the arguments, e.g. (X1 - 2). 
        % There expressions must be bracketed, though.
        X #= eval(X1),
        Y #= eval(Y1),
        R #= eval(R1),

        (
            nonvar(X),nonvar(Y) -> 
                R is X mod Y
        ;
                Y #\= 0,
                get_min(X,LBX),
                get_max(X,UBX),
                UBXNeg is -UBX,
                LBXNeg is -LBX,
                min(LBX,UBXNeg,MinX),
                max(UBX,LBXNeg,MaxX),
                D :: MinX..MaxX,
                
                X #= Y * D + R,
                -abs(Y) #< R, R #< abs(Y),
                MinX #=< D,
                D #=< MaxX
        ).
