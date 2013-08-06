/*

  de Bruijn sequence in SICStus Prolog.

  Implementation of de Bruijn sequences, both "classical" 
  and "arbitrary". 
 
  Compare with the the web based programs:
  -  http://www.hakank.org/comb/debruijn.cgi   
  -  http://www.hakank.org/comb/debruijn_arb.cgi

  And the following model in other constraint programming systems
  * MiniZinc: http://www.hakank.org/minizinc/debruijn_binary.mzn
  * Choco   : http://www.hakank.org/choco/DeBruijn.java
  * JaCoP   : http://www.hakank.org/JaCoP/DeBruijn.java
  * JaCoP   : http://www.hakank.org/JaCoP/DeBruijnIterate.java
  * Gecode/R: http://www.hakank.org/gecode_r/debruijn_binary.rb
  * Comet   : http://www.hakank.org/comet/debruijn.co
  * Gecode  : http://www.hakank.org/gecode/debruijn.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/debruijn.ecl

  For Base = 2, N = 3, M = 8 there are 2 solutions:
    x : [0, 1, 3, 7, 6, 5, 2, 4]
    bincode : [0, 0, 0, 1, 1, 1, 0, 1]

    x : [0, 1, 2, 5, 3, 7, 6, 4]
    bincode : [0, 0, 0, 1, 0, 1, 1, 1]


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


%
% Converts a list of digits (list) to a number (Num) 
% given a base (Base). And vice versa
% 
toNum(List, Base, Num) :-
        length(List, Len),
        length(Xs, Len),
        exp_list(Len, Base, Xs), % calculate exponents
        Base1 is Base-1,
        domain(List, 0, Base1),
        scalar_product(Xs,List, #=, Num).

%
% Defaults to Base 10
%
toNum(List, Num) :-
        toNum(List, 10, Num).
   

%
% exp_list/3
% exp_list(+N, +Base, -ExpList)
% 
% ExpList is a list of the exponents 
%       [Base^(N-1), Base^(N-2),...,Base^0],
% 
% Example:
%   exp_list(3, 10, ExpList). -> ExpList = [100,10,1]
%
exp_list(N, Base, ExpList) :-
        (
            for(I, 0, N-1),
            fromto([], In, Out, ExpList),
            param(Base)
        do 
            B is integer(Base**I),
            Out = [B|In]
        ).

%
% explist/2
%
% exponent list for base 10.
exp_list(N, ExpList) :-
        exp_list(N, 10, ExpList).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


%
% de Bruijn sequence with 
%   Base: base to use
%   N: length of each element (row)
%   M: length of sequence
%
deBruijn(Base, N, M, X, Binary, BinCode) :-

        % X: list of the integers to use which are converted to
        %    base-ary numbers below
        length(X, M),
        BB is integer(Base**N)-1,
        domain(X, 0, BB),
        all_different(X),

        % Binary: the matrix of the "fully expanded" integers in X
        matrix(Binary, [M,N]),
        append(Binary,BinaryList),
        Base1 is Base - 1,
        domain(BinaryList, 0, Base1),

        % convert the integer to base-ary representation
        (for(I, 1, M), param(X, N, Base,Binary) do 
             nth1(I,Binary,Bslice),
             element(I,X,Xslice),
             toNum(Bslice,Base,Xslice)
        ),

        % The de Bruijn criterion: Connect one element to the next...
        ( for(I, 2, M),param(Binary,N) do
              ( for(J, 2, N), param(Binary,I) do
                    % Binary[I-1, J] #= Binary[I, J-1]
                    I1 is I-1,
                    matrix_element(Binary,I1,J,BA),
                    J1 is J-1,
                    matrix_element(Binary,I,J1,BB),
                    BA #= BB
              )
        ),
        
        
        % ... and around the corner.
        ( for(J, 2, N), param(Binary, M)  do 
              % Binary[M, J] #= Binary[1, J-1] 
              matrix_element(Binary,M,J,BA),
              J1 is J-1,
              matrix_element(Binary,1,J1,BB),
              BA #= BB
        ),
        
        % BinCode: The de Bruijn sequence, i.e. the first
        %          elements in each row in Binary
        ( foreach(B,Binary),
          fromto(BinCode,Out,In,[]) do
              nth1(1,B,BC),
              Out = [BC|In]
        ),

        % symmetry breaking
        minimum(MinX, X),
        element(1,X,MinX),

        append(Binary,Flattened),
        append(X,Flattened,Flattened2),
        labeling([ff,step,up], Flattened2).
        


go :-
        Base = 13,
        N = 4,
        M = 52,
        Tmp is integer(Base**N)-1,
        write(['Base':Base, 'N':N, 'M':M, '(Base^N)-1)':(Tmp)]),nl,
        deBruijn(Base, N, M, X, Binary, BinCode),
        write(x:X),nl,
        (foreach(B,Binary) do
             write(B),nl
        ),
        write(debruijn:BinCode),nl,
        fd_statistics.
