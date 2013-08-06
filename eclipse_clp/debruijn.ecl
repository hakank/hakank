/*

  de Bruijn sequence in ECLiPSe.

  Implementation of de Bruijn sequences in Comet, both "classical" 
  and "arbitrary". 
 
  Compare with the the web based programs:
    http://www.hakank.org/comb/debruijn.cgi   
    http://www.hakank.org/comb/debruijn_arb.cgi

  And the following in other constraint programming systems
  * MiniZinc: http://www.hakank.org/minizinc/debruijn_binary.mzn
  * Choco   : http://www.hakank.org/choco/DeBruijn.java
  * JaCoP   : http://www.hakank.org/JaCoP/DeBruijn.java
  * JaCoP   : http://www.hakank.org/JaCoP/DeBruijnIterate.java
  * Gecode/R: http://www.hakank.org/gecode_r/debruijn_binary.rb
  * Comet   : http://www.hakank.org/comet/debruijn.co
  * Gecode  : http://www.hakank.org/gecode/debruijn.cpp


  For Base = 2, N = 3, M = 8 there are 2 solutions:
    x : [](0, 1, 3, 7, 6, 5, 2, 4)
    bincode : [0, 0, 0, 1, 1, 1, 0, 1]

    x : [](0, 1, 2, 5, 3, 7, 6, 4)
    bincode : [0, 0, 0, 1, 0, 1, 1, 1]

  Darn, quite soon there is stack overflow, e.g. the following goal:
    deBruijn(13, 4, 521, _, _, BinCode), writeln(BinCode).


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(propia).


%
% converts a number Num to/from a list of integer List given a base Base
%
toNum2(List, Base, Num) :-
        length(List, Len),      
        length(Xs, Len),
        exp_list2(Len, Base, Xs), % calculate exponents
        Num #= List*Xs.


%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list2(N, Base, ExpList) :-
        length(ExpList, N),
        (for(I, 0, N-1), fromto([], In, Out, ExpList), param(Base)  do 
            B is Base^I,
            Out = [B|In]
        ).


%
% de Bruijn sequence with 
%   Base: base to use
%   N: length of each element (row)
%   M: length of sequence
%
deBruijn(Base, N, M, X, Binary, BinCode, Backtracks) :-

        %
        % X: list of the integers to use which are converted to
        %    base-ary numbers below
        %
        dim(X, [M]),
        X[1..M] :: 0..(Base^N)-1,
        ic_global:alldifferent(X),

        %
        % Binary: the matrix of the "fully expanded" integers in X
        %
        dim(Binary, [M, N]),
        Binary[1..M, 1..N] :: 0..Base-1,
        (for(I, 1, M), param(X, N, Base, Binary) do 
             % convert the integer to base-ary representation
             Bslice is Binary[I,1..N],
             Xslice is X[I],
             toNum2(Bslice, Base, Xslice)
        ),


        %
        % The de Bruijn criterion: Connect one element to the next...
        %
        (for(I, 2, M) * for(J, 2, N), param(Binary) do
             Binary[I-1, J] #= Binary[I, J-1]
        ),

        
        % ... and around the corner.
        (for(J, 2, N), param(Binary, M)  do 
             Binary[M, J] #= Binary[1, J-1] 
        ),
        
        
        % 
        % BinCode: The de Bruijn sequence, i.e. the first
        %          elements in each row in Binary
        %          Note: this is just a slice.
        BinCode is Binary[1..M, 1],

        % symmetry breaking
        X[1] #= ic:min(X),

        term_variables([X, Binary], Flattened),
        search(Flattened, 0, first_fail, indomain_split, complete, [backtrack(Backtracks)]).
        


go :-
        Base = 13,
        N = 4,
        M = 52,
        Tmp is Base^N-1,
        writeln(["Base":Base, "N":N, "M":M, "(Base^N)-1)":(Tmp)]),
        deBruijn(Base, N, M, X, Binary, BinCode,Backtracks),
        writeln(x:X),
        writeln(binary:Binary),
        writeln(debruijn:BinCode),
        writeln(backtracks:Backtracks).
