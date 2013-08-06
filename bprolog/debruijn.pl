/*

  de Bruijn sequence in B-Prolog.

  Implementation of de Bruijn sequences in Comet, both "classical" 
  and "arbitrary". 
 
  Compare with the the web based programs:
    http://www.hakank.org/comb/debruijn.cgi   
    http://www.hakank.org/comb/debruijn_arb.cgi

  For Base = 2, N = 3, M = 8 there are 2 solutions:
    x : [](0, 1, 3, 7, 6, 5, 2, 4)
    bincode : [0, 0, 0, 1, 1, 1, 0, 1]

    x : [](0, 1, 2, 5, 3, 7, 6, 4)
    bincode : [0, 0, 0, 1, 0, 1, 1, 1]


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



%
% converts a number Num to/from a list of integer List given a base Base
%
toNum2(List, Base, Num) :-
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


%
% de Bruijn sequence with 
%   Base: base to use
%   N: length of each element (row)
%   M: length of sequence
%
deBruijn(Base, N, M, X, Binary, BinCode) :-

        %
        % X: list of the integers to use which are converted to
        %    base-ary numbers below
        %
        length(X, M),
        X :: 0..(Base**N)-1,
        alldifferent(X),

        %
        % Binary: the matrix of the "fully expanded" integers in X
        %
        new_array(Binary, [M, N]),
        array_to_list(Binary, BinaryVar),
        BinaryVar :: 0..Base-1,
        foreach(I in 1..M, 
                [Bslice,Xslice],
                (
                    % convert the integer to base-ary representation
                    Bslice @= [B : J in 1..N, [B], B @= Binary[I,J]],
                    Xslice @= X[I],
                    toNum2(Bslice, Base, Xslice)
                )
        ),

        %
        % The de Bruijn criterion: Connect one element to the next...
        %
        foreach(I in 2..M, J in 2..N, Binary[I-1, J] #= Binary[I, J-1]),

        
        % ... and around the corner.
        foreach(J in 2..N, Binary[M, J] #= Binary[1, J-1]),
        
        
        % 
        % BinCode: The de Bruijn sequence, i.e. the first
        %          elements in each row in Binary
        %          Note: this is just a slice.
        % BinCode is Binary[1..M, 1],
        BinCode @= [B : I in 1..M, [B], B @= Binary[I,1]],

        % symmetry breaking
        X[1] #= min(X),

        term_variables([X, Binary], Flattened),
        labeling([ff,down],Flattened).
        

% Let's start simple
go :-
        Base = 2,
        N = 3,
        M is 2**3,
        Tmp is Base**N-1,
        writeln(['Base':Base, 'N':N, 'M':M, '(Base**N)-1)':(Tmp)]),
        deBruijn(Base, N, M, X, Binary, BinCode),
        writeln(x:X),
        writeln('binary:':Binary),
        writeln(debruijn:BinCode),
        fail.

% This is an "arbitrary" de Bruijn sequence, i.e. the length of
% the sequence is 52, i.e. < Base**N.
go2 :-
        Base = 13,
        N = 4,
        M = 52,
        Tmp is Base**N-1,
        writeln(['Base':Base, 'N':N, 'M':M, '(Base**N)-1)':(Tmp)]),
        deBruijn(Base, N, M, X, Binary, BinCode),
        writeln(x:X),
        writeln(binary:Binary),
        writeln(debruijn:BinCode),
        nl,nl,
        fail.


go3 :-
        Base = 10,
        N = 4,
        M is Base**N,
        Tmp is Base**N-1,
        writeln(['Base':Base, 'N':N, 'M':M, '(Base**N)-1)':(Tmp)]),
        deBruijn(Base, N, M, X, Binary, BinCode),
        writeln(x:X),
        writeln(binary:Binary),
        writeln(debruijn:BinCode),
        nl,nl.
