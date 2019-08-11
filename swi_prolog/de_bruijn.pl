/*

  de Bruijn sequence in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).




% Let's start simple: This has 2 solutions.
go :-
        Base #= 2,
        N #= 3,
        M #= Base^N,
        deBruijn(Base, N, M, X, Binary, BinCode, GCC),
        writeln(x=X),
        maplist(writeln,Binary),
        % writeln(binCode=BinCode),
        maplist(term_to_atom,BinCode,BinCodeL),
        atom_string(BinCodeL,BinCodeS),
        writeln(bincode=BinCodeS),
        writeln(gcc=GCC),
        nl,
        fail,
        nl.


go.

%%
%% This is an "arbitrary" de Bruijn sequence, i.e. the length of
%% the sequence is 52, ( < Base**N =28561). It has many solutions.
%% 
%% (This might represent a "random" - or perhaps not that random - card deck.)
%%
%% Quite hard problem...
%%
go2 :-
        Base #= 13,
        N #= 4,
        M #= 52,
        Tmp #= (Base^N)-1,
        wrapper(Base, N, M, Tmp),
        fail.

go2.

%%
%% The "door code" sequence, i.e. all codes of length 4 in for 0..9.
%% It's quite hard....
%%
go3 :-
        Base #= 10,
        N #= 4,
        M #= Base^N,
        Tmp #= (Base^N)-1,
        wrapper(Base, N, M, Tmp).

%%
%% Another problem. It's 2000 solutions.
%% 
go4 :-
        Base #= 2,
        N #= 5,
        M #= 27,
        Tmp #= (Base^N)-1,
        wrapper(Base, N, M, Tmp),
        fail.

go4.


%
% de Bruijn sequence with 
%   Base: base to use
%   N: length of each element (row)
%   M: length of sequence
%
deBruijn(Base, N, M, X, Binary, BinCode, GCC) :-

        %%
        %% X: list of the integers to use which are converted to
        %%    base-ary numbers below
        %%
        length(X,M),
        Base2 #= (Base^N)-1,
        X ins 0..Base2,

        % all_different(X),
        all_distinct(X),

        %%
        %% Binary: the matrix of the "fully expanded" integers in X
        %%
        Base1 #= Base-1,
        new_matrix(M,N,0..Base1,Binary),

        %% convert the integer to base-ary representation
        maplist(to_binary(Base),Binary,X),
        
        %% number of occurrences for each number
        length(GCC,Base),
        GCC ins 0..M,

        %%
        %% The de Bruijn criterion: Connect one element to the next...
        %% ... and around the corner.
        %%
        de_bruijn_criterion(Binary,M,N),
        
        %% 
        %% BinCode: The de Bruijn sequence, i.e. the first
        %%          elements in each row in Binary (i.e. the first column)
        transpose(Binary,BinaryT),
        nth1(1,BinaryT,BinCode),
        
        %% GCC: Count the number of different elements in the bin code
        findall(I,between(1,Base,I),BinCodeIs),
        maplist(gcc(BinCode),BinCodeIs,GCC),

        %%
        %% If possible, we require that the occurrences of number are the same.
        %%
        (
         M mod Base #= 0
        ->
         numlist(1,Base,GCCIs),
         maplist(gcc2(Base,M,GCC),GCCIs)
        ;
         true
        ),
        
        %% symmetry breaking
        element(1,X,X1),
        min_list_clp(X,X1),

        %% solve
        flatten([X,Binary,GCC,BinCode], Vars),
        labeling([ffc,enum],Vars).


de_bruijn_criterion(Binary,M,N) :-
        findall([I1,J,I,J1],(between(2,M,I), between(2,N,J), I1 #= I-1, J1 #= J-1),IJs1),
        findall([M,J,1,J1],(between(2,N,J),J1 #= J-1), IJs2),
        append(IJs1,IJs2,IJs),
        de_bruijn_criterion_(IJs,M,Binary).

de_bruijn_criterion_([],_M,_Binary).
de_bruijn_criterion_([[I1,J,I,J1]|IJs],M,Binary) :-
        matrix_element(Binary,I1,J,BinaryVal),
        matrix_element(Binary,I,J1,BinaryVal),
        de_bruijn_criterion_(IJs,M,Binary).

% global cardinality
gcc(BinCode,I,GCC) :-
        I1 #= I-1,
        count_occurrences(BinCode,I1,GCC).

% all GCCs should be the same, i.e. M div Base
gcc2(Base,M,GCC,I) :-
        GCCI #= M div Base,
        element(I,GCC,GCCI).

        
%        
% wrapper
%
wrapper(Base, N, M, Tmp) :-
        writeln([base=Base, n=N, m=M, '(Base**N)-1)'=(Tmp)]),
        deBruijn(Base, N, M, X, Binary, BinCode, GCC),
        writeln(x=X),
        % writeln(binary=Binary),
        maplist(writeln,Binary),
        maplist(term_to_atom,BinCode,BinCodeL),
        atom_string(BinCodeL,BinCodeS),
        writeln(bincode=BinCodeS),
        writeln(gcc=GCC),
        nl.


% convert the integer to base-ary representation
to_binary(Base,BinarySlice,X) :-
        to_num(BinarySlice, Base, X).
