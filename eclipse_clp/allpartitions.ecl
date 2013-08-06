/*

  All partitions in ECLiPSe.

  Simple implementation of all partitions.
  Also, I tried some different search strategies.

  For the number of different partitions, see
  The On-Line Encyclopedia of Integer Sequences:
  http://www.research.att.com/~njas/sequences/A000041


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_sbds).


go :-
        % N = 34,
        N :: 2..34,
        indomain(N),
        findall(X, allpartitions(N, X),L),
        length(L, Len),
        % for larger N we really don't want to print all partitions
        (
            Len =< 20 -> 
                ( foreach(El, L) 
                do
                  writeln(El)
                )
        ;
                true
        ),
        writeln([n:N, len:Len]),
        fail.


%
% Y is list L with value Val removed
%
remove_val(L,Val,Y) :-
        (
            foreach(El, L),
            % note the "reversed" order in fromto 
            % to get the order correct.
            fromto(Y, Out, In, []),
            param(Val)
        do
            El #\= Val
        ->
            Out = [El|In]
        ;    
            Out = In
        ).

%
% The first part is simply to get an ordered array where the sum is N.
% This ordering is needed for removing symmetries when alla 0's is removed.
% The second part removes 0 from the array.
%
% Also, I tried some different search strategies here.
% 
allpartitions(N, Xs) :-
        % part I: get all candidates
        length(X, N),
        X :: [0..N],
        N #= sum(X),
        ordered(=<,X),
        % ordered_sum(X, N), % this is slower than sum + ordered

        % labeling(X), % 1.69s for N=34
        % my_labeling(X, min), % 1.93s for N=34
        % my_labeling(X, 1), % 1.73s for N=34
        search(X, 0, occurrence, indomain, complete, []), % 1.90s
                                                            % for N=34
        
        % part II: now remove all 0's from X
        remove_val(X, 0, Xs).

my_labeling(Vars) :-
        my_labeling(Vars, min).

% For the arguments to indomain/2, see
% http://www.eclipse-clp.org/doc/bips/lib/ic/indomain-2.html
my_labeling(Vars, Method) :-
        collection_to_list(Vars, List),
        ( 
            foreach(Var,List), 
            param(Method) 
        do
            indomain(Var, Method)
        ).
