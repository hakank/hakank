/*

  Young tableaux and partition in SWI Prolog

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 
  And the corresponding standard Young tableaux are:
 
  1.   1 2 3 4
 
  2.   1 2 3         1 2 4    1 3 4
       4             3        2
 
  3.   1 2           1 3
       3 4           2 4
 
  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3
 
  5.   1
       2
       3
       4
  """  


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Show all 76 solutions for N=6
%%
go :- 
        N = 6,
        findall(_, young_tableaux(N,_X,_P,1), L),
        length(L,Len),
        format("It was ~d solutions.\n\n", Len),
        nl.



%%
%% Number of solutions for N in 1..10
%%
go2 :-
        between(1,10,N),
        findall(_, young_tableaux(N,_X,_P,0),L),
        length(L,Len),
        format("~d solutions.\n\n", Len), 
        fail,
        nl.

go2.


young_tableaux(N,X,P,Print) :-

        format("Young tableaux and partitions of order ~d\n", N),
        %% X = new_array(N,N),
        %% X :: 1..N+1,
        N1 #= N+1,
        new_matrix(N,N,1..N1,X),
        
        %% for count and labeling
        flatten(X,Vars),
        
        %% the partition structure
        length(P,N),
        P ins 0..N1,
        
        %% 1..N is used exactly once (N+1 may be used many times)
        %% All relevant integers must have a key in global_cardinality/2.
        %% foreach(I in 1..N) count(I, Vars, #=, 1) end,
        findall(I-1,between(1,N,I),Counts),
        append(Counts,[N1-_], Counts2),
        global_cardinality(Vars,Counts2),
    
        %% alternative (but much slower for this purpose)
        %% alldifferent_except_N(Vars,N1),

        %% The first element is always 1
        matrix_element(X,1,1,1),
    
        %% all rows and columns should be ordered
        maplist(increasing, X),
        transpose(X,XT),
        maplist(increasing, XT),

        %% calculate the structure (the partition)
        partition_structure(X, N, P),

        %% P should be ordered
        decreasing(P),
        sum(P,#=,N),
        %% first element
        element(1,P,PFirst),
        PFirst #>= 1,
        
        %% solve
        append(Vars,P,Vars2),
        labeling([],Vars2),
        (
         Print #= 1
        ->
         writeln(p=P),
         print_matrix(X,N),
         nl
        ;
         true
        ).

%% Partition structure:
%% foreach(I in 1..N)
%%   P[I] #= sum([ (X[I,J] #=< N)  : J in 1..N])
%% end,
partition_structure(X,N,P) :-
        numlist(1,N,Is),
        partition_structure_(Is,X,N,P).

partition_structure_([],_X,_N,_P).
partition_structure_([I|Is],X,N,P) :-
        %% This don't work. Why?
        %% findall(1, (between(1,N,J), matrix_element(X,I,J,XIJ), XIJ #=< N), L),
        %%writeln(l=L),
        %%sum(L,#=, Count),
        numlist(1,N,Js),
        p_lesseq_than_n(Js,I,X,N,Count),
        element(I,P,Count),
        partition_structure_(Is,X,N,P).

p_lesseq_than_n(Js,I,X,N,Count) :-
        p_lesseq_than_n_(Js,I,X,N,0,Count).

p_lesseq_than_n_([],_I,_X,_N,Count,Count).
p_lesseq_than_n_([J|Js],I,X,N,Count0,Count) :-
        matrix_element(X,I,J,XI),
        XI #=< N,
        Count1 #= Count0+1,
        p_lesseq_than_n_(Js,I,X,N,Count1,Count).
p_lesseq_than_n_([J|Js],I,X,N,Count0,Count) :-
        matrix_element(X,I,J,XI),
        XI #> N,
        p_lesseq_than_n_(Js,I,X,N,Count0,Count).

% Nicer print of a Young Tableaux
print_matrix([], _N).
print_matrix([Row|Rows], N) :-
        print_row(Row,N),
        nl,
        print_matrix(Rows,N).

print_row([],_N).
print_row([E|Row],N) :-
        (
        E #=< N
        ->
        write(E),write(" ")
        ;
         true
        ),
        print_row(Row,N).

