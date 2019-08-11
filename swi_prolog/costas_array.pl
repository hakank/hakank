/*

  Costas array in SWI Prolog

  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """
  Also see
  http://en.wikipedia.org/wiki/Costas_array

  [From my MiniZinc model:] 
  This model is based on Barry O'Sullivan's MiniZinc model
  (http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn)
  Here are the two rather simple differences 
  (marked by "hakank" below)
   1) no symmetry breaking on the order of the Costas array
   2) fixes the lower triangular matrix in the difference
      matrix to -n+1
  
  Since there is no symmetry breaking of the order of the Costas 
  array it gives all the solutions for a specific length of 
  the array, e.g. those 
  listed in http://mathworld.wolfram.com/CostasArray.html
  
  1	1	(1)
  2	2	(1, 2), (2,1)
  3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
  4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
                (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
                (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
  ....
  
  See http://www.research.att.com/~njas/sequences/A008404
  for the number of solutions for n=1..
  1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
  17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
  872, 200, 88, 56, 204,...

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Show all 444 solutions for N=8.
%%
go :-
        N = 8,
        findall(Costas,costas(N,Costas,print),L),
        length(L,Len),
        writeln(len=Len),
        nl.

%%
%% Number of solutions for N=2..10
%%
go2 :-
        between(2,10,N),
        findall(Costas,costas(N,Costas,noprint),L),
        length(L,Len),
        writeln(N=Len),
        fail,
        nl.

go2.


costas(N,Costas,Print) :-
   
        length(Costas,N),
        Costas ins 1..N,

        NegN1 #= -N+1,
        N1 #= N-1,
        new_matrix(N,N,NegN1..N1,Differences),
   
        %%
        %% hakank: Here are my two changes
        %%
        %% 1) I skipped this constraint since I want 
        %%    to generate all solutions.
        %% Costas[1] #< Costas[N],
   
        %% 2) Fix the values in the lower triangle in the
        %% difference matrix to -n+1. This removes variants 
        %% of the difference matrix for the the same Costas array.
        findall([I,J],
                (between(1,N,I),
                 between(1,I,J)
                ),
                IJs1),
        maplist(fix_lower_triangle(Differences,NegN1),IJs1),
   
        %% hakank: All the following constraints below are from 
        %% Barry O'Sullivans's original MiniZinc model.
        all_different(Costas),

        %% "How do the positions in the Costas array relate 
        %%  to the elements of the distance triangle."
        findall([I,J],
                (between(1,N,I),
                 I1 #= I+1,
                 between(I1,N,J)
                ),
                IJs2),
        maplist(position_distance_triangle(Differences,Costas),IJs2),
        

        %% "All entries in a particular row of the difference 
        %%  triangle must be distint."
        numlist(1,N1,Is3),
        maplist(distinct_row(Differences,N),Is3),
                
        %% "All the following are redundant - only here to speed up search."
   
        %% "We can never place a 'token' in the same row as any other."
        maplist(no_place_token(Differences),IJs2),        
        findall([K,L],
                (between(3,N,K),
                 K1 #= K+1,
                 between(K1,N,L)
                ),
                KLs),
        maplist(differences_add_eq(Differences),KLs),
        
        flatten([Costas,Differences],Vars),
        labeling([ff,enum],Vars),
        
        (
         Print == print
        ->
         writeln(costas=Costas),
         maplist(print_row(NegN1),Differences),
         nl
        ;
         true
        ).

print_row(NegN1,DifferenceRow) :-
        findall(E,
                (member(E,DifferenceRow),
                 E #\= NegN1
                ),
               Es),
        writeln(Es).
        

%% 2) Fix the values in the lower triangle in the
%% difference matrix to -n+1. This removes variants 
%% of the difference matrix for the the same Costas array.
fix_lower_triangle(Differences,NegN1,[I,J]) :-
        matrix_element(Differences,I,J,DIJ),
        DIJ #= NegN1.

%% "How do the positions in the Costas array relate 
%%  to the elements of the distance triangle."
position_distance_triangle(Differences,Costas,[I,J]) :-
        element(J,Costas,CJ),
        JI #= J-I,
        element(JI,Costas,CJ1),
        CDiff #= CJ - CJ1,
        matrix_element(Differences,I,J,CDiff).

%% "All entries in a particular row of the difference 
%%  triangle must be distint."
distinct_row(Differences,N,I) :-
        I1 #= I+1,
        numlist(I1,N,Js),
        N1 #= N-1,
        findall([I,J],
                (between(1,N1,I),
                 member(J,Js)
                ),
                IJs
               ),
        extract_from_indices2d(IJs,Differences,Ds),
        all_different(Ds).

        

%% "We can never place a 'token' in the same row as any other."
%% foreach(I in 1..N, J in I+1..N)  Differences[I,J] #!= 0 end,
no_place_token(Differences,[I,J]) :-
        matrix_element(Differences,I,J,DIJ),
        DIJ #\= 0.


%% foreach(K in 3..N, L in K+1..N)
%%   Differences[K-2,L-1] + Differences[K,L] #= 
%%   Differences[K-1,L-1] + Differences[K-1,L]
%% end,
differences_add_eq(Differences,[K,L]) :-
        K2 #= K-2,
        K1 #= K-1,
        L1 #= L-1,
        matrix_element(Differences,K2,L1,DK2L1),
        matrix_element(Differences,K,L,DKL),
        matrix_element(Differences,K1,L1,DK1L1),
        matrix_element(Differences,K1,L,DK1L),
        DK2L1 + DKL #= DK1L1 + DK1L.
