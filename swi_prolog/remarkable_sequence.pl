/*

  Remarkable sequence in SWI Prolog

  Problem statement in the Alma-0 example program remarkable.a0
  """
  This problem is taken from
  @book{CC88,
        author = "H. Coelho and J. C. Cotta",
        title = "Prolog by Example",
        publisher = "Springer-Verlag",
        address = "Berlin",
        year = 1988}
  (page 193)
 
  Call a sequence of 27 elements remarkable if it consists of three 1's,
  three 2's, ...  three 9's arranged in such a way that for all i in
  [1..9] there are exactly i numbers between successive occurrences of
  i.  For example, the sequence
 
  (1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7)
 
  is remarkable.  Write a program that generates all
  remarkable sequences.
  """

  There are three solution (with the symmetry breaking that 
  the first element must be less than the last element):

    [1,9,1,6,1,8,2,5,7,2,6,9,2,5,8,4,7,6,3,5,4,9,3,8,7,4,3]
    [1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7]
    [1,8,1,9,1,5,2,6,7,2,8,5,2,9,6,4,7,5,3,8,4,6,3,9,7,4,3]

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Find all remarkable sequences.
%%
go :-
        time(findall(A, remarkable_sequence(A),L)),
        maplist(writeln,L),
        nl.

%%
%% remarkable_sequence(A)
%%
remarkable_sequence(A) :-

   N = 9, % the digits
   M = 3, % number of occurrences of each number
   NM #= N*M,
   length(A,NM),
   A ins 1..N, 

   %% exact 3 occurrences of each digit
   findall(K-3,
           between(1,N,K),
           Cards),
   global_cardinality(A,Cards),
   
   numlist(1,N,Is),
   maplist(seq_loop(A),Is,Js),

   % Symmetry breaking: First element is less than the last
   element(1,A,A1),
   element(NM,A,ANM),
   A1 #=< ANM,   

   flatten([Js,A],Vars),
   labeling([ff,enum],Vars).

%%
%% Loop for each I in 1..N to ensure
%% that the differences between the 3 I's
%% in A is exactly I.
%%
seq_loop(A,I,J) :-
        MaxVal #= 25-(2*I),
        J in 1..MaxVal,
        element(J,A,I),
        JI1 #= J+I+1,
        element(JI1,A,I),
        J2I2 #= J+2*I+2,
        element(J2I2,A,I).
