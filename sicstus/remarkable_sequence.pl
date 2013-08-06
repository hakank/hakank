/*

  Remarkable sequence in SICStus Prolog.

  Problem statement in the Alma-0 example program remarkable.a0
  """
  This problem is taken from
  @book{CC88,
  author = "H. Coelho and J. C. Cotta",
  title = "{P}rolog by Example",
  publisher = "Springer-Verlag",
  address = "Berlin",
  year = 1988
  }
  (page 193)
 
  Call a sequence of 27 elements remarkable if it consists of three 1's,
  three 2's, ...  three 9's arranged in such a way that for all i in
  [1..9] there are exactly i numbers between successive occurrences of
  i.  For example, the sequence
 
  (1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7)
 
  is remarkable.  Write a program that generates all
  remarkable sequences.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/remarkable_sequence.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/remarkable_sequence.ecl 


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(A,remarkable_sequence(A), L),
        ( foreach(X, L) do
              write(X), nl
        ),
        nl, 
        fd_statistics.

remarkable_sequence(A) :-
        N = 9, % the digits
        M = 3, % number of occurrences of each number
        NM is N*M,
        length(A, NM),
        domain(A,1,N), 
        ( for(I,1,N), param(A) do
              MaxVal is 25-(2*I),
              J in 1..MaxVal,
              nth1(J,A,I),
              JI1 is J+I+1,
              nth1(JI1,A,I),
              J2I2 is J+2*I+2,
              nth1(J2I2,A,I)
        ),

        % exact 3 occurrences of each digit
        ( for(K,1,9),
          foreach(C,Cards)
        do
          C = K-3
        ),
        global_cardinality(A,Cards),


        % Symmetry breaking: First element is less than the last
        element(1,A,First),
        element(NM,A,Last),
        First #=< Last,
        
        labeling([],A).
