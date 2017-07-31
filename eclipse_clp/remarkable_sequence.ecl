/*
 
  Remarkable sequence in ECLiPSe.
  
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

  Compare with the MiniZinc model: http://www.hakank.org/minizinc/remarkable_sequence.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(listut).
:-lib(ic_search).
% :-lib(branch_and_bound).
% :-lib(propia).


go :-
        N = 9, % the digits
        M = 3, % number of occurrences of each number
        NM is N*M,
        length(A, NM),
        A :: 1..N, 
        ( for(I,1,N), param(A) do
              % a[j] = i
              % a[j+i+1] = i
              % a[j+2*i+2] = i

              MaxVal is 25-(2*I),
              J :: 1..MaxVal,
              listut:nth1(J,A,I),
              JI1 is J+I+1,
              listut:nth1(JI1,A,I),
              J2I2 is J+2*I+2,
              listut:nth1(J2I2,A,I)
        ),

        % exact 3 occurrences of each digit
        ( for(I,1,N), param(A,M) do
              ic_global:occurrences(I, A, M)
        ),

        % Symmetry breaking: First element is less than the last
        listut:nth1(1,A,First),
        listut:nth1(NM,A,Last),
        First #=< Last,
        
        % labeling(A),
        search(A,0,first_fail,indomain_min,complete,[backtrack(Backtrack)]),
        ( foreach(X, A) do
              write(X), write(" ")
        ),
        nl, 
        writeln(backtracks:Backtrack),
        fail.
    
;
