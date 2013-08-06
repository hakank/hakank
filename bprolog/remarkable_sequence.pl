/*

  Remarkable sequence in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        findall(A,remarkable_sequence(A), L),
        foreach(X in L, writeln(X)),
        nl.

remarkable_sequence(A) :-

        N = 9, % the digits
        M = 3, % number of occurrences of each number
        NM is N*M,
        length(A, NM),
        A :: 1..N, 

        foreach(I in 1..N,
                [MaxVal,J,JI1,J2I2],
                ac(Js,[]),
                (
                    MaxVal is 25-(2*I),
                    J in 1..MaxVal,
                    nth1(J,A,I),
                    JI1 is J+I+1,
                    nth1(JI1,A,I),
                    J2I2 is J+2*I+2,
                    nth1(J2I2,A,I),
                    Js^1 = [J|Js^0]
                )
        ),

        % exact 3 occurrences of each digit
        Cards @= [C : K in 1..9, [C], C = K-3],
        global_cardinality(A,Cards),
        % foreach(K in 1..9, exactly(3,A,K)),
        % foreach(K in 1..9, count(K,A,#=,3)),

        % Symmetry breaking: First element is less than the last
        element(1,A,First),
        element(NM,A,Last),
        First #=< Last,

        term_variables([A,Js],Vars),
        labeling([updown],Vars).
