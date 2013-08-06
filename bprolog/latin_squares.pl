/*

  Latin squares in B-Prolog.

  http://en.wikipedia.org/wiki/Latin_square:
  """
  A Latin square is an n X n table filled with n different symbols in
  such a way that each symbol occurs exactly once in each row and
  exactly once in each column. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        N = 40,
        latin_square(N, X),
        term_variables(X,Vars),
        labeling([ff],Vars),
        pretty_print(X),
        nl.


latin_square(N, X) :-
        new_array(X, [N,N]),
        array_to_list(X,XList),
        XList :: 1..N,
        Rows @= X^rows,
        foreach(Row in Rows,all_distinct(Row)),
        Columns @= X^columns,
        foreach(Column in Columns, all_distinct(Column)).

        % symmetry breaking: value in row R must not be R
        % foreach(R in 1..N, X[R,R] #\= R).


pretty_print(X) :-
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)).



