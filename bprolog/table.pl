/*

  Global constraint table in B-Prolog.

  From MiniZinc's global.mzn (or rather table_int.mzn) 
  """
  A table constraint: table(x, t) represents the constraint x in t where we
  consider each row in t to be a tuple and t as a set of tuples.
  """

  Note: B-Prolog has a table constraint, e.g.
       (I1,I2,I3) in Allowed

  Here is a simple variant (poor man's table), not using 
  the "in" construct.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

% using table/3
go :-

        % the table
        T = []([](1,2,1),
               [](1,2,3),
               [](1,3,3),
               [](2,1,3),
               [](2,3,4),
               [](2,5,2),
               [](3,4,4),
               [](4,1,3),
               [](6,5,1)),
        
        Rows @= T^length,

        X :: 1..6,
        Y :: 1..6, 
        Z :: 1..6, 
       
        X #\= Y,
        Z #< 3,

        Ix :: 1..Rows,
        table([](X,Y,Z), T, Ix),

        term_variables([X,Y,Z,Ix], Vars),
        labeling(Vars),

        writeln([X,Y,Z]),
        writeln(ix:Ix),
        nl,
        fail.


% using table/2
go2 :-

        % the table
        T = []([](1,2,1),
               [](1,2,3),
               [](1,3,3),
               [](2,1,3),
               [](2,3,4),
               [](2,5,2),
               [](3,4,4),
               [](4,1,3),
               [](6,5,1)),
        
        X :: 1..6,
        Y :: 1..6, 
        Z :: 1..6, 
       
        X #\= Y,
        Z #< 3,

        table([](X,Y,Z), T),

        term_variables([X,Y,Z], Vars),
        labeling(Vars),

        writeln([X,Y,Z]),
        nl,
        fail.



%
% table/3
% There is some Ix which is the index
% of Table and is = X.
%
% Note: This use freeze(Ix,Goal).
%
table(X, Table,Ix) :-
        Len @= Table[1]^length,
        foreach(I in 1..Len, 
                freeze(Ix,Table[Ix,I] #= X[I])
        ).

%
% table/2
%
% There is some Ix which is the index
% of Table and is X
% Here Ix is inside the predicate and we use labeling 
% to fix it.
%
table(X, Table) :-
        R @= Table^length,
        CLen @= Table[1]^length,
        Ix :: 1..R,
        foreach(I in 1..CLen, 
             freeze(Ix,Table[Ix,I] #= X[I])
        ),
        labeling([Ix]).
