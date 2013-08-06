/*

  Global constraint table in ECLiPSe.

  From MiniZinc's global.mzn (or rather table_int.mzn) 
  """
  A table constraint: table(x, t) represents the constraint x in t where we
  consider each row in t to be a tuple and t as a set of tuples.
  """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


%
% table/3
% There is some Ix which is the index
% of Table and is X
%
table(X, Table,Ix) :-
        dim(Table,[_R,C]),
        dim(X, [Len]),
        Len = C,

        (for(I,1,Len), param(X,Table,Ix) do
             suspend(Table[Ix,I] #= X[I],2,Ix->inst)
        ).

%
% table/2
% This works, i.e. when labeling inside table.
% There is some Ix which is the index
% of Table and is X
%
table(X, Table) :-
        dim(Table,[R,C]),
        dim(X, [Len]),
        Len = C,

        Ix :: 1..R,
        (for(I,1,Len), param(X,Table,Ix) do
             suspend(Table[Ix,I] #= X[I], 12, Ix->inst)
        ),
        labeling([Ix]).


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
        
        % Rows = 3,
        % Cols = 3,
        dim(T, [Rows,_Cols]),
        T :: 1..Rows,

        % dim(X, [Cols]),
        % X :: 1..6,

        X :: 1..6,
        Y :: 1..6, 
        Z :: 1..6, 
       
        X #\= Y,
        Z #< 3,

        % using table/2
        % table([](X,Y,Z), T),

        % using table/3
        Ix :: 1..Rows,
        table([](X,Y,Z), T, Ix),


        % term_variables([X,Y,Z], Vars), % table/2
        term_variables([X,Y,Z,Ix], Vars), % table/3
        labeling(Vars),

        writeln([X,Y,Z]),
        writeln(ix:Ix),
        nl,
        fail.

