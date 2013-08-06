/*

   Mr Smith problem in ECLiPSe.

   From an IF Prolog example (http://www.ifcomputer.de/)
   """
   The Smith family and their three children want to pay a visit but they
   do not all have the time to do so. Following are few hints who will go
   and who will not:
       o If Mr Smith comes, his wife will come too.
       o At least one of their two sons Matt and John will come.
       o Either Mrs Smith or Tim will come, but not both.
       o Either Tim and John will come, or neither will come.
       o If Matt comes, then John and his father will
         also come.
    """

   The answer should be:
    Mr_Smith_comes      =  0
    Mrs_Smith_comes     =  0
    Matt_comes          =  0
    John_comes          =  1
    Tim_comes           =  1


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/
    
*/


:- lib(ic).

go:-
        findall(L,mr_smith(L),Res),
        writeln(Res).

xor(X,Y) :-
        % (X or Y) #= 1.
        X or Y,
        neg(X and Y).


mr_smith(L) :-
        L = [Mr_Smith,Mrs_Smith,Matt, John,Tim],
        L :: [0..1], % binary domain

        % If Mr Smith comes, his wife will come too.
        Mr_Smith => Mrs_Smith,

        % At least one of their two sons Matt and John will come.
        Matt or John,

        % Either Mrs Smith or Tim will come, but not both.
        Mrs_Smith + Tim #= 1,
        % xor(Mrs_Smith, Tim),

        % Either Tim and John will come, or neither will come.
        Tim #= John,

        % If Matt comes, then John and his father will also come.
        Matt => (John and Mr_Smith),

        labeling(L).
