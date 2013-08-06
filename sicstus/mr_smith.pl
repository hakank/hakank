/*

  Mr Smith problem in SICStus Prolog.

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


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/mr_smith.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/mr_smith.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go:-
        findall(L,mr_smith(L),Res),
        write(Res),nl.

xor(X,Y) :-
        % (X or Y) #= 1.
        X or Y,
        neg(X and Y).


mr_smith(L) :-
        L = [Mr_Smith,Mrs_Smith,Matt, John,Tim],
        domain(L,0,1), % binary domain

        % If Mr Smith comes, his wife will come too.
        Mr_Smith #=> Mrs_Smith,

        % At least one of their two sons Matt and John will come.
        Matt #\/ John,

        % Either Mrs Smith or Tim will come, but not both.
        Mrs_Smith + Tim #= 1,
        % xor(Mrs_Smith, Tim),

        % Either Tim and John will come, or neither will come.
        Tim #= John,

        % If Matt comes, then John and his father will also come.
        Matt #=> (John #/\ Mr_Smith),

        labeling([],L).
