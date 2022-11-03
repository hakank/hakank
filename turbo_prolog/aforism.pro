predicates

  aforism(integer, string)
  aforisme(integer,string)

include "afor.pro"
include "afor2.pro"

predicates
        rand_aforism(integer)
        rand_int_1_401(integer)
                        
clauses

       rand_int_1_401(X) if
          random(Y), X=Y*401 + 0.5.

       rand_aforism(0) if !.

       rand_aforism(Count) if
             rand_int_1_401(N), N < 200, aforism(N, Aforism), !,
             write("Dagens l„rda ord „r:\n"), nl, nl,
             write(Aforism), nl, write("(H†kan)\n").

       rand_aforism(Count) if
             rand_int_1_401(N), N > 200, aforisme(N, Aforism), !,
             write("Dagens l„rda ord „r:\n"), nl, nl,
             write(Aforism), nl, write("(H†kan)\n").

       rand_aforism(_) if
             rand_aforism(1).


goal
      rand_aforism(1).
