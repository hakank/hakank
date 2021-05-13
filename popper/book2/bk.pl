%% background knowledge

author(1,camoes).
author(2,pessoa).
author(3,vergilioFerreira).
author(4,saramago).
author(5,saramago).
author(6,eca).
author(7,eca).

title(1,lusiadas).
title(2,mensagem).
title(3,contaCorrente).
title(4,levantadoDoChao).
title(5,memorialDoConvento).
title(6,osMaias).
title(7,aReliquia).

editor(1,asa).
editor(2,asa).
editor(3,asa).
editor(4,caminho).
editor(5,caminho).
editor(6,lello).
editor(7,lello).

%% extra

id(1). id(2). id(3). id(4). id(5). id(6). id(7).

c1(1). c2(2). c3(3).

n1(1,B,C) :- nth1(1,B,C).
n2(2,B,C) :- nth1(2,B,C).
n3(3,B,C) :- nth1(3,B,C).

cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).
not_empty([_|_]).
