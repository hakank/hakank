% https://open.kattis.com/problems/thegrandadventure
% 1s
% 2.1 Easy

/*
Unfortunately, he’ll also encounter villains along the way.
The Banker (who will demand money), 
the Trader (who demand incense), 
and the Jeweler (who will demand, of course, jewels). 
Jim must give each villain one of the kind of item they demand as 
he encounters them, or else he fails and his adventure is over.

$ represents Money
| represents Incense
* represents Gem
t represents a Trader
j represents a Jeweler
b represents a Banker
. represents the Ground (nothing)

*/

/*
main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([S|Ss]) :-
    string_chars(S,C),
    (p(C,[],[]) ->
        T='YES'
    ;
        T='NO'
    ),
    writeln(T),
    s(Ss).
p([],S,S).
p(['$'|T],S0,S) :- p(T,['$'|S0],S).
p(['|'|T],S0,S) :- p(T,['|'|S0],S).
p(['*'|T],S0,S) :- p(T,['*'|S0],S).
p(['t'|T],['|'|S0],S) :- p(T,S0,S).
p(['j'|T],['*'|S0],S) :- p(T,S0,S).
p(['b'|T],['$'|S0],S) :- p(T,S0,S).
p(['.'|T],S0,S) :- p(T,S0,S).
*/

% Compressed: 379 chars: Top 10 Place 8
main:-read_string(user_input,_,S),split_string(S,"\n","\n",[_|Ss]),s(Ss).
s([]). s([S|Ss]):-string_chars(S,C),(p(C,[],[])->T='YES';T='NO'),writeln(T),s(Ss).
p([],S,S).
p([$|T],S0,S):-p(T,[$|S0],S).
p(['|'|T],S0,S):-p(T,['|'|S0],S).
p([*|T],S0,S):-p(T,[*|S0],S).
p([t|T],['|'|S0],S):-p(T,S0,S).
p([j|T],[*|S0],S):-p(T,S0,S).
p([b|T],[$|S0],S):-p(T,S0,S).
p([.|T],S0,S):-p(T,S0,S).
