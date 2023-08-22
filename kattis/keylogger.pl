% https://open.kattis.com/problems/keylogger
% 1s
% 1.3-2.5 Easy

% TODO: Accepted 20/100
% Wrong answer on 31/87 and 41/87
% Not supported: delete and shift* does not work as expected...

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(atom_string,As,Ss),
    findall(C,(member(A,As),s(C,A)),Cs),
    p(Cs,lower,[],Ps),  
    maplist(write,Ps),
    nl.
main.

caps(upper,lower).
caps(lower,upper).

p([],_,S,S).
p([capslock|Cs],U,S0,S) :-
    caps(U,U1),
    p(Cs,U1,S0,S).
p([shift_hold_down|Cs],U,S0,S) :-
    caps(U,U1),
    p(Cs,U1,S0,S).
p([shift_release|Cs],U,S0,S) :-
    p(Cs,U,S0,S).
p([C|Cs],U,S0,[D|S]) :-
    not(memberchk(C,[capslock,delete,shift_hold_down,shift_release])),
    (U == lower ->
        D = C
    ;
        string_upper(C,D)
        
    ),
    p(Cs,U,S0,S).


s('a',clank).
s('b',bong).
s('c',click).
s('d',tap).
s('e',poing).
s('f',clonk).
s('g',clack).
s('h',ping).
s('i',tip).
s('j',cloing).
s('k',tic).
s('l',cling).
s('m',bing).
s('n',pong).
s('o',clang).
s('p',pang).
s('q',clong).
s('r',tac).
s('s',boing).
s('t',boink).
s('u',cloink).
s('v',rattle).
s('w',clock).
s('x',toc).
s('y',clink).
s('z',tuc).
s(' ',whack).
s(capslock,bump).
s(delete,pop).
s(shift_hold_down,dink).
s(shift_release,thump).
