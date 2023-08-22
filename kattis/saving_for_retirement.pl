% https://open.kattis.com/problems/savingforretirement
% 1s
% 2.0 Easy

% 166 chars
main:-read_line_to_string(user_input,S),split_string(S," ","",Ss),maplist(number_string,[By,Br,Bs,Ay,As],Ss),
between(0,10000,Y),As*Y>(Br-By)*Bs,A is Ay+Y,writeln(A).


% 195 chars
mainx2 :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[By,Br,Bs,Ay,As],Ss),
    between(0,1000,Y),A is Ay+Y,T is As*Y,T > (Br-By)*Bs,writeln(A).


% 237 chars
mainx :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[By,Br,Bs,Ay,As],Ss),
    Bx is (Br-By)*Bs,
    findall(A,(between(0,10000,Y),A is Ay+Y,T is As*Y,T > Bx),[Age|_]),
    writeln(Age).
