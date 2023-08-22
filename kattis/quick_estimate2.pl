% https://open.kattis.com/problems/quickestimate
% 1s
% 1.6 Easy

% A little shorter: 193 chars. Johan Öhlin's solution is 186 chars.
% Changed to 181 chars:
% - split_string(S,"\n","",[_|T])
% - skipping the empty main/0 clause.
% And skipping the size (-> _) in read_string/3 -> _:  176 chars
%
% - and changing to readln/2: 125 chars

main:-readln([_|T],end_of_file),q(T).
q([]). q([""]). q([N|Ns]):-(N>0->T is 1+floor(log(N)/log(10));T is 1),writeln(T),q(Ns).

/*
% Uncompressed: 181 chars
main :-
    readln([_|T],end_of_file),
    q(T).
q([]).
q([""]).
q([N|Ns]) :-
    (N>0->
        T is 1+floor(log(N)/log(10))
    ;
        T is 1
    ),
    writeln(T),
    q(Ns).
*/

/*
% Old version using read_string/3
main:-read_string(user_input,_,S),split_string(S,"\n","",[_|T]),q(T).
q([]). q([""]). q([I|Is]):-number_string(N,I),(N>0->T is 1+floor(log(N)/log(10));T is 1),writeln(T),q(Is).
*/
