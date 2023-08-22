% https://open.kattis.com/problems/intervalscheduling
% 1s
% 1.9 Easy

% From Johan Sannemo's Algorithmic Problem Solving, p176:
% Start with the interval with the leftmost right interval
% (using sort(2,@=<,Ns,NsS) )

% The first version was to include the actual intervals, but that gave
% Time Limit Exceeded on test 3/39. 
% By just countomg the included intervals, then it worked.
% 

% 367 chars
main :-
    read_string(user_input,10000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(t,Ss,Ns),
    sort(2,@=<,Ns,NsS),
    s(NsS,-1,0,Res),
    writeln(Res).
s([],_,R,R).
s([[A,B]|Ss],H0,R0,R) :-
    (A >= H0 -> R1 is R0 + 1, H1 = B ; R1 = R0, H1 = H0 ),
    s(Ss,H1,R1,R).
t(AB,[A,B]) :- split_string(AB," ","",ABs),maplist(number_string,[A,B],ABs).

/*
% Compressed: 307 chars
main:-read_string(user_input,10000000000,S),split_string(S,"\n","\n",[_|Ss]),maplist(t,Ss,Ns),
sort(2,@=<,Ns,NsS),s(NsS,-1,0,Res),writeln(Res).
s([],_,R,R). s([[A,B]|Ss],H0,R0,R):-(A>=H0->R1 is R0+1,H1=B;R1=R0,H1=H0),s(Ss,H1,R1,R).
t(AB,[A,B]):-split_string(AB," ","",ABs),maplist(number_string,[A,B],ABs).


*/

/*
% Here's a version that also shows the included intervals 
% (which is too slow for some of the tests).
main :-
    read_string(user_input,10000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(t,Ss,Ns),
    sort(2,@=<,Ns,NsS),
    s(NsS,-1,[],Res),
    writeln(Res),
    length(Res,ResLen),
    writeln(ResLen).
s([],_,R,R).
s([[A,B]|Ss],H0,R0,R) :-
    (A >= H0 ->
        append(R0,[[A,B]],R1), H1
        = B
    ;
        R1 = R0,
        H1 = H0
    ),
    s(Ss,H1,R1,R).
t(AB,[A,B]) :- split_string(AB," ","",ABs),maplist(number_string,[A,B],ABs).

*/