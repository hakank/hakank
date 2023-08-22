% https://open.kattis.com/problems/prva
% 1s
% 1.8 Easy

% Yes, this is a mess, converting between string,chars,codes,string...
%
% I also tried DCGs but it was messier...

:- use_module(library(clpfd)).
main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_chars,Ss,Cs),
    transpose(Cs,CsT),
    append(Cs,CsT,L),
    s(L,[],L2),    
    flatten(L2,F),
    string_codes(T,F),
    split_string(T,"#","#",Ts),
    findall(W,(member(W,Ts),string_length(W,Len),Len>=2),Ws),
    sort(Ws,[Res|_]),
    writeln(Res).
s([],S,S).
s([L|Ls],S0,[L,'#'|S]) :-
    s(Ls,S0,S).