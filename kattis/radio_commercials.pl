% https://open.kattis.com/problems/commercials
% 1s
% 1.8 Easy

% Time Limit Exceeded on test 3/11.

:- use_module(library(clpfd)).
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,P|Ns],Ss),
    length(X,N),
    X ins 0..1,
    From in 1..N,
    To in 1..N,
    From #=< To,
    Len #= To - From+1,
    c(X,1,From,To),
    sum(X,#=,Len),
    scalar_product(Ns,X,#=,Z),
    Cost #= Len*P,
    Profit #= Z - Cost,   
    flatten([X,From,To,Z,Cost],Vars),
    labeling([ffc,enum,max(Profit)],Vars),
    writeln(Profit).

c([],_I,_From,_To).
c([X|T],I,From,To) :-
    (I #>= From #/\ I #=< To) #<==> X #= 1,
    (I #< From #\/ I #> To) #<==> X #= 0,
    I1 is I+1,
    c(T,I1,From,To).
