% https://open.kattis.com/problems/expectedearnings
% 1s
% 1.8 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[N,K,P],Ss),
    T is N*P,    
    (T < K -> V="spela"; V="spela inte!"),
    writeln(V).