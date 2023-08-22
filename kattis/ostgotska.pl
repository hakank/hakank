% https://open.kattis.com/problems/ostgotska
% 1s
% 1,7 Easy

% Wrong answer on 13/13.
% It's probably some edge case I've missed.
% Empty case seems to be OK.
% It worked after adding once around sub_string/5,
% but I cannot replicate some strange case...
% 

main :-
    read_line_to_string(user_input, S),
    split_string(S," ","",Ss),
    length(Ss,Len),
    findall(W,(member(W,Ss),once(sub_string(W,_,_,_,"ae"))),Ws),
    length(Ws,N),
    P is N/Len,
    (P >= 0.4 ->
        T="dae ae ju traeligt va"
    ;
        T="haer talar vi rikssvenska"
    ),
    writeln(T).
