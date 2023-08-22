% https://open.kattis.com/problems/shoppinglisteasy
% 1s
% 1.9 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[NM|Ss]),
    split_string(NM," ","",NMs),
    maplist(number_string,[N,_],NMs),
    maplist(sp,Ss,Ts),
    flatten(Ts,TT),
    msort(TT,Sort),
    clumped(Sort,Cl),
    findall(F,member(F-N,Cl),Fs),
    msort(Fs,FsS),
    length(FsS,Len),
    writeln(Len),
    maplist(writeln,FsS).
sp(S,Ss) :- split_string(S," ","",Ss).