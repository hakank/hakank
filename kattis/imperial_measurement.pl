% https://open.kattis.com/problems/measurement
% 1s
% 2.0 Easy

% Convert everything to thou

% A little shorter: 436 chars
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[N0,M1,_,M2]),
    number_string(N,N0),
    A=["thou","inch","foot","yard","chain","furlong","mile","league"],
    B=["th","in","ft","yd","ch","fur","mi","lea"],
    C=[1,1000,12000,36000,792000,7920000,63360000,190080000],
    (nth1(I1,A,M1);nth1(I1,B,M1)),
    (nth1(I2,A,M2);nth1(I2,B,M2)),
    nth1(I1,C,Th1),nth1(I2,C,Th2),
    X is N*Th1/Th2,
    writeln(X).

/*
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[N0,M1,_,M2]),
    number_string(N,N0),
    (c(M1,_,Th1) ; c(_,M1,Th1)),
    (c(M2,_,Th2) ; c(_,M2,Th2)),
    nonvar(M1),nonvar(M2),
    X is N*Th1/Th2,
    writeln(X).
c("thou","th",1).
c("inch","in",1000).
c("foot","ft",12*1000).
c("yard","yd",3*12*1000).
c("chain","ch",22*3*12*1000).
c("furlong","fur",10*22*3*12*1000).
c("mile","mi",8*10*22*3*12*1000).
c("league","lea",3*8*10*22*3*12*1000).
*/
