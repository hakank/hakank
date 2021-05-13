%% background knowledge

side(b). side(w).

other(b,w). other(w,b).

red(1). red(2). red(3).

move(X,Y) :- red(R), Y is X-R.

% won(_,X,0) :- move(X,0).
% won(S,X,Y) :- move(X,Y), other(S,S1), not won(S1,Y,_).

divisible(X,Y) :- num(Y), Y > 0,  0 is X mod Y.

play(X) :- X=<3, write('White wins!'), nl.
play(X) :- won(w,X,Y), write(Y), write(': '), read(M), play(M).
play(_) :- write('White resigns.').


% num(0).
num(1). num(2). num(3). num(4).

num0(0). num1(1). num2(2). num3(3). num4(4).

to_list(A,B,C,[A,B,C]).
