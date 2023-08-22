% https://open.kattis.com/problems/compass
% 1s
% 2.1s

% TODO!

main :-
    readln([A,B],end_of_file),
    writeln([a=A,b=B]),
    X0 is A-B,
    X1 is (A-B) mod 360,
    X2 is 360-X1,
    X3 is X1-360,
    writeln([x0=X0,x1=X1,x2=X2,x3=X3]),
    X is min(X1,X2),
    writeln(X).
