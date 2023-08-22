% https://open.kattis.com/problems/upprodun
% 1s
% 1.2-1.6

main :-
    read_line_to_string(user_input,N0),
    read_line_to_string(user_input,M0),
    maplist(number_string,[N,M],[N0,M0]),
    t(N,M,N,M).
main.

t(0, 0,_N,_M).
t(I,0,N,M) :- I > 0, I1 is I-1, nl,t(I1,M,N,M).
t(I,J,N,M) :-
  I>0, J>0,
  mod2(J,N,Mod),
  (Mod =:= I -> write("*") ; true),
  J2 is J-1,
  t(I,J2,N,M).

mod2(A,B,Mod) =>
  Mod0 is A mod B,
  (Mod0 == 0 -> Mod = B ; Mod = Mod0).
