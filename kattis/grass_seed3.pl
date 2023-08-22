% https://open.kattis.com/problems/grassseed
% 1s
% 1.4 Easy

% Andriy Zyevakov submitted a version of 215 chars. How?
% Trying a shorter version. 196 chars!
% Major saving of chars is to use a shorter way of reading the input.
% And using readln/1-3 makes it even smaller, see grass_seed4.pl (126 chars!)

main:-read_string(user_input,_,S),split_string(S,"\n ","\n ", Ss),maplist(number_string,[C,_|Cs],Ss),s(Cs,0,T),R is C*T,format('~8f~n',[R]).
s([],S,S). s([A,B|Ss],S0,S):-S1 is S0 + A*B,s(Ss,S1,S).

/*
% Expanded: 230 chars
main :- 
  read_string(user_input,10000,S),
  split_string(S,"\n ","\n ", Ss),
  maplist(number_string,[C,_|Cs],Ss),
  s(Cs,0,T),
  R is C*T,
  format('~8f~n',[R]).
s([],S,S). 
s([A,B|Ss],S0,S) :- 
  S1 is S0 + A*B,
  s(Ss,S1,S).


*/