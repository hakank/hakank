% https://open.kattis.com/problems/addtwonumbers
% 1s
% 1.4 Easy

% 117 chars
main:-read_string(user_input,100,S),split_string(S," ","\n ",Ss),maplist(number_string,[A,B],Ss),T is A+B,writeln(T).

/*
  % Uncompressed: 135 chars
main :- 
  read_string(user_input,100,S),
  split_string(S," ","\n ",Ss),
  maplist(number_string,[A,B],Ss),
  T is A+B,
  writeln(T).

*/


/*
  Olö version
% 138 chars
main_old :-rs(S),split_string(S," ","",Ss),maplist(number_string,[A,B],Ss),T is A+B,writeln(T).
rs(S):-read_line_to_string(user_input,S).

*/