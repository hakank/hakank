% https://open.kattis.com/problems/grassseed
% 1s
% 1.4 Easy

% Another version using readln(): 126 chars!
% Note that readln/[123] converts to int if it can

main:-readln([C,_|S],end_of_file),s(S,0,T),R is C*T,format('~8f~n',[R]).
s([],S,S). s([A,B|Ss],S0,S):-S1 is S0+A*B,s(Ss,S1,S).

/*
% Uncompressed: 148 chars
main :- 
  readln([C,_|S],end_of_file)
  s(S,0,T),
  R is C*T,
  format('~8f~n',[R]).
s([],S,S). 
s([A,B|Ss],S0,S) :- 
  S1 is S0+A*B,
  s(Ss,S1,S).

*/
