% https://open.kattis.com/problems/ofugsnuid

% Trying to make a shorter program.
% Now it's the shortest Prolog program, but a little too short, right? :-)
% And it's (currently) also the fastest.
%
main:-r(_), c(L), reverse(L,R), maplist(writeln,R).
main.
r(S):-read_line_to_string(user_input,S).
c(Lines):-r(X), c(X,[],Lines).
c(X,Lines,Lines):-X==end_of_file.
c(X,Lines0,[X|Lines]):-r(X2), c(X2,Lines0,Lines).

/*
% Uncompressed
main :- 
  r(_), 
  c(L), 
  reverse(L,R), 
  maplist(writeln,R).
main.
r(S) :- 
  read_line_to_string(user_input,S).
c(Lines) :- 
  r(X), 
  c(X,[],Lines).
c(X,Lines,Lines) :-  
  X==end_of_file.
c(X,Lines0,[X|Lines]) :- 
  r(X2), 
  c(X2,Lines0,Lines).

*/