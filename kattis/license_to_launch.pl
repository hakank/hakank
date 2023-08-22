% https://open.kattis.com/problems/licensetolaunch
% 1s
% 1.6 Easy

% Time Limit Exceeded on 9/11
% This might be too slow:
%    min_list(Ns,Min),
%    findall(I,(between(1,N,I),nth1(I,Ns,Min)),[M|_]),
% Testing argmin/6 instead. Yes, it's much faster-> Accepted.
% Though it's slower than the usual 0.02s: 0.12s
% And compressed the program to 335 chars (from 448 chars).

main:-rs(_),Max is 10^9,rs(S),
split_string(S," ","", Ss),maplist(number_string,Ns,Ss),argmin(Ns,0,Max,_,0,Pos),writeln(Pos).
argmin([],_,Val,Val,Pos,Pos).
argmin([V|Vs],I,Val0,Val,Pos0,Pos) :-
(V<Val0->Val1 is V,Pos1 is I;Val1 is Val0,Pos1 is Pos0),I1 is I+1,argmin(Vs,I1,Val1,Val,Pos1,Pos). 
rs(S):-read_line_to_string(user_input,S).