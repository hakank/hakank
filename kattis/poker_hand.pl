% https://open.kattis.com/problems/pokerhand
% 1s
% 1.5 Easy

main :- once(read_line_to_string(user_input,S)),split_string(S," ", "",Ss),maplist(string_chars,Ss,Cs),
findall(V,member([V,_], Cs),Vs),msort(Vs,VsS),clumped(VsS,Clumped),findall(VV,member(_-VV,Clumped),Vs2),
max_list(Vs2,Max), writeln(Max).
main.
