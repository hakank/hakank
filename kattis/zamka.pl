% https://open.kattis.com/problems/zamka
% 1s
% 1.5 Easy

main :-rs(LS),rs(DS), rs(XS),maplist(number_string,[L,D,X],[LS,DS,XS]),
findall(N,(between(L,D,N), number_chars(N,Ns),maplist(atom_number,Ns,As),sumlist(As,X)),NNs),
min_list(NNs,MinN),writeln(MinN),max_list(NNs,MaxN), writeln(MaxN).
main.
rs(S):-read_line_to_string(user_input,S).
