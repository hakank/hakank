% https://open.kattis.com/problems/pieceofcake2
% 1s
% 1.5 Easy

main :-read_line_to_string(user_input,S),split_string(S," ", "", Ss),
maplist(number_string,[N,H,V],Ss),
NH is N-H,NV is N-V,
A1 is H*V*4,A2 is NH*V*4,A3 is H*NV*4,A4 is NH*NV*4,
max_list([A1,A2,A3,A4],Max),writeln(Max).
main.