% https://open.kattis.com/problems/veci
% 1s
% 1.7 Veci

main:-read_line_to_string(user_input,S),
number_string(N,S),number_chars(N,Ds),
findall(P,(permutation(Ds,Ps),number_chars(P,Ps),P>N),R),
(R==[]->T=0;sort(R,X),[T|_]= X),writeln(T).
