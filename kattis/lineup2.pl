% https://open.kattis.com/problems/lineup
% 1s
% 1.8 Easy

main:-read_string(user_input,1000,S),split_string(S,"\n","\n",[_|L]),(sort(0,@=<,L,L)->T="INCREASING";(sort(0,@>=,L,D)->(L==D->T="DECREASING";T="NEITHER"))),writeln(T).