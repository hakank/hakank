% https://open.kattis.com/problems/soylent
% 1s
% 1.7 Easy

main:-read_string(user_input,100000,S),split_string(S,"\n","\n",Ss),
maplist(number_string,[N|Ns],Ss),(N=:=0->writeln(0);s(Ns)).
s([]).
s([N|Ns]):-D is N div 400,(N mod 400=:=0->writeln(D);D1 is D+1,writeln(D1)),s(Ns).
