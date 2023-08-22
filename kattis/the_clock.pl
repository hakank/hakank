% https://open.kattis.com/problems/klockan2
% 1s
% 1.6-1.7 Easy

% Solution from Sannemo, page 154f
% Why is so hard to do 0 fills in format?

main :-
    readln(N,end_of_file),
    findall(H:M,(between(0,11,H),between(0,59,M),
               HourAng is 300*H+5*M,
               MinuteAng is 60*M,
               (N =:= (MinuteAng - HourAng + 3600) mod 3600 ->
                   format('~|~`0t~d~2+:~|~`0t~d~2+~n',[H,M])
               ;
                   true
               )),
            _).

/*
% Compressed: 203 chars. Not short enough for Top 10 (48..158 chars).
main:-readln(N,end_of_file),findall(H:M,(between(0,11,H),between(0,59,M),HourAng is 300*H+5*M,MinuteAng is 60*M,(N=:=(MinuteAng-HourAng+3600)mod 3600->format('~|~`0t~d~2+:~|~`0t~d~2+~n',[H,M]);true)),_).

*/