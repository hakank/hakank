% https://open.kattis.com/problems/costumecontest
% 1s
% 1.9 Easy

% Using readln/2 instead of read_string/4 is quite shorter.
% The compressed version is now #1 in the Top 10 list (all languages)!

% Uncompressed 154 chars (which would be #2 in Top 10!)
main :-
    readln([_|S],end_of_file),
    msort(S,T),
    clumped(T,Cl),
    sort(2,@=<,Cl,R),
    [_-Min|_] = R,
    forall(member(X-Min,R),writeln(X)).

/*
% Compressed: 123 chars
% which placed in the Top 10 shortest programs: First Place!
main:-readln([_|S],end_of_file),msort(S,T),clumped(T,Cl),sort(2,@=<,Cl,R),[_-Min|_] = R,forall(member(X-Min,R),writeln(X)).

*/