% https://open.kattis.com/problems/anotherbrick
% 1s
% 2.1 Easy

% 386 chars
main :-
    readln([H,W,_|B],end_of_file),
    ((s(B,H,W,0,_,0,L),L>=H)->T="YES";T="NO"),
    writeln(T).
s([],_,_,P,P,L,L).
s([B|Bs],H,W,P0,P,L0,L) :-
    (P0+B < W->
        (Bs==[] ; [T|_]=Bs, P0+B+T =< W ),
        P1 is P0+B,
        L1 = L0
    ;
        (P0+B=:=W ->
            P1 is 0,
            L1 is L0+1
        ;
            fail
        )
    ),
    s(Bs,H,W,P1,P,L1,L).

/*
% Compressed: 246 chars. Not short enough for Top 10 (98..223 chars)
main:-readln([H,W,_|B],end_of_file),((s(B,H,W,0,_,0,L),L>=H)->T="YES";T="NO"),writeln(T).
s([],_,_,P,P,L,L).
s([B|Bs],H,W,P0,P,L0,L):-(P0+B<W->(Bs==[];[T|_]=Bs,P0+B+T=<W),P1 is P0+B,L1=L0;
(P0+B=:=W->P1 is 0,L1 is L0+1;fail)),s(Bs,H,W,P1,P,L1,L).

*/

         