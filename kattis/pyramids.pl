% https://open.kattis.com/problems/pyramids
% Time limit: 1s
% Diff 1.4-1.5 Easy

% I first tried with a clpfd approach but that was
% way too slow. And it didn't work for all cases.

main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    % pyramids(N, NumLayers),
    % pyramids2(N, NumLayers),
    pyramids3(N, NumLayers),    
    writeln(NumLayers).
main.

% Slower 0.13s
pyramids(N, NumLayers) :-
    M is ceiling(sqrt(N)),
    writeln(m=M),
    between(1,M,Limit),
    findall(S,(between(1,Limit,I),
               I mod 2 =:= 1,
               S is I*I,
               S =< N
              ), Squares),
    length(Squares,NumSquares),
    sum_list(Squares,SumSquares),
    (SumSquares >= N ->
        Rest is SumSquares - N,
        ( Rest =:= 0 -> 
            NumLayers is NumSquares
        ;
            NumLayers is NumSquares - 1
        )
      
    ;
        fail
    ).

% Faster: 0.03s
pyramids2(N, NumLayers) :-
    M is ceiling(sqrt(N)),
    findall(S,(between(1,M,I),
               I mod 2 =:= 1,
               S is I*I,
               S =< N
              ), Squares),
    append(X,_,Squares),
    X \= [],
    sum_list(X,Sum),
    (Sum >= N ->
        length(X,Len),
        Rest is Sum - N,
        ( Rest =:= 0 -> 
            NumLayers is Len
        ;
            NumLayers is Len - 1
        )
    ;
        fail
    ).

%
% Based on the shorter Picat version (pyramids.pi main2)
% This is slower than pyramids2/2: 0.04s-0.06s.
% 
pyramids3(N, NumLayers) :-
    M is ceiling(sqrt(N)),
    between(1,M,Len),
    findall(S,(between(1,Len,I),
               S is (2*I-1)^2,
               S =< N
              ), Squares),
    sum_list(Squares,Sum),
    (Sum >= N ->
        Rest is Sum - N,
        ( Rest =:= 0 -> 
            NumLayers is Len
        ;
            NumLayers is Len - 1
        )
    ;
        fail
    ).
