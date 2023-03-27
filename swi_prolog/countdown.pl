/*

  Countdown in SWI Prolog

  See Peter Ludemann's SWI-Prolog Discord post 
     Prolog program is much slower than Haskell – why?
     https://swi-prolog.discourse.group/t/prolog-program-is-much-slower-than-haskell-why/6266/16

  as well as his GitHub repo: https://github.com/kamahen/nerdle

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
go :-
    L = [1,3,5,10,25,50],
    Target = 999,
    countdown(L,Target,X,Ops),
    p(X,Ops),
    nl,
    fail,
    nl.
go.

countdown(L,Target, X,Ops) :-
    length(L,LLen),
    between(2,LLen,Len),
    length(X,Len),
    list_domain_disjunction(L,Domain),        
    X ins Domain,
    
    length(Y,Len),
    Y ins -100000000..100000000,
    OpsLen is Len-1,
    
    length(Ops,OpsLen),
    Ops ins 1..4,

    % Note: This should really be global_cardinality/2
    %       but I'm lazy...
    all_different(X),

    % Ensure that the equation is correct
    check_c(X,Ops,[],Y),
    last(Y,Target),
        
    flatten([X,Y,Ops],Vars),
    
    labeling([ffc,enum,down],Vars).

% Create the equation
check_c([],_Op,Y,Y).
check_c([V],_Op,Y,[V|Y]).
check_c([V1,V2|Ls],[Op|Ops],Y0,[V|Y]) :-
    make_op(V1,V2,Op,V),
    check_c([V|Ls],Ops,Y0,Y).

% Convert the operatos number to constraints
make_op(A,B, Op,Res) :-
  (Op #= 1) #==> (Res #= A + B),
  (Op #= 2) #==> (Res #= A - B),
  (Op #= 3) #==> (Res #= A * B),
  (Op #= 4) #==> (A #= Res * B). % Res = A / B division 

% convert a list of integers to a proper clpfd domain
list_domain_disjunction([D|Ds],Disj) :-
        foldl(disj,Ds,D,Disj).
disj(A,B,C) :-C = \/(B,A).

% Print the solution
p(X,Ops) :-
    length(X,Len),
    forall(between(1,Len,_),write("(")),
    p_(X,Ops).
p_([],_).
p_([V],_) :- format("~d)",[V]).
p_([V1|Vs],[Op|Ops]) :-
    OpsV = [+,-,*,//],
    nth1(Op,OpsV,OpS),
    format("~d) ~w ",[V1,OpS]),
    p_(Vs,Ops).

