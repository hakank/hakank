/* 
  Problem 10
  
  http://projecteuler.net/index.php?section=problems&id=10

  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.

  Solution: 142913828922

*/

is_prime3(2).
is_prime3(3).
is_prime3(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor3(P,3).  

has_factor(N,L) :- N mod L #= 0.
has_factor(N,L) :- L * L #< N, L2 #= L + 2, has_factor(N,L2).

% :- table has_factor3/2.

has_factor3(N,L) :- N mod L =:= 0.
has_factor3(N,L) :- L * L < N, L2 is L + 2, has_factor3(N,L2).

% Requires too much memory
is_prime2(N) :-
        divisors(N, Divisors),
        Divisors = [].

divisors(N, Div) :-
        Div @= [J: J in 2..sqrt(N), [M], (M is N mod J, M =:= 0)].


%
% foreach with accumulator
% 1:15 minutes
% (ECLiPSe solve this in 7.5s with a similar approach)
%
euler10a :-
        foreach(
                I in 1..2000000, ac(Sum, 0),
                (
                  is_prime3(I) ->
                  Sum^1 is Sum^0 + I
                ;
                  Sum^1 is Sum^0
                )
               ),
        writeln(Sum).


%
% list comprehension
% 5:40minutes
%
euler10b :-
        L @= [I : I in 1..2000000,is_prime3(I)],
        Sum #= sum(L),
        writeln(Sum).

%
% checking just the odd numbers
% 1:13 minutes
%
euler10c :-
        foreach(
                I in 3..2..2000000, ac(Sum, 0),
                (
                  is_prime3(I) ->
                  Sum^1 is Sum^0 + I
                ;
                  Sum^1 is Sum^0
                )
               ),
        Sum2 is 2 + Sum,
        writeln(Sum2).

% list comprehension
% 5:40minutes
euler10d :-
        L @= [I : I in 3..2..2000000,is_prime3(I)],
        Sum #= sum(L),
        Sum2 is Sum + 2,
        writeln(Sum2).


% Too slow and memory hungry
% euler10e :-
%         foreach(
%                 I in 1..2000000, ac(Sum, 0),
%                 (
%                   is_prime2(I) ->
%                   Sum^1 is Sum^0 + I
%                 ;
%                   Sum^1 is Sum^0
%                 )
%                ),
%         writeln(Sum).


% 26.42s (best so far)
euler10f :-
        C @= [I : I in 3..2..2000000],
        sublist(prime, C, R),
        N is sum(R),
        writeln(N).

composite(N) :- N<2.
composite(N) :-
  N>2,
  N2 is floor(sqrt(N)),
  between(2, N2, A),
  0 =:= mod(N, A).

prime(N) :- not(composite(N)).

%
% From http://www.probp.com/publib/applic.html
%
%   sublist(Pred, List, SubList)
%   succeeds when SubList is the sub-sequence of the List containing all
%   the Elems of List for which Pred(Elem) succeeds.

sublist(_, [], []).
sublist(Pred, [Head|List], SubList) :-
	apply(Pred, [Head]),
	!,
	SubList = [Head|Rest],
	sublist(Pred, List, Rest).
sublist(Pred, [_|List], SubList) :-
	sublist(Pred, List, SubList).

%
% From http://www.probp.com/publib/applic.html
%
%   apply(Pred, Args)
%   is the key to this whole module.  It is basically a variant of call/1
%   (see the Dec-10 Prolog V3.43 manual) where some of the arguments may
%   be already in the Pred, and the rest are passed in the list of Args.
%   Thus apply(foo, [X,Y]) is the same as call(foo(X,Y)),
%   and apply(foo(X), [Y]) is also the same as call(foo(X,Y)).
%   BEWARE: any goal given to apply is handed off to call/1, which is the
%   Prolog *interpreter*, so if you want to apply compiled predicates you
%   MUST have :- public declarations for them.  The ability to pass goals
%   around with some of their (initial) arguments already filled in is
%   what makes apply/2 so useful.  Don't bother compiling anything that
%   uses apply heavily, the compiler won't be able to help much.  LISP
%   has the same problem.  Later Prolog systems may have a simpler link
%   between compiled and interpreted code, or may fuse compilation and
%   interpretation, so apply/2 may come back into its own.  At the moment,
%   apply and the routines based on it are not well thought of.

apply(Pred, Args) :-
	(   atom(Pred),
            Goal =.. [Pred|Args]
	;                       %	compound(Pred)
            Pred =.. OldList,
            append(OldList, Args, NewList),
            Goal =.. NewList
	),  !,
	call(Goal).


go :-
        % write('euler10a: '),
        % time(euler10a).
        % write('euler10b: '),
        % time(euler10b).
        % write('euler10c: '),
        % time(euler10c).
        % write('euler10d: '),
        % time(euler10d).
        % write('euler10e: '),
        % time(euler10e).
        write('euler10f: '),
        time(euler10f).

