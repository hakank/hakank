:-table fib/2.

fib(0, 1):-!.
fib(1, 1):-!.
fib(N,F):-N1 is N-1, N2 is N-2,fib(N1,F1),fib(N2,F2),F is F1+F2.

/*
| ?- fib(10,X)
X = 89 ?
yes
| ?- fib(100,X) 
X = 573147844013817084101 ?
yes
| ?- fib(1000,X)
X = 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501 ?
yes

% Notera att Y måste vara en local variabel, [Y].
| ?- F @= [ Y : X in 1..10, [Y],fib(X,Y)]
F = [1,2,3,5,8,13,21,34,55,89]

| ?- F @= [ Y : X in 1..100, [Y],fib(X,Y)],P @= F[100]
...
P = 573147844013817084101

fib/2 är dock inte reversibel:

| ?- fib(5,F)
F = 8 ?
yes
| ?- fib(N,8)
*** error(instantiation_error,(-)/2)

*/


:-table fib2/2.
fib2(0, 1):-!.
fib2(1, 1):-!.
fib2(N,F):-N1 #= N-1, N2 #= N-2,fib2(N1,F1),fib2(N2,F2),F #= F1+F2.

/*
Det blir inte heller reversibelt om man använde CLP(FD), dvs #= istället för "is"

| ?- fib2(10,F)
F = 89 ?;
no
| ?- fib2(N,89)
no

*/

% Plain CLP(FD)
:- table fib3/2.
fib3(N,1) :- N #=< 1.
fib3(N,F):- N #> 1, N1 #= N-1, N2 #= N-2,fib3(N1,F1),fib3(N2,F2),F #= F1+F2.

