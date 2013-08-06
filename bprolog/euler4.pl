/*
  Problem 4

http://projecteuler.net/index.php?section=problems&id=4

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91*99.

Find the largest palindrome made from the product of two 3-digit numbers.

Solution: 906609

*/

go :-
        L = [
             euler4a,
             euler4a2,
             euler4b,
             euler4c
            ],
        foreach(E in L, proc(E)).


proc(E) :-
        write(E),write(': '),
        time(call(E)), nl.


palindromic(L) :-
        reverse(L,L).


palindromic2(N) :-
        number_codes(N,L),
        reverse(L,L).



% listToNum(L,N) :-
%        L2 @= [C : I in L, [C], C is I+48],
%        number_codes(N,L2).

:- table numToList/2.
numToList(N, L) :-
        number_codes(N,L2),
        L @= [C : I in L2, [C], C is I-48].

%
% 12.0s
%
euler4a :-
        From = 100,
        To   = 999,
        L @= [IJ : I in From..To, J in I..To,
              [IJ, L2],
              (IJ is I*J, numToList(IJ, L2), palindromic(L2))],
        Max #= max(L),
        writeln(Max).


%
% 5.97s
%
euler4a2 :-
        From = 100,
        To   = 999,
        ExpList = [100000, 10000,1000,100,10,1],
        L @= [X : I in From..To, J in I..To,
              [L1,X],
              (
                X is I*J,
                length(L1, 6),
                L1 :: 0..9,
                scalar_product(ExpList, L1, #=, X),
                palindromic(L1)
              )
             ],
        Max #= max(L),
        writeln(Max).


%
% Using global variable
%
% 3.95s
% 
euler4b :-
        From = 100,
        To   = 999,
        global_set(maxval,0), % global variable
        ExpList = [100000, 10000,1000,100,10,1],
        % foreach(I in From..To, J in I..To,
        % Reversing the order is slightly faster
        foreach(I in To..-1..From, J in To..-1..I,
                [X, MaxVal,L1],
                (
                  X is I*J, 
                  global_get(maxval,MaxVal),
                  X > MaxVal,
                  length(L1, 6),
                  L1 :: 0..9,
                  scalar_product(ExpList, L1, #=, X),
                  reverse(L1,L1)
                -> 
                  global_set(maxval,X)
                ; 
                  true
                )
               ), 
        global_get(maxval,Max),
        writeln(Max).


%
% 2.17s (the fastest)
%
% Using palindromic2/1 which use number_codes/2 directly instead
% of converting via numToList/2.
%
euler4c :-
        From = 100,
        To   = 999,
        L @= [IJ : I in From..To, J in I..To,
              [IJ],
              (IJ is I*J, palindromic2(IJ))
             ],
        Max #= max(L),
        writeln(Max).
