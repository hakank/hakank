/*

  Propagator (bounds consistency) for modulo in ECLiPSe.

  ECLiPSe don't have a built-in support for the modulo operator 
  for in ic. So I have to roll my own. 

  This is inspired by MiniZinc's bounds consistency predicate in
  http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    

  For a use of this propagator, see these models:
  * http://www.hakank.org/eclipse/averbach_1.4.ecl
  * http://www.hakank.org/eclipse/divisible_by_9_through_1.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_kernel).
:-lib(propia).


go :-
        X :: 1..100,
        M :: 1..100,
        Y :: 0..100,
        modulo(X,M,Y),
        term_variables([X,Y,M], Vars),
        labeling(Vars),
        writeln([x:X,m:M, y:Y]),
        fail.

go1 :-
        %X :: 1..100,
        %M :: 1..100,
        % :: 0..100,
        M = 4,
        X = 10,
        Y = 2,
        modulo(X,M,Y),
        term_variables([X,Y,M], Vars),
        labeling(Vars),
        writeln([x:X,m:M, y:Y]),
        fail.



%
% This is an implementation of a propagator for modulo.
%   modulo(X1,Y1,R1)
%  R1 = X1 modulo Y1
%
% Heavily inspired by the MiniZinc definition of mod:
% http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    
% Also, see some comments on the MiniZinc Wiki:
% http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=tests:div_mod:wiki
% 
modulo(X1,Y1,R1) :-
        % These three evals is to be able to use expressions 
        % in the arguments, e.g. (X1 - 2). 
        % There expressions must be bracketed, though.
        X #= eval(X1),
        Y #= eval(Y1),
        R #= eval(R1),

        (
            nonvar(X),nonvar(Y) -> 
                R is X mod Y
        ;
                Y #\= 0,
                get_min(X,LBX),
                get_max(X,UBX),
                UBXNeg is -UBX,
                LBXNeg is -LBX,
                min(LBX,UBXNeg,MinX),
                max(UBX,LBXNeg,MaxX),
                D :: MinX..MaxX,
                
                X #= Y * D + R,
                -abs(Y) #< R, R #< abs(Y),
                MinX #=< D,
                D #=< MaxX
        ).


% X mod M #= Y using suspend
% This work sometimes, but not as general as I want.
modulo_suspend(X,M,Y) :-  suspend(M is X mod M, 3, [X,Y]->inst).

