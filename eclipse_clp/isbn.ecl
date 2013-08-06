/*

  Some explorations of ISBN13 in ECLiPSe.

 See http://en.wikipedia.org/wiki/ISBN

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/isbn.mzn
  * Comet   : http://www.hakank.org/comet/isbn.co
  * SICStus : http://www.hakank.org/sicstus/isbn.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
:-lib(propia).




% Test ISBN:
% 978-0262720304: The OPL Optimization Programming Language 
% [9,7,8,0,2,6,2,7,2,0,3,0]
%
% isbn = 978-0262220774: Constraint-based Local Search
%  [9,7,8,0,2,6,2,2,2,0,7,7];


go :-
        N = 13,

        length(ISBN,N),
        ISBN :: 0..9,

        Mult0 :: 1..9,
        Mult1 :: 1..9,

        Mult0 #= 3,
        Mult1 #= 1,

        % Mult0 #\= Mult1, % extra constraint

        % The first N-1 digits, for the check sum
        N1 is N-1,
        length(TT,N1),
        TT :: 0..100,

        % test ISBN
        ISBN = [9,7,8,0,2,6,2,7,2,0,3,0,_], % get check digit
        % ISBN = [9,7,8,0,2,_,2,7,2,0,3,0,4], % get some other digit

        % Prepare for the check sum
        ( foreach(T,TT),
          for(C,1,N1),
          param(ISBN,Mult0,Mult1) do
              nth1(C,ISBN,I),
              (
                  C mod 2 =:= 0 ->
                      T #= I*Mult0
              ;
                      T #= I*Mult1
              )
        ),
        sum(TT)#=TSum,

        % check digit
        nth1(N,ISBN, Check),
        % since ic don't have support for modulo we use suspend
        suspend(Check is (10 - TSum mod 10) mod 10, 0, [TSum->inst,Check->inst]),


        term_variables([ISBN,TT,Mult0,Mult1,TSum],Vars),
        % labeling(Vars),
        search(Vars,0,first_fail,indomain_min,complete,[backtrack(Backtracks)]),

        writeln(isbn:ISBN),
        writeln(check:Check),
        writeln(tt:TT),
        writeln([t_sum:TSum,mult0:Mult0,mult1:Mult1]),
        writeln(backtracks:Backtracks),
        fail.
        