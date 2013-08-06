/*

  Some explorations of ISBN13 in B-Prolog.

  See http://en.wikipedia.org/wiki/ISBN

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


% Test ISBN:
% 978-0262720304: The OPL Optimization Programming Language 
% [9,7,8,0,2,6,2,7,2,0,3,0]
%
% isbn = 978-0262220774: Constraint-based Local Search
% [9,7,8,0,2,6,2,2,2,0,7,7];


go :-
        N = 13,
        % Mult0 = 3,
        % Mult1 = 1,
        isbn(N, ISBN,Mult0,Mult1,Check),
        writeln(isbn:ISBN),
        writeln(check:Check),
        writeln([mult0:Mult0,mult1:Mult1]),
        nl,
        fail.
        


isbn(N, ISBN,Mult0,Mult1,Check) :-
        
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
        % ISBN = [9,7,8,0,2,6,2,7,2,0,3,0,_], % get check digit
        % ISBN = [9,7,8,0,2,_,2,7,2,0,3,0,4], % get some other digit
        ISBN = [9,7,8,0,2,6,2,_,_,_,_,_,_],

        % ISBN starts with 978 or 979
        ISBN[1] #= 9,
        ISBN[2] #= 7,
        ISBN[3] #>=8,

        % Prepare for the check sum
        foreach((T,C) in (TT,1..N1),[I],
                (element(C,ISBN,I),
                 (C mod 2 =:= 0 -> T #= I*Mult0 ; T #= I*Mult1)
                )),

        sum(TT)#=TSum,

        % check digit
        nth1(N,ISBN, Check),
        Check #= (10 - TSum mod 10) mod 10,

        term_variables([ISBN,TT,Mult0,Mult1,TSum],Vars),
        labeling([ff],Vars).


        