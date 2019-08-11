/*

  Some explorations of ISBN13 in SWI Prolog

  See http://en.wikipedia.org/wiki/ISBN

  Test ISBN:
  978-0262720304: The OPL Optimization Programming Language 
  [9,7,8,0,2,6,2,7,2,0,3,0]
 
  isbn = 978-0262220774: Constraint-based Local Search
  [9,7,8,0,2,6,2,2,2,0,7,7];

  Constraint Solving and Planning with Picat
  book: http://www.springer.com/gp/book/9783319258812
  [9,7,8,3,3,1,9,2,5,8,8,1,2]
  Ebook: [9,7,8,3,3,1,9,2,5,8,8,3,6]



  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 13,
        
        %% test ISBNs
        %% ISBN = [9,7,8,0,2,6,2,7,2,0,3,0,_], % get check digit
        %% ISBN = [9,7,8,0,2,_,2,7,2,0,3,0,4], % get some other digit
        %% ISBN = [9,7,8,0,2,_,_,7,2,0,3,0,4], % get some other digit
        %% ISBN = [9,7,8,0,2,6,2,_,_,_,_,_,_],
        
        %% Picat book: http://www.springer.com/gp/book/9783319258812
        ISBN = [9,7,8,3,3,1,9,2,5,8,8,_,_],

        %% fixed Mult0 and Mult1
        Mult0 = 3,
        Mult1 = 1,
        isbn(N, ISBN,Mult0,Mult1,Check),
        writeln(isbn=ISBN),
        writeln(check=Check),
        writeln([mult0=Mult0,mult1=Mult1]),
        nl,
        fail,
        nl.
   

go.


%%
%% Simpler version, fixed Mult0 and Mult1
%%
go2 :- 
        %% The Picat book: http://www.springer.com/gp/book/9783319258812
        ISBN = [9,7,8,3,3,1,9,2,5,8,8,_,_],
        isbn2(ISBN),
        writeln(ISBN),
        fail,
        nl.

go2.


%%
%% Just check digits, but now with unfixed Mult0 and Mult1
%%
go3 :-
   N = 13,
   ISBN = [9,7,8,0,2,6,2,7,2,0,3,0,_], % get check digit

   isbn(N, ISBN,Mult0,Mult1,Check),
   writeln(isbn=ISBN),
   writeln(check=Check),
   writeln([mult0=Mult0,mult1=Mult1]),
   nl,
   fail,
   nl.


isbn(N, ISBN,Mult0,Mult1,Check) :-
        writeln(isbn(N, ISBN,Mult0,Mult1,Check)),
        
        length(ISBN,N),
        ISBN ins 0..9,

        Mult0 in 1..9,
        Mult1 in 1..9,

        %% Mult0 #\= Mult1, % extra constraint

        %% The first N-1 digits, for the check sum
        N1 #= N-1,
        length(TT,N1),
        TT ins 0..100,

        %% ISBN starts with 978 or 979
        element(1,ISBN,ISBN1),
        element(2,ISBN,ISBN2),
        element(3,ISBN,ISBN3),
        ISBN1 #= 9,
        ISBN2 #= 7,
        ISBN3 #>=8,


        
        %% Prepare for the check sum
        numlist(1,N1,Cs),
        zip2(TT,Cs,TCs),
        maplist(prepare_check_sum(ISBN,Mult0,Mult1),TCs),
        
        sum(TT,#=,TSum),

        %% check digit
        element(N,ISBN,ISBNN),
        Check #= ISBNN,
        Check #= (10 - TSum mod 10) mod 10,

        flatten([ISBN,TT,[Mult0,Mult1,TSum]],Vars),
        labeling([ff],Vars).

%%
%% Simpler version: fixed Mult0 and Mult1
%%
isbn2(ISBN) :-

        N=13,   
        length(ISBN,N),
        ISBN ins 0..9,
        
        %% The first N-1 digits, for the check sum
        N1 #= N-1,
        length(TT,N1),
        TT ins 0..100,

        %% ISBN starts with 978 or 979
        element(1,ISBN,ISBN1),
        element(2,ISBN,ISBN2),
        element(3,ISBN,ISBN3),
        ISBN1 #= 9,
        ISBN2 #= 7,
        ISBN3 #>=8,

        %% Prepare for the check sum
        numlist(1,N1,Cs),
        zip2(TT,Cs,TCs),
        maplist(prepare_check_sum2(ISBN),TCs),

        sum(TT,#=,TSum),
        element(N,ISBN,ISBNN),
        Check #= ISBNN,
        Check #= (10 - TSum mod 10) mod 10,

        flatten([ISBN,TT,TSum],Vars),
        labeling([ff],Vars).

%%
%% Prepare for the check sum
%%
prepare_check_sum(ISBN,Mult0,Mult1,[T,C]) :-
        element(C,ISBN,I),
        (
         C mod 2 #= 0
        ->
         T #= I*Mult0
        ;
         T #= I*Mult1
        ).


%%
%% Prepare for the check sum
%%
prepare_check_sum2(ISBN,[T,C]) :-
        element(C,ISBN,I),
        (
         C mod 2 #= 0
        ->
         T #= I*3
        ;
         T #= I*1
        ).
