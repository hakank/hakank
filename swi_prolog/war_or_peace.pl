/*

  War or Peace problem in SWI Prolog

  From the Alma0 model war_or_peace.a0  
  http://www.cwi.nl/en/alma
  """
  There are N countries.
  Each pair of two countries is either at war or has a peace treaty.
  Each pair of two countries that has a common enemy has a peace treaty.
  What is the minimum no of peace treaties?
  """
  
  Note: 
  For 8 countries there are 35 solutions with the minimum number of peace treaties 12.

  The minimum number of peace treaties for N=2.12. seems to be 
  https://oeis.org/A002620
  Quarter-squares: floor(n/2)*ceiling(n/2). Equivalently, floor(n^2/4). 

  0, 1, 2, 4, 6, 9, 12, 16, 20, 25, 30, 36, 42, 49, 56, 64, 72, 81,..


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% 8 countries.
%
go :-
        writeln("Find the minimum for 8 countries:"),
        N = 8,
        time(once(war_and_peace(N,_X, CountPeaces))),
        writeln(minumum=CountPeaces),

        writeln("\nFind all solution with minimal peace treaties"),
        time(once(findall(X2,war_and_peace(N,X2, CountPeaces),All))),
        length(All,Len),
        writeln(len=Len),
        maplist(print_matrix,All),
        writeln(len=Len),
        nl.

%
% General solution: Check 2..n countries.
%
% N  2 3 4 5 6 7  8  9 10 11 12 13 14 15 16
%   [0,1,2,4,6,9,12,16,20,25,30,36,42,49,56]
%
go2 :-
        between(2,15,N),
        writeln(n=N),
        time(once(war_and_peace(N,X, CountPeaces))),
        print_matrix(X),
        writeln(minimum=CountPeaces),
        nl,
        fail,
        nl.

go2.

war_and_peace(N, X, CountPeaces) :-
        War = 0,
        Peace = 1,
        new_matrix(N,N,War..Peace,X),
        flatten(X,Vars),

        N2 #= N*N,
        CountPeaces in 1..N2,
        sum(Vars,#=,CountPeaces),

        N1 #= N-1,
        numlist(2,N1,Is),
        maplist(wop_loop(War,Peace,X,N),Is),
        
        (var(CountPeaces)
        ->
         labeling([ffc,min(CountPeaces)],Vars)
        ;
         labeling([enum],Vars)
        ).



wop_loop(War,Peace,X,N,I) :-
        I1 #= I+1,
        numlist(I1,N,Js),
        maplist(wop_loop_(War,Peace,X,I),Js).

wop_loop_(War,Peace,X,I,J) :-
        matrix_element(X,I,J,XIJ),
        I1 #= I-1,
        numlist(1,I1,Ks),
        sum_line(Ks,X,I,J,0,SumLine),
        (
         (
          XIJ #= War #/\ SumLine #= I1
         )
        #\/
        (
         XIJ #= Peace
        )
        ).


sum_line([],_X,_I,_J,Sum,Sum).
sum_line([K|Ks],X,I,J,Sum0,Sum) :-
        matrix_element(X,K,I,XKI),
        matrix_element(X,K,J,XKJ),
        B in 0..1,
        %% (XKI #= Peace #\/ XKJ #= Peace) #<==> B #= 1,
        XKI + XKJ #> 0 #<==> B #= 1,        
        Sum1 #= Sum0 + B,
        sum_line(Ks,X,I,J,Sum1,Sum).
        
