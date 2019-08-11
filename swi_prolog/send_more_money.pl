/*

  SEND+MORE=MONEY puzzle in SWI Prolog

  Solve the alphametic problem 
    SEND+MORE=MONEY
  using distinct digits for the letters.

  Some CLP and non-CLP approaches.
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

% 
% Plain CLP. 0.003s
% 
go :- 
        send_more_money(Digits),
        writeln(digits_before=Digits),
        list_domains(Digits,Domains),
        writeln(domains=Domains),
        labeling([ff],Digits),
        writeln(Digits),       
        nl.

%
% using scalar_product/4: 0.007s
% 
go2 :- 
        send_more_money_scalar_product(Digits),
        writeln(Digits),
        list_domains(Digits,Domains),
        writeln(domains=Domains),        
        labeling([ff],Digits),
        writeln(Digits),       
        nl.

%
% With carry: 0.005s
% 
go3 :- 
        send_more_money_with_carry(Digits, Carry),
        writeln(digits=Digits),
        writeln(carry=Carry),
        append(Digits,Carry, Vars),
        labeling([ff],Vars),
        writeln(digits=Digits),
        writeln(carry=Carry),
        
        nl.

%
% No CLPFD, using select. TOO SLOW!
% 
go4 :- 
        time(send_more_money_no_cp1(Digits)),
        writeln(Digits),
        
        nl.

%
% No CLPFD, using assign/select. 1.7s.
% 
go5 :- 
        time(send_more_money_no_cp2(Digits)),
        writeln(Digits),
        
        nl.

%
% No CLPFD, using permutation/2.
% 
go6 :- 
        time(send_more_money_permutation(Digits)),
        writeln(Digits),
        
        nl.


%
% Plain CLP.
%
send_more_money(Digits) :-
        Digits = [S,E,N,D,M,O,R,Y],
        Digits ins 0..9,

        all_distinct(Digits),
        S #> 0,
        M #> 0,
               1000*S + 100*E + 10*N + D
        +      1000*M + 100*O + 10*R + E
        #= 10000*M + 1000*O + 100*N + 10*E + Y.


%
% Using scalar_product.
%
send_more_money_scalar_product(Digits) :-

  Digits = [S,E,N,D,M,O,R,Y],
  Digits ins 0..9,

  all_distinct(Digits),

  S #> 0,
  M #> 0,

  Base4 = [1000,100,10,1],
  Base5 = [10000,1000,100,10,1],
  scalar_product(Base4, [S,E,N,D], #=, SEND),
  scalar_product(Base4, [M,O,R,E], #=, MORE),
  scalar_product(Base5, [M,O,N,E,Y], #= , MONEY),
  SEND + MORE #= MONEY.


%
% With carry
%
%   C4 C3 C2 C1
%       S  E  N  D
%  +    M  O  R  E
%  ---------------
%  = M  O  N  E  Y
% 
send_more_money_with_carry(Digits, Carry) :-
  Digits = [S,E,N,D,M,O,R,Y],
  Digits ins 0..9,

  Carry = [C1,C2,C3,C4],
  Carry ins 0..1,

  all_distinct(Digits),

  S #> 0,
  M #> 0,
  
  C4 #= M,
  C3 + S + M #= O + 10 * C4,
  C2 + E + O #= N + 10 * C3,
  C1 + N + R #= E + 10 * C2,
  D + E #= Y + 10 * C1.


%
% Without CLP, using select. TOO SLOW
%
send_more_money_no_cp1(Digits) :-
  Digits = [S,E,N,D,M,O,R,Y],
  between(0,9,S),
  select(S,Digits,Rest1),
  S #\= 0,
  between(0,9,E),
  select(E,Rest1,Rest2),
  between(0,9,N) , 
  select(N,Rest2,Rest3),
  between(0,9,D),
  select(D,Rest3,Rest4),
  between(0,9,M),
  select(M,Rest4,Rest5),
  M #\= 0,
  between(0,9,O),
  select(O,Rest5,Rest6),
  between(0,9,R),
  select(R,Rest6,Rest7),
  between(0,9,Y),   
  select(Y,Rest7,_Rest8),

  (S * 1000 + E * 100 + N * 10 + D) +
  (M * 1000 + O * 100 + R * 10 + E) #= 
  (M * 10000 + O * 1000 + N * 100 + E * 10 + Y ).


%
% Variant where the selects are moved to assign/2
% Much faster than send_more_money_no_cp2: 1.7s
% 
send_more_money_no_cp2(Digits) :-
  Digits = [S,E,N,D,M,O,R,Y],
  assign([0,1,2,3,4,5,6,7,8,9],Digits),
  S \= 0,
  M \= 0,
  (S * 1000 + E * 100 + N * 10 + D) +
  (M * 1000 + O * 100 + R * 10 + E) =:= 
  (M * 10000 + O * 1000 + N * 100 + E * 10 + Y ).

%
% assign( Digits, Vars)
% Assign chosen distinct digits from list Digits to variables in list Vars
%
assign(_,[]).
assign(Digs,[D|Vars])  :-
  select(D, Digs,Digs1),
  assign(Digs1,Vars).

%
% Without CLP, using permutation/2: ~3.0s
% 
send_more_money_permutation(Digits2) :-
  Digits = [S,E,N,D,M,O,R,Y, A,B],
  permutation([0,1,2,3,4,5,6,7,8,9],Digits),
  A < B, % symmetry breaking of the two unused digits
  S > 0, M #> 0,
  (S * 1000 + E * 100 + N * 10 + D) +
  (M * 1000 + O * 100 + R * 10 + E) =:= 
  (M * 10000 + O * 1000 + N * 100 + E * 10 + Y),
  Digits2 = [S,E,N,D,M,O,R,Y].
