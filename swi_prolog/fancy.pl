/*

  Mr Greenguest puzzle (Fancy dress) in SWI Prolog

  Problem (and LPL) code in
 
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
 
  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        %% T: Tie
        %% H: Hat
        %% R: Shirt
        %% S: Socks
        %% N: Entrance Fee
        LD = [T,H,R,S,N],
        LD ins 0..1,

        Cost in 0..10000,

        %% This is a straight translation from the LPL 
        ((T #=1)                     #==> (R #=1)) #\/ N#=1,
        ((S #=1 #\/ R#=1)            #==> (T#=1 #\/ H#=1)) #\/ N#=1,
        ((R#=1 #\/ H#=1 #\/ (S#\=1)) #==> T#=1) #\/ N#=1,
        Cost #= 10*T + 2*H + 12*S + 11*N,
        
        labeling([min(Cost)], LD),
        
        writeln(tie=T),
        writeln(hat=H),
        writeln(shirt=R),
        writeln(socks=S),
        writeln(entrance_fee=N),
        writeln(cost=Cost),nl.
