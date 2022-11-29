/*

  Mamas age problem in SWI Prolog

  Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
  """
  Tommy: "How old are you, mamma?"
  Mamma: "Our three ages add up to exactly seventy years."
  Tommy: "And how old are you, papa?"
  Papa: "Just six times as old as you, my son."
  Tommy: "Shall I ever be half as old as you, papa?"
  Papa: "Yes, Tommy; and when that happens our three ages will add up to
  exactly twice as much as today."
  
  Can you find the age of Mamma?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go:-
        mama([M,P,T]),
        % Convert months to years.
        MA is M // 12,
        PA is P // 12,
        TA is T // 12,
        writeln([mama:M=MA,papa:P=PA,tommy:T=TA]),
        fail,
        nl.

mama(LD) :-
        LD = [M,P,T],
        LD ins 0..500, % in months
        M + P + T #= 70 * 12,
        6 * T #= P,
        (T + I) * 2 #= P + I,
        M + I + P + I + T + I #= 2 * (M + P + T),
        
        label(LD).
        

