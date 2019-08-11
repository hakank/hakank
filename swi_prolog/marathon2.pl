/*

  Marathon puzzle in SWI Prolog

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  """
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.
  
     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 6,
   Names = ["Dominique", "Ignace", "Naren", "Olivier", "Philippe", "Pascal"],
   Runners = [Dominique, Ignace, Naren, Olivier, Philippe, Pascal],
   Runners ins 1..6,

   all_different(Runners),
  
   % a: Olivier not last
   Olivier #\= N,

   % b: Dominique, Pascal and Ignace before Naren and Olivier
   Dominique  #< Naren,
   Dominique  #< Olivier,
   Pascal     #< Naren,
   Pascal     #< Olivier,
   Ignace     #< Naren,
   Ignace     #< Olivier,
   
   % c: Dominique better than third
   Dominique  #< 3, 
   
   % d: Philippe is among the first four
   Philippe   #=< 4 ,
   
   % e: Ignace neither second nor third
   Ignace     #\= 2, 
   Ignace     #\= 3, 
   
   % f: Pascal three places earlier than Naren
   Pascal + 3 #= Naren, 
   
   % g: Neither Ignace nor Dominique on fourth position
   Ignace     #\= 4,
   Dominique  #\= 4,

   % For the presentation
   inverse(Runners, RunnersInv),

   label(Runners),

   format("runners   : ~w~n",[Runners]),
   format("assignment: ~w~n",[RunnersInv]),
   writeln("\nPlacings:\n"),
   findall([I-Name],
           (between(1,N,I),
            element(I,Runners,R),
            nth1(R,Names,Name)
           ),
           L),
   maplist(writeln,L),
   nl.
