/*

  Who killed the Bosmer logic puzzle in SWI Prolog

  From https://swi-prolog.discourse.group/t/similar-einstein-riddle/5142/6
  """
  A Bosmer, was slain. The Altmer claims the Dunmer is guilty. The Dunmer says the Khajiit did it. 
  The Orc swears he didn’t kill the Bosmer. The Khajiit says the Dunmer is lying. If only one of 
  these speaks the truth, who killed the Bosmer?""
  """

  Cf http://hakank.org/picat/who_killed_the_bosmer.pi
     http://hakank.org/webppl/who_killed_the_bosmer.wppl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).

go :- 
          L = [altmer,dunmer,orc,khajiit],

          % Who speaks the truth?
          SpeaksTruth = [AltmerT,DunmerT,OrcT,KhajiitT],
          SpeaksTruth ins 0..1,

          % Who is guilty?
          Guilty = [_AltmerG,DunmerG,OrcG,KhajiitG],
          Guilty ins 0..1,
   
          % A Bosmer, was slain.
  
          % The Altmer claims the Dunmer is guilty.
          AltmerT #<==> DunmerG,

          % The Dunmer says the Khajiit did it.
          DunmerT #<==> KhajiitG,
  
          % The Orc swears he didn’t kill the Bosmer.
          OrcT #<==> (OrcG #= 0),
  
          % The Khajiit says the Dunmer is lying.
          KhajiitT #<==> (DunmerT #= 0),

          % If only one of these speaks the truth, who killed the Bosmer?""
          sum(SpeaksTruth,#=,1),

          % Only one is is guilty
          sum(Guilty, #=,1),

          append(SpeaksTruth,Guilty,Vars),
          label(Vars),
          writeln(speaks_truth=SpeaksTruth),
          writeln(guilty=Guilty),
          nl,
          maplist(print_solution,SpeaksTruth,Guilty,L),
          fail,
          nl.
go.


print_solution(SpeaksTruth,Guilty,T) :-
        Guilty      == 1 -> format("Guilty: ~w~n",[T]) ; true,
        SpeaksTruth == 1 -> format("Speaks truth: ~w~n",[T]) ; true.
        