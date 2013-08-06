/*

  Train example in SICStus Prolog.

  This is a simple example using table/2. From SWI prolog manual
  http://www.swi-prolog.org/pldoc/doc_for?object=section%282%2c%20%27A.7%27%2c%20swi%28%27%2fdoc%2fManual%2fclpfd.html%27%29%29
  """
  As another example, consider a train schedule represented as a list
  of quadruples, denoting departure and arrival places and times for each 
  train. In the following program, Ps is a feasible journey of length 
  3 from A to D via trains that are part of the given schedule.
  """

  Answer:
  [[1,2,0,1],[2,3,4,5],[3,4,8,9]]

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        threepath(1, 4, Ps,TotalTime),
        write(Ps),nl,
        write(total_time:TotalTime),nl,
        fd_statistics.


trains([[1,2,0,1],[2,3,4,5],[2,3,0,1],[3,4,5,6],[3,4,2,3],[3,4,8,9]]).
 
threepath(A, D, Ps,TotalTime) :-
        Ps = [[A,B,_T0,T1],[B,C,T2,T3],[C,D,T4,_T5]],
        T2 #> T1,
        T4 #> T3,
        trains(Ts),
        % tuples_in(Ps, Ts). % SWI Prolog syntax
        table(Ps, Ts),

        % added total time
        ( foreach(P,Ps),
          fromto(0,In,Out,TotalTime) do
              P = [_,_,Start,End],
              Out #= In + End - Start
        ).
