/*

 
  Test of global constraint cumulative in ECLiPSe.
 
  cumulative is defined in globals.mzn (in the MiniZinc package).
 
  Example from Global constraints catalogue
   http://www.emn.fr/x-info/sdemasse/gccat/sec4.71.html
  """
  cumulative<U+200B>(TASKS,<U+200B>LIMIT)
 
  Purpose
 
  Cumulative scheduling constraint or scheduling under resource constraints.
  Consider a set T of tasks described by the TASKS collection. The cumulative
  constraint enforces that at each point in time, the cumulated height of
  the set of tasks that overlap that point, does not exceed a given limit.
  It also imposes for each task of T the constraint origin+duration=end.
 
  Example
     (
     <
      origin-1  duration-3  end-4   height-1,
      origin-2  duration-9  end-11  height-2,
      origin-3  duration-10 end-13  height-1,
      origin-6  duration-6  end-12  height-1,
      origin-7  duration-2  end-9   height-3
      >,8
      )
 
  Figure 4.71.1 [see the web page] shows the cumulated profile associated with
  the example. To each task of the cumulative constraint corresponds a set of
  rectangles coloured with the same colour: the sum of the lengths of the
  rectangles corresponds to the duration of the task, while the height of the
  rectangles (i.e., all the rectangles associated with a task have the same
  height) corresponds to the resource consumption of the task. The cumulative
  constraint holds since at each point in time we don't have a cumulated
  resource consumption strictly greater than the upper limit 8 enforced by
  the last argument of the cumulative constraint.
  """
 
  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:- lib(ic).
:- lib(ic_edge_finder).% for cumulative/4
:- lib(branch_and_bound).% for minimize/2

go :-       
        Len = 5,
        length(LS,Len),
        length(LD,Len),
        length(LR,Len),
        LS = [1, 2, 3, 6, 7], % start times
        LS :: 1..10,
        LD = [3, 9,10, 6, 2],% duration times
        LR = [1, 2, 1, 1, 3],% resource used by tasks
        % LE = [4,11,13,12, 9],

        Limit :: 1..8, % we can't use more than 8 resources at the
                       % same time. In fact, we only use 7 in this
                       % example 

        % setup a list of end times LE
        ( foreach(S,LS),foreach(D,LD),foreach(K,LE) do
              K #= S+D
        ),

        End #= max(LE),% latest end time of all tasks

        cumulative(LS, LD, LR, Limit),

        % note: Limit must also be labeled.
        term_variables([LS,LE, Limit], Vars),

        minimize(labeling(Vars), Limit),

        writeln("start   ":LS),
        writeln(duration:LD),
        writeln(resource:LR),
        writeln("end     ":LE),
        writeln("limit ":Limit),
        writeln(max_end_time:End).

