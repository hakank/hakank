/*

  Organizing a day in B-Prolog.

  Problem formulation:
  Slides on (finite domain) Constraint Logic Programming, page 38f


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 4,
        TasksStr = ['Work','Mail','Shop','Bank'],
        length(Begins,N),
        Begins :: 9..17,
        length(Ends,N),
        Ends   :: 9..17,
        durations(Durations),
        before_tasks(BeforeTasks),

        findall([Begins,Ends], 
                organize(Durations,BeforeTasks,Begins, Ends),
                L),
        foreach([Begins,Ends] in L,
                (foreach(Task in 1..N,[B,E,S],
                         (
                             S @= TasksStr[Task],
                             B @= Begins[Task],
                             E @= Ends[Task],
                             format("~w: ~2d .. ~2d\n",[S,B,E])
                         )),
                 nl
                )),
        nl.


organize(Durations,BeforeTasks,Begins,Ends) :-
        N = 4, % number of tasks

        foreach(I in 1..N,
                Ends[I] #= Begins[I] + Durations[I]
               ),

        % foreach(I in 1..N, J in I+1..N,
        %         no_overlap(Begins,Durations,I,J)
        %        ),

        % no_overlaps is better written during the built-in constraint
        % serialized:
        serialized(Begins,Durations),

        % and serialized is in turn a special case of cumulative:
        % Ones @= [1 : I in 1..N],
        % cumulative(Begins,Durations,Ones, 1),

        % handle precendeces
        foreach([A,B] in BeforeTasks,
                Ends[A] #=< Begins[B]
               ),

        % Work >= 11 a clock
        Begins[1] #>= 11,

        term_variables([Begins,Ends], Vars),
        labeling(Vars).


no_overlap(Begins,Durations,I,J) :-
        (Begins[I] + Durations[I] #=< Begins[J])
        #\/
        (Begins[J] + Durations[J] #=< Begins[I]).
        
        
        
% duration of the four tasks
durations([4,1,2,1]).

% precedences
% [A,B] : task A must be completed before task B
before_tasks([[4,3],
              [2,1]]).