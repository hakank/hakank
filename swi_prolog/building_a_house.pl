/*

  Scheduling (Building a house) in SWI Prolog

  This model is adapted OPL model sched_intro.mod (examples).
  """
  This is a basic problem that involves building a house. The masonry,
  roofing, painting, etc.  must be scheduled. Some tasks must
  necessarily take place before others, and these requirements are
  expressed through precedence constraints.
  """

  The OPL solution is
  """
  Masonry  : 0..35
  Carpentry: 35..50
  Plumbing : 35..75
  Ceiling  : 35..50
  Roofing  : 50..55
  Painting : 50..60
  Windows  : 55..60
  Facade   : 75..85
  Garden   : 75..80
  Moving   : 85..90
  """

  When minimizing MakeSpan, this model give the following solution:
        makeSpan=90
        z=21000
        limit=3
        masonry    :   0.. 35
        carpentry  :  35.. 50
        plumbing   :  35.. 75
        ceiling    :  35.. 50
        roofing    :  50.. 55
        painting   :  50.. 60
        windows    :  55.. 60
        facade     :  75.. 85
        garden     :  75.. 80
        moving     :  85.. 90

  When minimizing Z the result is:

        makeSpan=110
        z=5000
        limit=3
        masonry    :  20.. 55
        carpentry  :  75.. 90
        plumbing   :  55.. 95
        ceiling    :  75.. 90
        roofing    :  90.. 95
        painting   :  90..100
        windows    : 100..105
        facade     :  95..105
        garden     :  95..100
        moving     : 105..110

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Compare different limits (1..3) in cumulative/2.
%% and type of minimization (MakeSpan or Z).
%% 
%% Here's the make span, z, and time for each combination of limit and minimization type:
%%
%% limit=1, mintype=min_z
%% 18,891,247 inferences, 1.067 CPU in 1.067 seconds (100% CPU, 17707584 Lips)
%% makeSpan=145
%% z=23000
%%
%% limit=1, mintype=min_makespan
%% 18,620,029 inferences, 1.086 CPU in 1.086 seconds (100% CPU, 17152053 Lips)
%% makeSpan=145
%% z=23000
%%
%% limit=2, mintype=min_z
%% 4,457,223,913 inferences, 247.997 CPU in 247.996 seconds (100% CPU, 17972884 Lips)
%% makeSpan=110
%% z=8500
%%
%% limit=2, mintype=min_makespan
%% 799,870,535 inferences, 47.440 CPU in 47.440 seconds (100% CPU, 16860746 Lips)
%% makeSpan=95
%% z=19000
%%
%% limit=3, mintype=min_z
%% 133,286,428 inferences, 7.421 CPU in 7.421 seconds (100% CPU, 17961198 Lips)
%% makeSpan=110
%% z=5000
%%
%% limit=3, mintype=min_makespan
%% 3,666,411 inferences, 0.276 CPU in 0.276 seconds (100% CPU, 13306734 Lips)
%% makeSpan=90
%% z=21000
%%
go :-
        between(1,3,Limit),

        %% The type to minimize
        member(MinType,[min_z,min_makespan]),
        writeln([limit=Limit, mintype=MinType]),
        
        tasks(Tasks),
        length(Tasks,NumTasks),
        tasks2(Map),
        
        duration(Duration),
        precedences(Precedences),
        time(once(building_a_house(Tasks,Map,Precedences,Duration,Limit,MinType, Start, End, Z, MakeSpan))),

        %% Solution
        writeln(start=Start),
        writeln(end=End),
        writeln(makeSpan=MakeSpan),
        writeln(limit=Limit),
        writeln(z=Z),

        findall([T,S,E],
                (between(1,NumTasks,I),
                 nth1(I,Tasks,T),
                 nth1(I,Start,S),
                 nth1(I,End,E)
                ),
                Sol),
        maplist(format("~t~w~20|~t~w~25|..~t~w~28|~n"),Sol),

        nl,
        fail,
        
        nl.

go.

building_a_house(Tasks,Map,Precedences, Duration,Limit,MinType, Start, End, Z, MakeSpan) :-


        length(Tasks,NumTasks),

        sum(Duration,#=,TotalDuration),
        
        %% Height = [1 : _I in 1..NumTasks],
        findall(1,between(1,NumTasks,_),Height),
  

        %%
        %% decision variables
        %%
        length(Start,NumTasks),  
        Start ins 0..TotalDuration,

        length(End,NumTasks),  
        End ins 0..TotalDuration,

        MakeSpan in 1..TotalDuration,

        %%
        %% constraints
        %%
        
        %% cumulative
        maplist(create_task,Start,Duration,Height,CumulativeTasks,End),
        cumulative(CumulativeTasks,[limit(Limit)]),

        % makespan
        max_list_clp(End,MakeSpan),

        %% This is not needed since we calculate End in create_task/5.
        %% maplist(sums,Start,Duration,End),
        
        %% precedences
        findall([PP1,PP2],
                (
                 member([P1,P2],Precedences),
                 member(P1=PP1,Map),
                 member(P2=PP2,Map)
                ),
                Ps),
        maplist(prec(Start,Duration),Ps),

        Z #> 0,
        calc_z(Map,Start,End,Z),

        flatten([Start,End],Vars),
        (
         MinType == min_makespan
        ->
         labeling([ffc,enum,min(MakeSpan)], Vars) % minimizing MakeSpan
        ; 
         labeling([ffc,enum,min(Z)], Vars)  % minimizing Z
        ).


sums(Start,Duration,End) :-
        End #= Start + Duration.
               
%%
%% Z #=  400 * max(End[Map.get(moving)]- 100, 0) +
%% 200 * max(25 - Start[Map.get(masonry)], 0)   +
%% 300 * max(75 - Start[Map.get(carpentry)], 0) +
%% 100 * max(75 - Start[Map.get(ceiling)], 0),
%%
calc_z(Map,Start,End,Z) :-
        member(ceiling=CeilingIx,Map),
        member(moving=MovingIx,Map),
        member(masonry=MasonryIx,Map),
        member(carpentry=CarpentryIx,Map),

        element(MovingIx,End,MovingVal),
        Moving #= max(MovingVal-100,0),

        element(MasonryIx,Start,MasonryVal),
        Masonry #= max(25-MasonryVal,0),
       
        element(CarpentryIx,Start,CarpentryVal),
        Carpentry #= max(75-CarpentryVal,0),
        
        element(CeilingIx,Start,CeilingVal),
        Ceiling #= max(75-CeilingVal,0),
        
        Z #= 400*Moving + 200*Masonry + 300 * Carpentry + 100 * Ceiling,
             
        nl.
        

         
%
% Predence:
%   Task X (Start[X]+Duration[X]) must precede task Y (Start[Y]).
% 
prec(Start, Duration, [X, Y]) :-
        element(X,Start,StartX),
        element(X,Duration,DurationX),
        element(Y,Start,StartY),
        StartX + DurationX #=< StartY.


tasks(Tasks) :- 
  Tasks = [masonry,carpentry,plumbing,ceiling,roofing,painting,windows,facade,garden,moving].

% For indices in the constraints
tasks2(Tasks) :- 
        Tasks =
        [masonry=1,
         carpentry=2,
         plumbing=3,
         ceiling=4,
         roofing=5,
         painting=6,
         windows=7,
         facade=8,
         garden=9,
         moving=10].


duration([35,15,40,15, 5,10, 5,10, 5, 5]).


%
% The precedences: Task1 must be done before Task2.
%
precedences(Precedences) :-
        Precedences = [
                       [masonry,   carpentry], 
                       [masonry,   plumbing], 
                       [masonry,   ceiling],
                       [carpentry, roofing],
                       [ceiling,   painting],
                       [roofing,   windows],
                       [roofing,   facade],
                       [plumbing,  facade],
                       [roofing,   garden],
                       [plumbing,  garden],
                       [windows,   moving],
                       [facade,    moving],
                       [garden,    moving],
                       [painting,  moving]
].
