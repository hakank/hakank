/*

  Bin packing in B-Prolog.

  Simple bin packing problems
  This model is based on the ECLiPSe and SICStus Prolog models.

  (Mats Carlsson suggested the solution in bin_packing2/1.)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


% sorted Stuff: very slow for problem 9
go :-
        foreach(P in 0..9, time2(bin_packing(P,sorted))).


% Unsorted Stuff: very slow for problem 9
go2 :-
        foreach(P in 0..9, time2(bin_packing(P,unsorted))).

% In all very slow...
go3 :-
        foreach(P in 0..9, time2(bin_packing2(P))).


bin_packing(Problem, SortMode) :-

        problem(Problem, StuffUnordered, BinCapacity),

        % sorting (reversed) the stuff may makes it a bit faster
        (
            SortMode = sorted 
        -> 
            sort(>=,StuffUnordered,Stuff)
        ;
            Stuff = StuffUnordered
        ),

        format('\nProblem ~w\n',[Problem]),

        % BinCapacity: the (common) capacity for each bin
        write(bin_capacity:BinCapacity),nl,

        % Stuff: values/weight of the things to pack
        length(Stuff,NumStuff), 
        write(stuff:Stuff),nl,
        write(num_stuff:NumStuff),nl,

        % Number of bins cannot exceed num_stuff...
        NumBins #= NumStuff,

        % Sanity clause: 
        %    No thing can be larger than capacity.
        foreach(S in Stuff, 
                (
                    S > BinCapacity 
                -> 
                    format('Stuff ~d is larger than BinCapacity ~d\n', [S, BinCapacity])
                ;
                    true
                )
               ),
        
        % Bins: where to put a thing.
        new_array(Bins,[NumBins,NumStuff]),
        term_variables(Bins,BinsList),
        BinsList :: 0..1,        

        % BinLoads: contains how many things (the summed 
        %           weights of) a bin takes
        length(BinLoads,NumBins),
        BinLoads :: 0..BinCapacity,
        BinRows @= Bins^rows,
        foreach((Bin,Load) in (BinRows,BinLoads),
                scalar_product(Stuff,Bin,#=,Load)
        ),

        % a thing is packed exactly once 
        BinsTransposed @= Bins^columns,        
        foreach(Column in BinsTransposed, sum(Column) #= 1),

        % compare the total loads
        sum(Stuff) #= sum(BinLoads),

        % load the bins in order:
        % first bin must be loaded, and the list must be ordered
        % element(1, BinLoads, BinLoads1),
        % BinLoads1 #> 0,
        BinLoads[1] #> 0,
        % decreasing(BinLoads),

        % symmetry breaking: 
        %    if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0
        % (This seems to be better than using decreasing/1.)
        foreach(B in 1..NumBins-1,
                (
                    BinLoads[B+1] #>0 #=> BinLoads[B] #>0,
                    BinLoads[B] #>= BinLoads[B+1]
                )
               ),

        % calculate the number of loaded bins (which we will minimize)
        NumLoadedBins #= sum([(BL #> 0) : BL in BinLoads]),

        %
        % search
        %
        writeln(search),
        % term_variables([BinsList,BinLoads,NumBins,NumLoadedBins],Vars),
        term_variables([Bins,BinLoads],Vars),
        % minof(labeling([inout,split],Vars), NumLoadedBins,
        minof(labeling([inout],Vars), NumLoadedBins,
              format("Obj: NumLoadedBins: ~d\n", [NumLoadedBins])),

        % term_variables([Bins],Vars2),
        % labeling([inout], Vars2),

        % output
        writeln('...'),
        writeln(bin_loads:BinLoads),
        writeln(num_loaded_bins:NumLoadedBins),
        writeln(numBins:NumBins),
        % just print smaller problems
        (
            NumBins =< 40 -> 
                % pretty_print(Bins) 
                pretty_print2(Bins) 
        ; 
                writeln('Bins is too large to print.'), true
        ),
        nl.


%
% Using cumulative for this problem was suggested by Mats Carlsson
% (and it is very fast in the SICStus prolog version).
%
% However, the implementation has been rewritten trying to keep
% the same approach. However, this is (much) slower than bin_packing/1. 
% The correct MaxStart value is mostly found very fast 
% (including for #9), but it then takes a long time to prove 
% its optimality. I might have missed something here...
%
bin_packing2(Problem) :-

        problem(Problem, StuffUnordered, BinCapacity),
        sort(>=,StuffUnordered,Stuff),
        % sort(=<,StuffUnordered,Stuff),
        % Stuff = StuffUnordered,
     
        length(Stuff, N),
        format("\nProblem ~d BinCapacity:~d N:~d\n", [Problem,BinCapacity,N]),
        writeln(stuff:Stuff),

        Duration @= [1 : _I in 1..N],
        Resources = Stuff,
        Limit #= BinCapacity,
        length(Start, N),
        Start :: 1..BinCapacity,
        length(End, N),
        End :: 1..BinCapacity,

        % to minimize
        MaxStart #= max(Start),
        foreach((S,D,E) in (Start,Duration,End), E #= S+D), 

        cumulative(Start,Duration,Resources,Limit),

        term_variables([Start], Vars),
        minof(labeling([forward,split],Vars),MaxStart,
              format("MaxStart: ~d  Start: ~w\n",[MaxStart,Start])),

        nl,
        writeln(maxStart:MaxStart),
        writeln(start:Start),
        writeln(duration:Duration),
        writeln(end:End),
        writeln(limit:Limit),
        writeln(stuff:Stuff),
        writeln(binCapacity:BinCapacity),

        foreach((S,E,T) in (Start,End,Stuff),
                writeln([bin:S,stuff:T])
                ),
        nl.



%
% pretty prints the Bin matrix.

pretty_print(X) :-
        Rows @= X^rows,
        foreach(Row in Rows,[Sum],
                (Sum #= sum(Row),
                 foreach(R in Row, format('~d ', [R])),
                 format(" = ~d\n", [Sum])
                )
               ).

% just print the filled bins
pretty_print2(X) :-
        Rows @= X^rows,
        foreach(Row in Rows,[Sum],
                (Sum #= sum(Row),
                 Sum > 0 -> 
                     (
                         foreach(R in Row, format('~d ', R)),
                         format(" = ~d\n", [Sum])
                     )
                ;
                     true
                )
        ).

decreasing(List) :-
        foreach(I in 2..List^length, List[I-1] #>= List[I]).

increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).


time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


%
% Data
%        

% easy problem to test with
problem(0, 
        [1,2,3,4,5,6,7,8,9,10],
        30).


%
% Example from the Alice system
% Copying files to disks
% http://news.mozart-oz.org/alice/manual/cptutorial/node55.html
%
% """
% Suppose, you want to copy a set of files from your hard-disk onto as 
% few as possible diskettes of a given size, e. g. onto common 1.44 MB 
% diskettes. In case your files do not fit on a single diskette, it might 
% become quite tricky to figure out the minimal number of needed diskettes 
% and how to partition the files.
% """
problem(1, 
        [360, 850, 630, 70, 700, 210], 
        1440).


% simple (and probably unrealistic) packing
problem(2, 
        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
        20).


% simple (and probably even less unrealistic) packing
problem(3, 
        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],
        50).


% This problem below is from
% http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
% 1D Bin Packing (or "CP? Who cares?"), page 3
% and also from
% http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
%
% num_stuff = 10;
% stuff = [42,63,67,57,93,90,38,36,45,42];
% bin_capacity = 150;
problem(4, 
        [42,69,67,57,93,90,38,36,45,42], 
        150).


% same source of data, but now there are 22 things
problem(5, 
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33],
        250).


%
% continuing of the above example.
%
problem(6, 
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,
         30,64,29,74,41,49,55,98,80,32,25,38,82,30], 
        290).


% ibid. 
problem(7,
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,
         85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,
         41,49,55,98,80,32,25,38,82,30,35,39,57,84,62,50,55,27,30,36,
         20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,
         71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,
         94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,
         39],
        500).


% From 
% Graham Kendall: Bin Packing made Easier 
% http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
problem(8,
        [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
        252,252,127,85,12,10,9,252,127,106,84,12,10,252,
        127,106,46,12,10],
        524).


% Variant: remove 46 from the problem above
problem(9,
        [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
        252,252,127,85,12,10,9,252,127,106,84,12,10,252,
        127,106,12,10],
        524).
