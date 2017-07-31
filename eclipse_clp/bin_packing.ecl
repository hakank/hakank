/*

  Simple bin packing in ECLiPSe.

  Compare to the MiniZinc model http://www.hakank.org/minizinc/bin_packing.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
:-lib(propia).



go :-
        % Problems = [0,1,2,3,4,5,6,7,8,9],
        Problems = [0,1,2,3,4,5,6, 8,9], % 7 is hard
        ( foreach(P,Problems) do
              bin_packing(P)
        ). 


bin_packing(Problem) :-

        problem(Problem, StuffUnsorted, BinCapacity),

        % sort works only on lists so we convert to 
        % a list, and then back again
        collection_to_list(StuffUnsorted,StuffUnsortedList),
        sort(0,>=,StuffUnsortedList,StuffSorted),
        length(StuffUnsortedList,SLen),
        dim(Stuff,[SLen]),
        ( foreach(S,StuffSorted),
          foreachelem(S,Stuff) do
              true
        ),
        printf("\nProblem %w\n",[Problem]),
        writeln(stuff:Stuff),

        % BinCapacity: the (common) capacity for each bin
        writeln(bin_capacity:BinCapacity),

        % NumStuff: number of things to pack
        % Stuff: Values/Weight of the things to pack
        dim(Stuff,[NumStuff]), 
        writeln(num_stuff:NumStuff),

        % Number of bins cannot exceed num_stuff...
        NumBins = NumStuff,
        
        % Bins: Where to put a thing.
        %
        dim(Bins,[NumBins,NumStuff]),
        Bins :: 0..1,        

        % BinLoads: contains how many things (the summed weights of) a bin takes
        dim(BinLoads,[NumBins]),
        
        % sanity clause: No thing can be larger than capacity.
        %                This is needed since we must pack everything.
        ( for(S,1,NumStuff),
          param(Stuff, BinCapacity) do
              Stuff[S] > BinCapacity 
        -> 
          printf("Stuff %d is larger than BinCapacity %d\n", [S, BinCapacity])
        ;
          true
        ),

        % the total load in a bin cannot exceed BinCapacity
        (for(B,1,NumBins),
         param(BinLoads,Stuff,Bins,BinCapacity,NumStuff) do
             ( for(S,1,NumStuff),
               foreach(S1,SSum),
               param(Stuff,Bins,B) do
                   S1 = Stuff[S] * Bins[B,S]
             ),
             Sum #= sum(SSum),
             BinLoads[B] #>= 0,
             Sum #=< BinCapacity,
             BinLoads[B] #= Sum

        ),

        % calculate the total loads for the bins
        collection_to_list(Stuff, StuffList),
        collection_to_list(BinLoads,BinLoadsList),
        sum(StuffList) #= sum(BinLoadsList),
        
        % a thing is packed just once 
        (for(S,1,NumStuff),
         param(Bins,NumBins) do
             sum(Bins[1..NumBins,S]) #= 1
        ),

        % symmetry breaking: first bin must be loaded
        BinLoads[1] #> 0,


        % symmetry breaking: 
        %    if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0

        ( for(B,1,NumBins-1), param(BinLoads) do
              (BinLoads[B+1] #> 0) => (BinLoads[B] #> 0),
              BinLoads[B] #>= BinLoads[B+1]
        ),


        % calculate num_loaded_bins (which we will minimize)
        ( for(B,1,NumBins),
          foreach(Bin,BinSum),
          param(BinLoads) do
              Bin = (BinLoads[B] #> 0)
        ),
        NumLoadedBins #= sum(BinSum),

        % search
        term_variables([Bins,BinLoads,NumLoadedBins], Vars),
        % search(Vars,0,first_fail,indomain_min,complete,[backtrack(Backtracks)]),
        minimize(search(Vars,0,occurrence,indomain_min,complete,
                       [backtrack(Backtracks)]), NumLoadedBins),
        % bb_min(search(Vars,0,occurrence,indomain_min,complete,
        %              [backtrack(Backtracks)]), NumLoadedBins, bb_options{strategy:restart}),

        writeln(bin_loads:BinLoads),

        % just print smaller problems
        (NumBins =< 35 
        -> 
         pretty_print(Bins) 
        ; 
         writeln("Bins is too large to print.\n"), true
        ),
        writeln(num_loaded_bins:NumLoadedBins),
        writeln(backtracks:Backtracks).

%
% pretty prints the Bin matrix
%
pretty_print(X) :-
        dim(X, [N,M]),
        ( for(I, 1, N), param(X, M) do
              RowSum #= sum(X[I,1..M]),
            ( for(J, 1, M), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX)%,
                % write(" ")
            ),
             printf(" = %d", [RowSum]),
            nl
        ),nl.        


%
% Data
%        

% easy problem to test with
problem(0, 
        [](1,2,3,4,5,6,7,8,9,10), 
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
        [](360, 850, 630, 70, 700, 210), 
        1440).



% simple (and probably unrealistic) packing
problem(2, 
        [](1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
        30).


% simple (and probably unrealistic) packing
problem(3, 
        [](1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25), 
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
        [](42,69,67,57,93,90,38,36,45,42), 
        150).


% same source of data, but now there are 22 things
problem(5, 
        [](42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33), 
        250).


%
% continuing of the above example.
%
problem(6, 
        [](42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,
         30,64,29,74,41,49,55,98,80,32,25,38,82,30), 
        290).


% ibid. (takes about 20 seconds)
problem(7,
        [](42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,
           85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,
           41,49,55,98,80,32,25,38,82,30,35,39,57,84,62,50,55,27,30,36,
           20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,
           71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,
           94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,
           39),
        500).

% From 
% Graham Kendall: Bin Packing made Easier 
% http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
problem(8,
        [](442, 252, 127, 106, 37, 10, 10, 252, 252, 127, 106, 37, 10, 9,
        252, 252, 127, 85, 12, 10, 9, 252, 127, 106, 84, 12, 10, 252,
        127, 106, 46, 12, 10),
        524).


% Variant: remove 46 from the problem above
problem(9,
        [](442, 252, 127, 106, 37, 10, 10, 252, 252, 127, 106, 37, 10, 9,
        252, 252, 127, 85, 12, 10, 9, 252, 127, 106, 84, 12, 10, 252,
        127, 106, 12, 10),
        524).