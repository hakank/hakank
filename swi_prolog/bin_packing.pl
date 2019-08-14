/*

  Bin packing in SWI Prolog

  Simple bin packing problems.
  This model is based on my Picat version,via
  the B-Prolog, ECLiPSe and SICStus Prolog models.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%%
%% sorted Stuff
%%
go :-
        between(0,9,P),
        format("Problem ~w~n",[P]),
        problem(P, StuffUnordered, BinCapacity),
        time(once(bin_packing(StuffUnordered, BinCapacity,sorted, NumBins, Bins, BinLoads, NumLoadedBins))),
        print_solution(NumBins, Bins,BinLoads,NumLoadedBins),
        fail,
        nl.

go.

%%
%% Unsorted Stuff. 
%%
go2 :-
        between(0,9,P),
        format("Problem ~w~n",[P]),
        problem(P, StuffUnordered, BinCapacity),
        time(once(bin_packing(StuffUnordered, BinCapacity,unsorted, NumBins, Bins, BinLoads, NumLoadedBins))),
        print_solution(NumBins, Bins,BinLoads,NumLoadedBins),
        fail,
        nl.

go2.


bin_packing(StuffUnordered, BinCapacity, SortMode, NumBins, Bins, BinLoads, NumLoadedBins) :-

        %% sorting the stuff might make it a bit faster
        (
         SortMode == sorted
        ->
         %% sort (reverse)
         %% sort(0,@>=,StuffUnordered,Stuff)
         %% sort 
         sort(0,@=<,StuffUnordered,Stuff)        
        ;
         Stuff = StuffUnordered
        ),

        %% BinCapacity: the (common) capacity for each bin
        writeln(bin_capacity=BinCapacity),

        %% Stuff: values/weight of the things to pack
        length(Stuff,NumStuff), 
        writeln(stuff=Stuff),
        writeln(num_stuff=NumStuff),

        %% Number of bins cannot exceed num_stuff...
        NumBins #= NumStuff,

        %% Bins: where to put a thing.
        new_matrix(NumBins,NumStuff,0..1,Bins),

        %% BinLoads: contains how many things (the summed 
        %% weights of) a bin takes
        length(BinLoads,NumBins),
        BinLoads ins 0..BinCapacity,

        %% BinLoads is the number of loaded items in Bins
        maplist(scalar_products(Stuff),Bins,BinLoads),

        %% A thing is packed exactly once
        transpose(Bins,BinsTransposed),
        maplist(packed_once,BinsTransposed),

        %% Compare the total loads
        sum(Stuff,#=,SumStuff),
        sum(BinLoads,#=,SumStuff),

        %% calculate the number of loaded bins (which we will minimize)
        num_loaded_bins(BinLoads, 0, NumLoadedBins),

        %%
        %% symmetry breaking
        %%
        
        %% load the bins in order:
        %% first bin must be loaded
        element(1,BinLoads,BinLoads1),
        BinLoads1 #> 0,

        %% decreasing(BinLoads),
        
        %% if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0
        %% This seems to be better than using decreasing/1.
        %%
        load_bins_in_order(BinLoads,NumBins),

        %% search
        flatten([BinLoads,Bins],Vars),
        %% labeling([min,min(NumLoadedBins)],Vars).
        labeling([min,min(NumLoadedBins)],Vars).



scalar_products(Stuff,Bin,BinLoad) :-
        scalar_product(Stuff,Bin,#=,BinLoad).

%%
%% a thing is packed exactly once 
%%
packed_once(BinColumn) :-
        sum(BinColumn, #=, 1).

%%
%% calculate the number of loaded bins (which we will minimize)
%%
num_loaded_bins([],NumLoaded,NumLoaded).
num_loaded_bins([Bin|Bins],NumLoaded0,NumLoaded) :-
        B in 0..1,
        Bin #> 0 #<==> B #= 1,
        NumLoaded1 #= NumLoaded0 + B,
        num_loaded_bins(Bins,NumLoaded1,NumLoaded).

%% symmetry breaking: 
%%    if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0
load_bins_in_order(BinLoads,NumBins) :-
        NumBins1 #= NumBins-1,
        numlist(1,NumBins1,Bs),
        maplist(load_bins_in_order_(BinLoads),Bs).

load_bins_in_order_(BinLoads,B) :-
        element(B,BinLoads, BinLoadsB),
        B1 #= B+1,
        element(B1,BinLoads, BinLoadsB1),
        BinLoadsB1 #>0 #==> BinLoadsB #> 0,
        BinLoadsB #>= BinLoadsB1.        

%%
%% Pretty print a solution
%%
print_solution(NumBins, Bins,BinLoads,NumLoadedBins) :-
        writeln('...'),
        writeln(bin_loads=BinLoads),
        writeln(num_loaded_bins=NumLoadedBins),
        writeln(numBins=NumBins),
        %% just print smaller problems
        (
         NumBins =< 50
        ->
         writeln("Bins:"),
         maplist(pretty_print_row,BinLoads, Bins) 
         ;
         writeln("Bins are too large to print.\n")
        ),
        nl.


%%
%% Just print the used bins.
%%
pretty_print_row(BinLoad,Bin) :-
        (
         BinLoad #> 0
        ->
         sum(Bin, #=, BinSum),
         format("~w = ~w~n",[Bin,BinSum])
        ;
         true
        ).

%%
%% Data
%%   

%%
%% Easy problem to test with 
%% (though problem 1 is easier)
%%
problem(0, P, Capacity) :-
        P=[1,2,3,4,5,6,7,8,9,10],
        Capacity=30.


%%
%% Example from the Alice system
%% Copying files to disks
%% http://news.mozart-oz.org/alice/manual/cptutorial/node55.html
%%
%% """
%% Suppose, you want to copy a set of files from your hard-disk onto as 
%% few as possible diskettes of a given size, e. g. onto common 1.44 MB 
%% diskettes. In case your files do not fit on a single diskette, it might 
%% become quite tricky to figure out the minimal number of needed diskettes 
%% and how to partition the files.
%% """
%%
problem(1, P, Capacity) :- 
        P=[360, 850, 630, 70, 700, 210], 
        Capacity=1440.

%%
%% simple (and probably unrealistic) packing
%%
problem(2, P, Capacity) :- 
        P=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
        Capacity=20.


%%
%% simple (and probably even less unrealistic) packing
%%
problem(3, P, Capacity) :- 
        P = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],
        Capacity = 50.

%%
%% This problem is from
%% http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
%% 1D Bin Packing (or "CP? Who cares?"), page 3
%% and also from
%% http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
%%
problem(4, P, Capacity) :- 
        P= [42,69,67,57,93,90,38,36,45,42], 
        Capacity = 150.


%%
%% Same source of data as problem 4, but now there are 22 things.
%%
problem(5, P, Capacity) :- 
        P=[42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
           46,38,85,33],
        Capacity = 250.


%%
%% continuing of the above example.
%%
problem(6, P, Capacity) :- 
        P = [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
             46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,
             30,64,29,74,41,49,55,98,80,32,25,38,82,30], 
        Capacity = 290.

%%
%% ibid.
%%
problem(7, P, Capacity) :- 
        P= [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,
            85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,
            41,49,55,98,80,32,25,38,82,30,35,39,57,84,62,50,55,27,30,36,
            20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,
            71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,
            94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,
            39],
        Capacity = 500.


%%
%% From 
%% Graham Kendall: Bin Packing made Easier 
%% http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
%%
problem(8, P, Capacity) :- 
        P = [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
             252,252,127,85,12,10,9,252,127,106,84,12,10,252,
             127,106,46,12,10],
        Capacity = 524.

%%
%% Variant: remove 46 from the problem 8 above.
%% And it's much harder.
%%
problem(9, P, Capacity) :- 
        P =  
        [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
         252,252,127,85,12,10,9,252,127,106,84,12,10,252,
         127,106,12,10],
        Capacity = 524.
