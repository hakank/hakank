/*

  Advent of Code 2023 Day 3 in SWI Prolog

  https://adventofcode.com/2023/day/3


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

/*
  This is a port of my Picat approach (and I should perhaps have thought
  of another approach in Prolog).
  
  Especially part 2 is very slow:
  100,964,445 inferences, 14.635 CPU in 14.635 seconds (100% CPU, 6898771 Lips

  Speedup:
  % 23,075,672 inferences, 2.898 CPU in 2.898 seconds (100% CPU, 7962175 Lips)

 
  And I had to add a couple of cuts as well, some of them are speedup/green
  cuts, but some are indeed red cuts.
  
*/

go :- 
        File = '3.txt',

        %% Parse the file and create
        %% - the numbers + their coordinates
        % 
        read_file_to_string(File,Str,[]),
        split_string(Str,"\n", "\n",Lines),
        maplist(string_to_list,Lines,M),
        length(M,Rows),
        nth1(1,M,FirstCol),
        length(FirstCol,Cols),

        % The symbols
        findall([I,J], (between(1,Rows,I),
                        between(1,Cols,J),
                        matrix_element(M,I,J,C),
                        C \= 0'.,  % '
                        (C < 48 ; C > 57)
                       ),
                Symbols),
        
        % The number coordinates
        findall([I,J], (between(1,Rows,I),
                        between(1,Cols,J),
                        matrix_element(M,I,J,C),
                        C >= 48, C =< 57
                        ),
                NumberCoordinates),

        % Get all numbers and their indices: Number=Indices
        get_numbers(NumberCoordinates,M,[],[],Ns),

        %% Part 1
        findall(TheNumber, (member([TheNumber,Nums],Ns),
                            neighbours1(Rows,Cols,Nums,Symbols)
                           ),
                Part1),
        sum_list(Part1,Sum1),
        writeln(Sum1),

        %% Part 2
        findall(Prod,(member([I,J],Symbols),
                      neighbours2(Rows,Cols,I,J,Ns,Prod)
                     ),
                Part2),
        sum_list(Part2,Sum2),
        writeln(Sum2),
        
        nl.

%%
%% Find the neighbours of [I,J] in a Rows x Cols matrix
%%
neibs(Rows,Cols,I,J,Neibs) :-
        numlist(-1,1,ABs),
        findall([IA,JB],(member(A,ABs),
                         member(B,ABs),
                         IA is I+A, JB is J+B,
                         IA >= 1, IA =< Rows, JB >= 1, JB =< Cols
               ),
        Neibs).

%%
%% Find the neighbours for part 1
%%
neighbours1(Rows,Cols,Nums,Symbols) :-
        findall(1,(member([I,J],Nums),
                   neibs(Rows,Cols,I,J,Neibs),
                   member([P,Q],Neibs),
                   memberchk([P,Q],Symbols), ! % CUT (speedup)
                  ),
                Ns),
        length(Ns,Len),
        Len > 0.


%%
%% Find the neighbours for part 2
%%
neighbours2(Rows,Cols,I,J,Ns,Prod) :-
        neibs(Rows,Cols,I,J,Neibs),
        findall(TheNumber,(member([TheNumber,Nums],Ns),
                           has_neighbour(Nums,Neibs)
                          ),
                Ps),
        [A,B] = Ps,
        Prod is A*B.

%%
%% For neighbours2/6: Is any of the indices in nums a neighbour?
%%
has_neighbour(Nums,Neibs) :-
        findall(1,(member([P,Q],Nums), 
                   memberchk([P,Q],Neibs), ! % CUT (for not getting duplicates)
                  ),
                Ns),
        length(Ns,Len),
        Len > 0.

%% N is the number created by the coordinates in Nums
make_number(M,Nums,N) :-
        length(Nums,NumsLen),
        NumsLen > 0,
        Nums = [[_StartI,StartJ]|_],
        findall(V,(member([I,J],Nums),
                   matrix_element(M,I,J,C),
                   Val is C-0'0, % '
                   V is Val*10**(NumsLen-(J-StartJ+1))
                  ),
                Ns),
        sumlist(Ns,N).

%%
%% Parsing the matrix to get the number and its indices
%% 

% Empty coordinates and no Nums: We're done.
get_numbers([],_M,[],Ns,Ns).

% Empty coordinates, but with the trailing Num
get_numbers([],M,Nums,Ns0,[[Num,Nums]|Ns0]) :-
        make_number(M,Nums,Num).

% Nums is empty, start new Nums
get_numbers([[I,J]|T],M,[],Ns0,Ns) :-
        get_numbers(T,M,[[I,J]],Ns0,Ns), !. % CUT

% The next [I,J] belongs to Nums
get_numbers([[I,J]|T],M,Nums,Ns0,Ns) :-
        last(Nums,[P,Q]),
        Q1 is Q+1,
        I == P,
        J == Q1, !, % CUT
        once(append(Nums,[[I,J]],Nums2)),
        get_numbers(T,M,Nums2,Ns0,Ns).

%% [I,J] does not belong to the existing number (Nums)
%% Create a new number
get_numbers([[I,J]|T],M,Nums,Ns0,[[Num,Nums]|Ns]) :-
        last(Nums,[P,Q]),
        Q1 is Q+1,
        (I \= P ; J \= Q1), !, % CUT
        make_number(M,Nums,Num), 
        get_numbers(T,M,[[I,J]],Ns0,Ns).

%%
%% Val = M[I,J]
%%
matrix_element(M,I,J,Val) :-
        nth1(I,M,Row),
        nth1(J,Row,Val).