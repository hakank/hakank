/*

  Langford's number problem in B-Prolog.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  Note: For k=4 there are two different solutions:
     solution:[4,1,3,1,2,4,3,2]
     position:[2,5,3,1,4,8,7,6]
  and
     solution:[2,3,4,2,1,3,1,4]
     position:[5,1,2,3,7,4,6,8]

  Which this symmetry breaking

     Solution[1] #< Solution[K2],

  then just the second solution is shown.
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


%
% Find all solutions for K=2..8.
%
go :-
        Selection = ff,
        Choice = down,
        foreach(K in 2..8,
                [L,Len],
                (
                    writeln(k:K),
                    time(findall([K,Backtracks,Solution,Position], langford(K,Solution,Position,Selection,Choice,Backtracks), L)),        
                    length(L,Len),
                    foreach(LL in L, 
                            [_K, Backtracks,Solution,Position],
                            (
                                [K,Backtracks,Solution,Position] = LL,
                                writeln(solution:Solution),
                                writeln(position:Position),
                                writeln(backtracks:Backtracks),
                                nl
                            )
                           ),
                    writeln(len:Len),
                    nl
                )
               ).


%
% For a specific K, check all possible variants of Selection and
% Choice methods.
% This was originally to check which heuristics that was the best.
% However, all heuristics give 0 backtracks and about 0.43 seconds.
%
go2 :-
        K = 8,
        selection(Selections),
        choice(Choices),
        writeln(k:K),
        foreach(Selection in Selections, Choice in Choices,
                [Backtracks,Solution,Position,L,Len],
                (
                    writeln([selection:Selection, choice:Choice]),
                    time(findall(Backtracks, 
                                 langford(K,Solution,Position,Selection,Choice,Backtracks),
                                 L)),
                    length(L,Len),
                    format('All backtracks: ~q\n', [L]),
                    writeln(len:Len),
                    nl,
                    flush_output
                )
              ).

%
% Just get the first (if any) solutions for K in 2..12
%
go3 :-
        foreach(K in 2..12,
                [Solution,Position,Backtracks],
                (
                    writeln(k:K),
                    time(langford(K,Solution,Position,ff,down,Backtracks)) -> 
                        writeln(solution:Solution),
                        writeln(position:Position),
                        writeln(backtracks:Backtracks),
                        nl
               ;
                        writeln('No solution'),
                        nl,
                        true

                )
               ).
        

        
langford(K, Solution, Position) :-
        langford(K, Solution, ff, down, Position,_Backtracks).


langford(K, Solution, Position,Selection,Choice,Backtracks) :-
        statistics(backtracks,Backtracks1),

        K2 is 2*K,
        length(Position, K2),
        Position :: 1..K2,

        length(Solution,K2),
        Solution :: 1..K,

        alldifferent(Position),

        % symmetry breaking:
        Solution[1] #< Solution[K2],
        % Solution[1] #> Solution[K2],

        foreach(I in 1..K,
                [IK,PositionI,PositionIK,SolutionPositionI,SolutionPositionIK],
                (
                    IK is I+K,
                    nth1(IK, Position, PositionIK),
                    nth1(I, Position, PositionI),
                    % I1 is I+1,
                    PositionIK #= PositionI + I+1,
                    nth1(PositionI,Solution,SolutionPositionI),
                    SolutionPositionI #= I,
                    nth1(PositionIK,Solution,SolutionPositionIK),
                    SolutionPositionIK #= I
                )
        ),


        term_variables([Position,Solution], Vars),
        labeling([Selection,Choice], Vars),

        statistics(backtracks,Backtracks2),
        Backtracks is Backtracks2 - Backtracks1.



% Variable selection
selection([backward,constr,degree,ff,ffc,forward,inout,leftmost,max,min]).

% Value selection
choice([down,updown,split,reverse_split]).
