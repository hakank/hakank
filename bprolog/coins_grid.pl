/*

  Coins puzzle in B-Prolog.

  Problem from 
  Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one 
  should place coins in such a way that the following conditions are 
  fulfilled:
    1. In each row exactly 14 coins must be placed.
    2. In each column exactly 14 coins must be placed.
    3. The sum of the quadratic horizontal distance from the main
       diagonal of all cells containing a coin must be as small as possible.
    4. In each cell at most one coin can be placed.

   The description says to place 14x31 = 434 coins on the chessboard 
   each row containing 14 coins and each column also containing 14 coins.
  """

  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).



% CLP(FD): 16s
go :-
        N = 10,
        C = 4,
        time(coins(N, C)).

%
% IP solve: Fast as expected: 0.044s
%
go2 :-
        N = 31,
        C = 14,
        time(coins_ip(N,C)).


% sat_solve: 22.72s
go3 :-
        N = 10,
        C = 4,
        time(coins_sat(N,C)).

% cp_solve: 17.8s
go4 :-
        N = 10,
        C = 4,
        time(coins_cp(N,C)).

%
% CLP(FD) testing all labelings (with a timeout)
%
% It took 13:56 minutes to run this with a 30s timeout.
% Best was [ff,reverse_split] in 15.91s.
% 
go5 :-
        N = 10,
        C = 4,
        Timeout is 30 * 1000, % in millis
        selection(VariableSelect),
        choice(ValueSelect),
        foreach(Var in VariableSelect, Val in ValueSelect,
                [Result],
                (
                    writeln([Var,Val]),
                    time_out(time2(coins_label_test(N,C,Var,Val)),
                             Timeout,Result),
                    writeln('Timeout result':Result),
                    nl
                )
               ).



pretty_print(X) :-
        N @= X^length,
        foreach(I in 1..N,
                (foreach(J in 1..N,[XX],
                         (
                             XX @= X[I,J],
                             format('~d ', [XX])
                         )
                        ),
                 nl
                )
               ).


% standard CLP(FD)
coins(N,C) :-
        % N = 10, % 31 the grid size
        % C = 6,  % 14, number of coins per row/column
        
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 0..1,

        % Sum :: 0..99999,
        Sum #>= 0,
        foreach(I in 1..N, 
                (
                    C #= sum([T : J in 1..N, [T], T @= X[I,J]]), % rows
                    C #= sum([T : J in 1..N, [T], T @= X[J,I]]) % columns
                    )
               ),

        % quadratic horizontal distance
        Sum #= sum([
                       T : I in 1..N, J in 1..N, 
                       [T],
                       T @= (X[I,J] * abs(I-J)*abs(I-J))
                   ]),

        minof(labeling([ff,reverse_split],Vars),Sum),
        writeln(sum:Sum),
        pretty_print(X).



% IP solver
coins_ip(N, C) :-
        % N = 10, % 31 the grid size
        % C = 6,  % 14, number of coins per row/column
        
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 0..1,

        % Sum :: 0..99999,
        Sum $>= 0,
        foreach(I in 1..N, 
                (
                    C $= sum([T : J in 1..N, [T], T @= X[I,J]]), % rows
                    C $= sum([T : J in 1..N, [T], T @= X[J,I]]) % columns
                    )
               ),

        % quadratic horizontal distance
        Sum $= sum([
                       T : I in 1..N, J in 1..N, 
                       [T],
                       T @= (X[I,J] * abs(I-J)*abs(I-J))
                   ]),


        % minof(labeling([ff,down],Vars),Sum),
        ip_solve([min(Sum)], Vars),
        writeln(sum:Sum),
        pretty_print(X).
                
% SAT solver
coins_sat(N, C) :-
        % N = 10, % 31 the grid size
        % C = 6,  % 14, number of coins per row/column
        
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 0..1,

        % Sum :: 0..99999,
        Sum $>= 0,
        foreach(I in 1..N, 
                (
                    C $= sum([T : J in 1..N, [T], T @= X[I,J]]), % rows
                    C $= sum([T : J in 1..N, [T], T @= X[J,I]]) % columns
                    )
               ),

        % quadratic horizontal distance
        Sum $= sum([
                       T : I in 1..N, J in 1..N, 
                       [T],
                       T @= (X[I,J] * abs(I-J)*abs(I-J))
                   ]),


        % minof(labeling([ff,down],Vars),Sum),
        sat_solve([min(Sum)], Vars),
        writeln(sum:Sum),
        pretty_print(X).


% CP solve (!= CLP(FD) it seems)
coins_cp(N, C) :-
        % N = 10, % 31 the grid size
        % C = 6,  % 14, number of coins per row/column
        
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 0..1,

        % Sum :: 0..99999,
        Sum $>= 0,
        foreach(I in 1..N, 
                (
                    C $= sum([T : J in 1..N, [T], T @= X[I,J]]), % rows
                    C $= sum([T : J in 1..N, [T], T @= X[J,I]]) % columns
                    )
               ),

        % quadratic horizontal distance
        Sum $= sum([
                       T : I in 1..N, J in 1..N, 
                       [T],
                       T @= (X[I,J] * abs(I-J)*abs(I-J))
                   ]),


        cp_solve([min(Sum),ff,reverse_split], Vars),
        writeln(sum:Sum),
        pretty_print(X).


% standard CLP(FD)
coins_label_test(N,C,VariableSel,ValueSel) :-
        % N = 10, % 31 the grid size
        % C = 6,  % 14, number of coins per row/column
        
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 0..1,

        % Sum :: 0..99999,
        Sum #>= 0,
        foreach(I in 1..N, 
                (
                    C #= sum([T : J in 1..N, [T], T @= X[I,J]]), % rows
                    C #= sum([T : J in 1..N, [T], T @= X[J,I]]) % columns
                    )
               ),

        % quadratic horizontal distance
        Sum #= sum([
                       T : I in 1..N, J in 1..N, 
                       [T],
                       T @= (X[I,J] * abs(I-J)*abs(I-J))
                   ]),

        (
        minof(labeling([VariableSel,ValueSel],Vars),Sum) -> 
            writeln(sum:Sum),
            pretty_print(X)
        ;
            writeln('Failed for some reason'),
            true
        ).



% Variable selection
selection([backward,constr,degree,ff,ffc,forward,inout,leftmost,max,min]).

% Value selection
choice([down,updown,split,reverse_split]).