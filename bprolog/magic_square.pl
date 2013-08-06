/*

  Magic squares in B-Prolog.



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

%
% Simple run
%
go :-
        time2(magic(5,_Square)).

%
% Run for different sizes
% 
% ff/updown solves N=6 in 0.06s and 6856 backtracks
%                   10 in 2.99s and 289939 backtracks
% ffd/updown solves N=10 in 2.37s and 306336 backtracks
go2 :-
        foreach(N in 3..11, [Square], time2(magic(N,Square))).


%
% All solutions.
%
go3 :-
        N = 4,
        findall(Square,magic(N,Square),L),
        length(L,Len),
        writeln('Len':Len).




magic(N,Square) :-
        format('\n\nN: ~d\n', [N]),
        NN is N*N,
        Sum is N*(NN+1)//2,% magical sum
        format('Sum = ~d\n', [Sum]),

        new_array(Square,[N,N]),
        array_to_list(Square, Vars),
        Vars :: 1..NN,

        alldifferent(Vars),
        foreach(I in 1..N,
                (
                    Sum #= sum([ T : J in 1..N, [T], T @= Square[I,J]]),% rows
                    Sum #= sum([ T : J in 1..N, [T], T @= Square[J,I]]) % column
                )
        ),

        Sum #= sum(Square^diagonal1),% diagonal sums
        Sum #= sum(Square^diagonal2),% diagonal sums

        % Symmetry breaking
        Square[1,1] #< Square[1,N],
        Square[1,1] #< Square[N,N],
        Square[1,1] #< Square[N,1],
        Square[1,N] #< Square[N,1],

        labeling([ffd,updown],Vars),
        print_square(Square).



print_square(Square) :-
        N is Square^length,
        foreach(I in 1..N,
                (
                foreach(J in 1..N,
                        [S],
                        (
                            S @= Square[I,J],
                            format('~3d', [S])
                        )
                       ),
                nl
                )
               ), 
        nl.
