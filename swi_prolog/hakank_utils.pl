/*

  Utils in SWI Prolog

  These are mostly for clpfd, e.g. global constraints that's missing in
  SWI-Prolog, but also some wrapper/convenience utils.
  
  Module created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/
:- module(hakank_utils,
          [
           time2/2,
           new_matrix/3,
           new_matrix/4,
           domain_matrix/2,
           print_matrix/1,      % perhaps not?
           matrix_dimensions/3,
           latin_square/1,        
           list_domains/2,
           matrix_element/4,
           %% matrix_element1/4,
           matrix_element2/4,
           %% matrix_element3/4,
           %% matrix_element4/4,
           matrix_element5/4,
           matrix_nth1/4,           
           element2/3,
           alldifferent_except_0/1,
           alldifferent_except_n/2,
           increasing/1,
           increasing_strict/1,
           decreasing/1,
           decreasing_strict/1,
           count_occurrences/3,
           extract_from_indices/3,
           extract_from_indices2d/3,
           extract_from_indices2d/4,           
           scalar_product2/3,
           min_list_clp/2,
           max_list_clp/2,
           slice/4,
           to_num/2,
           to_num/3,
           zip2/3,
           zip3/4,
           inverse/1,
           inverse/2,
           between/4,
           between_down/3,           
           numlist_step/4,           
           same/1,
           lex_lte/2,
           lex_lt/2,
           numlist_cross2/3,
           numlist_cross3/4,           
           diagonal1_slice/2,
           diagonal2_slice/2,
           prod/2,
           regular/6,
           regular2/7,           
           rotate/2,
           variable_selection/1,
           value_selection/1,
           strategy_selection/1,
           labelings/3,
           atleast/3,
           atmost/3,
           list_domain_disjunction/2,
           make_disj_domain/2,
           create_task/5,
           my_cumulative/4,
           distribute/3,
           sliding_sum/4,
           sliding_sum/3,
           circuit_path/2,
           nvalue/2,
           nvalues/3,           
           exactly/3,
           alldifferent_modulo/2,
           all_differ_from_at_least_k_pos/2,
           alldifferent_cst/2,
           all_equal/1,
           all_min_dist/2,
           alldiffer_on_intersection/2,
           among/3,
           among_seq/5,
           among_range/4,
           
           print_attrs_list/1
          ]).

:- use_module(library(clpfd)).

%%
%% time2(Goal, Time)
%%
%% Returns the time of a goal.
%%
time2(Goal, Time) :-
        statistics(cputime, Time1),
        call(Goal),
        statistics(cputime,Time2),
        Time is Time2 - Time1.

%%
%% new_matrix(NumRows, NumCols, Matrix)
%%
%% Create a new matrix, but without any domain
%%
new_matrix(NumRows, NumCols, Matrix) :-
        length(Cols,NumCols),
        length(Matrix,NumRows),
        maplist(same_length(Cols),Matrix).

%%
%% new_matrix(NumRows, NumCols, Domain, Matrix)
%%
%% Create a matrix of dimension NumRows x NumCols and with domain Domain.
%%
new_matrix(NumRows, NumCols, Domain, Matrix) :-
        length(Cols,NumCols),
        length(Matrix,NumRows),
        maplist(same_length(Cols),Matrix),
        domain_matrix(Matrix,Domain).


%%
%% domain_matrix([],_)
%%
%% Ensure the domain of Domain
%%
domain_matrix([],_).
domain_matrix([L1|LRest],Domain) :-
        L1 ins Domain,
        domain_matrix(LRest, Domain).

%%
%% print_matrix(Matrix)
%%
%% Nice print of a matrix.
%%
print_matrix(Matrix) :-
        maplist(writeln,Matrix),
        nl.

%%
%% matrix_dimensions(Matrix, Rows, Cols)
%%
%% Dimensions of a matrix.
%%
matrix_dimensions(Matrix, Rows, Cols) :-
        length(Matrix,Rows),
        transpose(Matrix, Transposed),
        length(Transposed, Cols).

%%
%% latin_square(X)
%%
%% Ensure that X (NxN matrix) is a Latin Square
%%
latin_square(X) :-
        maplist(all_different,X),
        transpose(X,XT),
        maplist(all_different,XT).


%%
%% list_domains(L,Domains)
%%
%% Create a list of the domains in the
%% list of decision variables in list L.
%% (not reversible).
%%
list_domains(L,Domains) :-
        list_domains(L,[],Domains).
list_domains([], D,D).
list_domains([H|T],D0,[Dom|D]) :-
        fd_size(H,Size),
        fd_dom(H,Dom1),
        (Size > 1 -> 
         Dom = Dom1
        ;
          Dom..Dom = Dom1
        ),
        list_domains(T,D0,D).


%%
%% matrix_element(X, I, J, Val)
%%
%% Matrix[I,J] = Val
%%
matrix_element(X, I, J, Val) :-
        matrix_element2(X,I,J,Val).

%%
%% Different approaches.
%% matrix_element2/4 and matrix_element5/4 seems to work best.
%%
% matrix_element1(X, I, J, Val) :-
%         element(I, X, Row),
%         element(J, Row, Val).

matrix_element2(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).

% matrix_element3(X, I, J, Val) :-
%         freeze(I, (nth1(I, X, Row),freeze(J,nth1(J,Row,Val)))).

% matrix_element4(X, I, J, Val) :-
%         freeze(I, (element(I, X, Row),freeze(J,element(J,Row,Val)))).

matrix_element5(X, I, J, Val) :-
        nth1(I, X, Row),
        nth1(J, Row, Val).

%%
%% matrix_nth1(X, I, J, Val)
%%
%% A better name for matrix_element5/3.
%%
matrix_nth1(X, I, J, Val) :-
        nth1(I, X, Row),
        nth1(J, Row, Val).

%%
%% element2(I,X,Y)
%%
%% Y[X[I]] #= I (symmetry between two lists),
%% cf. inverse/2.
%% 
element2(I,X,Y) :-
   element(I,X,XI),
   element(XI,Y,XIY),
   XIY #= I.


%%
%% alldifferent_except_0(X)
%%
%% Ensure that all values in Xs which are != 0 are different.
%%                                %
%%
alldifferent_except_0(X) :-
        alldifferent_except_n(X,0).

%%
%% alldifferent_except_n(X,N)
%%
%% alldifferent except N.
%%
alldifferent_except_n(X,N) :-
        length(X,Len),
        findall([I,J], (between(2,Len,I), I1 #= I-1, between(1,I1,J)),L),
        alldifferent_except_n_(L,X,N).

alldifferent_except_n_([], _X,_N).
alldifferent_except_n_([[I,J]|L],X,N) :-
        element(I,X,XI),
        element(J,X,XJ),        
        (XI #\= N #/\ XJ #\= N) #==> (XI #\= XJ),
        alldifferent_except_n_(L,X,N).


%%
%% increasing(X)
%% increasing_strict(X)
%% decreasing(X)
%% decreasing_strict(X)
%%
%% X must be a (strictly/not strictly) increasing/descreasing list
%%
increasing(X) :-
        chain(X, #=<).

increasing_strict(X) :-
        chain(X, #<).

decreasing(X) :-
        chain(X, #>=).

decreasing_strict(X) :-
        chain(X, #>).

%%
%% count_occurrences(L,Element,Count)
%%
%% In the list L, there must be exactly Count occurrences of Element
%%
count_occurrences(L,Element,Count) :-
        count_occurrences_(L,Element,0, Count).

count_occurrences_([],_Element,Count, Count).
count_occurrences_([H|T],Element,Count0, Count) :-
        B in 0..1,
        H #= Element #<==> B #= 1,
        Count1 #= Count0 + B,
        count_occurrences_(T,Element,Count1, Count).


%%
%% extract_from_indices(Is,X,Xs)
%%
%% Extract from indices in a list.
%% extract_from_indices([I1,I2,I3,...], X, ExtractedFromX)
%%
%% It seems to be reversible.
%%
extract_from_indices(Is,X,Xs) :-
        extract_from_indices(Is, X, [], Xs).
extract_from_indices([], _X, Xs, Xs).
extract_from_indices([I|Is], X, Xs0, [XI|Xs]) :-
        element(I,X,XI),
        extract_from_indices(Is, X, Xs0, Xs).


%%
%% extract_from_indices2d(Is,X,Xs)
%%
%% Extract from indices in a matrix (2d list).
%% extract_from_indices2d([[I1,J1],[I2,J2],...], X,[], ExtractedFromXs)
%%
extract_from_indices2d(Is,X,Xs) :-
        extract_from_indices2d(Is, X, [], Xs).
extract_from_indices2d([], _X, Xs, Xs).
extract_from_indices2d([[I,J]|IJs], X, Xs0, [XIJ|Xs]) :-
        matrix_element(X,I,J,XIJ),
        extract_from_indices2d(IJs, X, Xs0, Xs).

%%
%% scalar_product2(Xs,Ys,Sum)
%%
%% The built-in scalar_product/4 don't accept two lists of decision variables
%%
scalar_product2(Xs,Ys,Sum) :-
        scalar_product2_(Xs,Ys,0,Sum).
scalar_product2_([],[],Sum,Sum).
scalar_product2_([X|Xs],[Y|Ys],Sum0,Sum) :-
        Sum1 #= Sum0 + X*Y,
        scalar_product2_(Xs,Ys,Sum1,Sum).


%%
%% min_list_clp(List,MinElement)
%%
%% SWI-Prolog's min_list/2 don't handle decision variables.
%%
min_list_clp([H|T], Min) :-
    min_list_clp(T, H, Min).

min_list_clp([], Min, Min).
min_list_clp([H|T], Min0, Min) :-
    Min1 #= min(H, Min0),
    min_list_clp(T, Min1, Min).

%%
%% max_list_clp(List,MinElement)
%%
%% SWI-Prolog's max_list/2 don't handle decision variables.
%%
max_list_clp([H|T], Min) :-
    max_list_clp(T, H, Min).

max_list_clp([], Min, Min).
max_list_clp([H|T], Min0, Min) :-
    Min1 #= max(H, Min0),
    max_list_clp(T, Min1, Min).


%%
%% From https://github.com/sunilnandihalli/99-problems-in-prolog/blob/master/slice.pl
%%
%% slice(List,From,To,Slice)
%%
%% Note: It's 0 based!
%% Note2: It might be easier to use extract_from_indices/3 instead.
%%
slice([X|_],0,0,[X]).
slice([X|Xs],0,K,[X|Ys]):- K > 0, K1 is K - 1, slice(Xs,0,K1,Ys).
slice([_|Xs],I,K,Ys):- I > 0, I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).



%%
%% to_num(List, Base, Num)
%%
%% sum(List) #= Num  (using base Base)
%%
to_num(List,Num) :-
        to_num(List,10,Num).
to_num(List, Base, Num) :-
        length(List,Len),
        to_num_(List,1,Len,Base,0, Num).

% Num #= sum([List[I]*Base**(Len-I) : I in 1..Len]).
to_num_([],_I,_Len,_Base,Num,Num).
to_num_([H|T],I,Len,Base,Num0,Num) :-
        Len1 #= Len-I,
        Num1 #= Num0 + H*(Base^Len1),
        I1 #= I+1,
        to_num_(T,I1,Len,Base,Num1,Num).

%%
%% zip2(List1,List2, ZippedList)
%%
%% ZippedList is the zipped list of List1 and List2.
%% Note: If List1 and List2 is of different length, zip2/3 fails.
%%
%% Examples:
%% ?- zip2([a,b,c],[1,2,3],[[a,1],[b,2],[c,3]])
%% true.
%% ?- make,zip2(L1,[1,2,3],[[a,1],[b,2],[c,3]]).
%% L1 = [a, b, c].
%%
%% ?- make,zip2([a,b,c],L2,[[a,1],[b,2],[c,3]]).
%% L2 = [1, 2, 3].
%% 
%% ?- make,zip2(L1,L2,[[a,1],[b,2],[c,3]]).
%% L1 = [a, b, c],
%% L2 = [1, 2, 3] ;
%% false.
%%
%% ?- make,zip2(L1,[1,2,3],[[a,1],[b,2],[c,X]]).
%% L1 = [a, b, c],
%% X = 3.
%%
zip2(L1,L2,L) :-
        zip2(L1,L2,[],L).
zip2([],[], L,L).
zip2([H1|T1],[H2|T2], L0,[L1|L]) :-
        L1 = [H1,H2],
        zip2(T1,T2, L0,L).

%%
%% zip3(L1,L2,L3,L)
%%
zip3(L1,L2,L3,L) :-
        zip3(L1,L2,L3,[],L).
zip3([],[],[],L,L).
zip3([H1|T1],[H2|T2], [H3|T3], L0,[L1|L]) :-
        L1 = [H1,H2,H3],
        zip3(T1,T2, T3, L0,L).



%%
%% inverse(L)
%%
%% inverse/1, a.k.a. "self-assignment"
%%
inverse(L) :-
        inverse(L,L).

%%
%% inverse(L1,L2)
%% a.k.a. assignment
%%
%% For each element in L1 and L2
%%     - L1[I] = I
%%     or
%%     - X[I] = J <=> X[J] = I
%% 
inverse(L1,L2) :-
        %% same length
        length(L1,Len),
        length(L2,Len),
        findall([I,J],(between(1,Len,I),between(1,Len,J)),IJs),
        inverse_(IJs,L1,L2).


inverse_([],_L1,_L2).
inverse_([[I,J]|IJs],L1,L2) :-
        element(I,L1,L1I),
        element(J,L2,L2J),
        (J #= L1I) #<==> (I #= L2J),
        inverse_(IJs,L1,L2).

%%
%% between(From,Step,To,N)
%%
%% As between/3 but with a step parameter.
%%
between(From,Step,To,N) :-
        Div is To div Step,
        between(From,Div,Tmp),
        TmpFrom is Tmp-From,
        N is TmpFrom*Step+From.

%%
%% between_down(From, To, N)
%%
%% Count down from From to To (From >= N).
%%
between_down(N, M, K) :-
        N #>= M,
        K #= N.
between_down(N, M, K) :-
        N #> M,
        N1 #= N-1,
        between_down(N1, M, K).


%%
%% numlist_step(L,Step,U,Ls)
%%
%% Ls is a list of L..Step..U.
%%
%% As numlist/3 but with step Step.
%%
%% Examples:
%%   ?- numlist_step(1,2,10,L).
%%   [1,3,5,7,9]
%%%  ?- numlist_step(1,2,11,L)
%%   [1,3,5,7,9,11]
%%
numlist_step(L, Step, U, Ns) :-
        numlist_step_(L, Step, U, Ns).

numlist_step_(L, Step, U, [L]) :-
        L + Step > U,
        !.
numlist_step_(L, Step, U, [U]) :-
        L + Step =:= U,
        !.
numlist_step_(L, Step, U, [L|Ns]) :-
        L+Step =< U,        
        L2 is L+Step,
        numlist_step_(L2, Step, U, Ns).


%%
%% same(Xs)
%%
%% All elements in the list must be the same.
%%
same([X|Xs]) :-
        maplist(same(X),Xs).

same(X,X2) :-
        X2 #= X.

%%
%% lex_lte(X1,X2)
%%
%% X1 is lexicographic less or equal than X2
%%
lex_lte(X1,X2) :-
        X1 #=< X2.

%%
%% lex_lt(X1,X2)
%%
%% X1 is lexicographic less than X2
%%
lex_lt(X1,X2) :-
        X1 #=< X2.


%%
%% numlist_cross2(N1,N2,IJs)
%%
%% All combinations of 1..N1 x 1..N2 -> [[I1,J1],[I2,J2],....]
%%
numlist_cross2(N1,N2,IJs) :-
        findall([I,J],(between(1,N1,I),
                       between(1,N2,J)
                      ),
                IJs).

numlist_cross3(N1,N2,N3,IJKs) :-
        findall([I,J,K],(between(1,N1,I),
                       between(1,N2,J),
                       between(1,N3,K)
                      ),
                IJKs).


%%
%% diagonal1_slice(M,Slice)
%%
%% Diagonal 1 slice.
%%
diagonal1_slice(M,Slice) :-
        length(M,N),
        findall([I,I],
                between(1,N,I),
                Ixs
               ),
        extract_from_indices2d(Ixs,M,Slice).


%%
%% diagonal2_slice(M,Slice)
%%
%% Diagonal 2 slice.
%%
diagonal2_slice(M,Slice) :-
        length(M,N),
        findall([I,I2],
                (between(1,N,I),
                 I2 #= N-I+1)
               , Ixs
               ),
        extract_from_indices2d(Ixs,M,Slice).

%%
%% Helper for prod/2. (Not exported.)
%%
mult(X,Y,Z) :- Z #= X*Y.

%%
%% prod(List,Product)
%%
%% Product is the product of the number is a list.
%%
prod(L,Product) :-
        foldl(mult,L,1,Product).
           


/*

  regular(X, Q, S, D, Q0, F)
  
  This is a translation of MiniZinc's regular constraint (defined in
  lib/zinc/globals.mzn), via the Comet code refered above.
  All comments in '"""' are from the MiniZinc code.
  
  Note: This is a translation of my Picat implementation
        (http://hakank.org/picat/regular.pi)
  """
  The sequence of values in array 'x' (which must all be in the range 1..S)
  is accepted by the DFA of 'Q' states with input 1..S and transition
  function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
  (which must be in 1..Q) and accepting states 'F' (which all must be in
  1..Q).  We reserve state 0 to be an always failing state.
  """
  
  x : IntVar array
  Q : number of states
  S : input_max
  d : transition matrix
  q0: initial state
  F : accepting states
  
*/
regular(X, Q, S, D, Q0, F) :-

        %% """
        %% If x has index set m..n-1, then a[m] holds the initial state
        %% (q0), and a[i+1] holds the state we're in after  processing
        %% x[i].  If a[n] is in F, then we succeed (ie. accept the string).
        %% """
        M = 1,
        length(X,N),
        N2 #= N+1,
        length(A,N2),
        A ins 1..Q,

        X ins 1..S, %% """Do this in case it's a var."""

        element(M,A,AM),
        AM #= Q0,       %% Set a[0], initial state
        
        %% MiniZinc's infamous matrix element
        %%   a[i+1] = d[a[i], x[i]]
        numlist(1,N,Is),
        maplist(regular_loop(A,X,D),Is),
        
        %% member(A[N2], F). %% """Check the final state is in F."""
        %% A[N2] :: F.       %% """Check the final state is in F."""
        element(N2,A,AN2),
        element(_,F,AN2).


/*

  regular2(X, Q, S, D, Q0, F, A)
  
  regular2/7 is the same as regular/6 but it also has the
  output parameter A (last), the state list.
  This might be handy for debugging or explorations.

*/
regular2(X, Q, S, D, Q0, F, A) :-

        %% """
        %% If x has index set m..n-1, then a[m] holds the initial state
        %% (q0), and a[i+1] holds the state we're in after  processing
        %% x[i].  If a[n] is in F, then we succeed (ie. accept the string).
        %% """
        M = 1,
        length(X,N),
        N2 #= N+1,
        length(A,N2),
        A ins 1..Q,

        X ins 1..S, %% """Do this in case it's a var."""

        element(M,A,AM),
        AM #= Q0,       %% Set a[0], initial state
        
        %% MiniZinc's infamous matrix element
        %%   a[i+1] = d[a[i], x[i]]
        numlist(1,N,Is),
        maplist(regular_loop(A,X,D),Is),
        
        %% member(A[N2], F). %% """Check the final state is in F."""
        %% A[N2] :: F.       %% """Check the final state is in F."""
        element(N2,A,AN2),
        element(_,F,AN2).


%%
%% The matrix element loop
%%   a[i+1] = d[a[i], x[i]]
%%
regular_loop(A,X,D,I) :-
        element(I,A,AI),
        element(I,X,XI),
        I1 #= I+1,
        element(I1,A,AI1),
        matrix_element(D,AI,XI,AI1).

%%
%% rotate(L,L2)
%%
%% rotate a list (put first element -> last)
%%
rotate(L,L2) :-
        L=[X|LRest], append(LRest,[X],L2).



%% For benchmarking different labelings
%%
%% variable_selection([leftmost,ff,ffc, min,max]).
%% value_selection([up,down]).
%% strategy_selection([step,enum,bisect]).
%% labelings(VariableSelection,ValueSelection,StrategySelection)
%%
% Variable selection
variable_selection([leftmost,ff,ffc, min,max]).

% Value selection
value_selection([up,down]).

% Branching strategy
strategy_selection([step,enum,bisect]).

labelings(VariableSelection,ValueSelection,StrategySelection) :-
        variable_selection(VariableSelection),
        value_selection(ValueSelection),
        strategy_selection(StrategySelection).


%%
%% atmost(N,L,V)
%%
%% The value V in list L can have at most N occurrences.
%%
atmost(N,L,V) :-
        count_occurrences(L,V,C),
        C #=< N.

%%
%% atleast(N,L,V)
%%
%% The value V in list L must have at least N occurrences.
%%
atleast(N,L,V) :-
        count_occurrences(L,V,C),
        C #>= N.


%%
%% list_domain_disjunction([D|Ds],Conj)
%%
%% Convert a list of integers to a disjunction of domain.
%%
%% ?- write_canonical(1\/2\/3\/4\/5\/6).
%% \/(\/(\/(\/(\/(1,2),3),4),5),6)
%%
list_domain_disjunction([D|Ds],Disj) :-
        foldl(disj,Ds,D,Disj).
disj(A,B,C) :-C = \/(A,B).


%%
%%  make_disj_domain(V, Domain)
%% 
%%  Converts a list domain Domain to a disjunction domain
%%  and make the domain assignment.
%%
%% Example:
%%   make_disj_domain(V, [1,3,5,7,9,11])
%%
make_disj_domain(V, Domains) :-
        list_domain_disjunction(Domains,Disj),
        V in Disj.


%%
%% create_tasks(StartTime,Duration,Resource,Task,EndTime)
%%
%% Create a task/5 structure and Endtime given 
%%   Start
%%   Duration
%%   Resource
%%
%% Output:
%%   Task
%%   EndTime
%%
%% Normal usage is with maplist
%%   % ...
%%   maplist(create_task,StarTimes,Durations,Resources, Tasks, EndTimes),
%%   cumulative(Tasks,[limit(Capacity)]),
%%   % ...
%%
create_task(StartTime,Duration,Resource,Task,EndTime) :-
        EndTime #= StartTime+Duration,
        Task = task(StartTime,Duration,EndTime,Resource,_).


%%
%% The other representation of cumulative constraint
%% (this is the one I'm more customed to).
%%
%%    my_cumulative(Starts,Durations,Resources,Limit)
%%
%% Converts to clpfd's
%%     cumulative(Tasks,[limit(Limit)])
%%
my_cumulative(Starts,Durations,Resources,Limit) :-
        maplist(create_task2,Starts,Durations,Resources, Tasks),
        cumulative(Tasks,[limit(Limit)]).

%%
%% Create a cumulative task.
%% (not exported)
%%
create_task2(StartTime,Duration,Resource,Task) :-
        EndTime #= StartTime+Duration,
        Task = task(StartTime,Duration,EndTime,Resource,_).


%%
%% distribute(Card, Value, X)
%%
%% Global constraint distribute(Card, Values, X)
%% 
%% Requires that Card[I] is the number of occurences of Value[I] in the list X.
%%
%% Assumption:
%% The values in Value should be distinct, but
%% - they don't have to be ordered,
%% - they don't have to be in a complete range of values.
%%
%% Picat code:
%% foreach(I in 1..CardLen)
%%    Sum #= sum([(ValueI #= X) : J in 1..XLen,
%%                   element(I,Value,ValueI),
%%                   element(J,X,XJ)]),
%%    element(I,Card,Sum)
%% end,
%%
distribute(Card, Value, X) :-
        length(Card,CardLen),
        length(Value,CardLen), 
        
        all_different(Value),      
        numlist(1,CardLen,Is),
        maplist(distribute_(Card,Value,X),Is).

distribute_(Card,Value,X,I) :-
        element(I,Value,ValueI),
        length(X,XLen),
        numlist(1,XLen,Js),
        element(I,Card,Sum),
        distribute1_sum(Js,ValueI,X,0,Sum).
        
distribute1_sum([],_ValueI,_X,Sum,Sum).
distribute1_sum([J|Js],ValueI,X,Sum0,Sum) :-
        B in 0..1,
        element(J,X,XJ),
        ValueI #= XJ #<==> B #= 1,
        Sum1 #= Sum0 + B,
        distribute1_sum(Js,ValueI,X,Sum1,Sum).

%%
%% sliding_sum(Low, Up, Seq, Variables)
%%
%% Ensure that the sum of all subsequences of Seq values in Variables,
%% is between Low and Up.
%%
%% Note: Seq must be instantiated, but neither Low or Up has
%% to be (the result may be weird unless they are, though).
%%
sliding_sum(Low, Up, Seq, Variables) :-
        Low #=< Up,        
        length(Variables,Len),
        U #= Len-Seq+1,
        numlist(1,U,Is),
        maplist(sliding_sum_(Variables,Low,Up,Seq),Is).
sliding_sum_(Variables,Low,Up,Seq,I) :-
        ISeq1 #= I+Seq-1,
        numlist(I,ISeq1,Js),
        sliding_sum_sum(Js,Variables,0,Sum),
        Sum #>= Low,
        Sum #=< Up.

sliding_sum_sum([],_Variables,Sum,Sum).
sliding_sum_sum([J|Js],Variables,Sum0,Sum) :-
        element(J,Variables,VJ),
        Sum1 #= Sum0 + VJ,
        sliding_sum_sum(Js,Variables,Sum1,Sum).
        
%%
%% sliding_sum(Sum, Seq, Variables)
%%
%% Ensure that the sum of all subsequences of length Seq in Variables is Sum.
%%
sliding_sum(Sum, Seq, Variables) :-
        sliding_sum(Sum, Sum, Seq, Variables).


%%
%% circuit_path(Circuit,Path)
%%
%% As circuit/1 but with the path as second parameter.
%%
circuit_path(X,Z) :-
   length(X,N),
   length(Z,N),
   Z ins 1..N,

   %
   % The main constraint is that Z[I] must not be 1 
   % until I = N, and for I = N it must be 1.
   %
   all_different(X),
   all_different(Z),
   % all_distinct(X), % slower
   % all_distinct(Z), % slower

   % put the orbit of x[1] in in z[1..n]
   element(1,X,X1),
   element(1,Z,Z1),
   X1 #= Z1,
   
   % when I = N it must be 1
   element(N,Z,ZN),
   ZN #= 1,

   %
   % Get the orbit for Z.
   %
   numlist(2,N,Is),
   maplist(orbit(X,Z),Is).


%%
%% orbit(Z,X,I)
%%
%% helper predicate for circuit_path/2.
%%
%% foreach(I in 2..N)
%%   element(Z[I-1],X,Z[I])
%% end.
%%
orbit(X,Z,I) :-
        I1 #= I-1,
        element(I1,Z,ZI1),
        element(I,Z,ZI),
        element(ZI1,X,ZI).


%
% nvalue(?N,?X)
%
% Requires that the number of distinct values in X is N.
%
nvalue(N, X) :-
        get_min_domain(X,LB),
        get_max_domain(X,UB),
        numlist(LB,UB,Is),
        maplist(nvalue_(X),Is,Sums),
        sum(Sums,#=,N).

%%
%% Is there _any_ occurrence of I in X? (Boolean 0..1)
%%
nvalue_(X,I,B) :-
        count_occurrences(X,I,ISum),
        B in 0..1,
        ISum #> 0 #<==> B #= 1.

%%
%% Get the lower domain limit in X
%%
get_min_domain(X,Min) :-
        maplist(fd_inf,X,Inf),
        min_list(Inf,Min).

%%
%% Get the upper domain limit in X
%%
get_max_domain(X,Max) :-
        maplist(fd_sup,X,Sup),
        min_list(Sup,Max).

%
% nvalues(X,Op,N)
%
% Requires that the number of distinct values in the array X is 
%    Op N 
% where
% Op is either one of 
%   #=, #<, #=<, #>=, or #>
% (this is not checked though)    
%
nvalues(X, Op, N) :-
   nvalue(M,X),
   call(Op, M, N).


%%
%% exactly(?N,?X,?N)
%%
%% Requires exactly N variables in X to take the value V.
%%
exactly(N, Xs, V) :-
   length(Xs,Len),
   length(Bs,Len),
   Bs ins 0..1,
   maplist(exactly_(V),Xs,Bs),
   sum(Bs,#=,N).

exactly_(V,X,B) :-
        X #= V #<==> B #= 1.

%%
%% Alternative implementation of exactly/3 is
%%
%%   exactly(N,Xs,V) :-
%%      count_occurrences(Xs,V,N).
%%




%%
%% alldifferent_modulo(Xs,M)
%%
%% Enforce all variables of the collection Xs to have a distinct 
%% rest when divided by M.
%%
alldifferent_modulo(Xs,M) :-
        maplist(modulo_m(M),Xs,Ms),
        all_different(Ms).

modulo_m(Mod,X,M) :-
        M #= X mod Mod.



%%
%% all_differ_from_at_least_k_pos(K,X)
%%
%% Enforce all pairs of distinct vectors of the X collection to differ 
%% from at least K positions.
%%
all_differ_from_at_least_k_pos(K,X) :-
        length(X,Len),
        findall([I,J],
                (between(1,Len,I),
                 between(1,Len,J),
                 I #\= J
                ),
                IJs),
        maplist(differ_by_k(K,X),IJs).

differ_by_k(K,X,[I,J]) :-
        nth1(I,X,XIs),
        nth1(J,X,XJs),
        differ_by_k_(XIs,XJs,0,DiffSum),
        DiffSum #>= K.

differ_by_k_([],_XJs,Sum,Sum).
differ_by_k_([XI|XIs],[XJ|XJs],Sum0,Sum) :-
        B in 0..1,
        XI #\= XJ #<==> B #= 1,
        Sum1 #= Sum0 + B,
        differ_by_k_(XIs,XJs,Sum1,Sum).
        

%%
%% alldifferent_cst(Xs, Cst)
%%
%%  For all pairs of items (X[i], X[j]) (i!=j) of the 
%%  collection X enforce 
%%  X[i].var+X[i].cst != X[j].var+X[j].cst.
%%
alldifferent_cst(Xs, Cst) :-
        %% Res = [$(X + C) :  {X,C} in zip(Xs,Cst)],
        maplist(plus,Xs,Cst,Res),
        all_different(Res).
plus(A,B,C) :- C #= A + B.


%%
%% all_equal(X)
%% 
%% Enforce all variables of the collection X to take the same value.
%%
all_equal([X|Xs]) :-
        maplist(eq(X),Xs).
eq(X,Y) :- X #= Y.



%%
%% all_min_dist(C,X)
%% 
%% Enforce for each pair (vari, varj), i < j of distinct variables of the 
%% collection X that
%%
%%   |X[i] - X[j]| >= C.
%%
all_min_dist(_C,[]).
all_min_dist(C, [H|Ts]) :-
        all_min_dist_(C,H,Ts),
        all_min_dist(C,Ts).

all_min_dist_(_C,_H,[]).
all_min_dist_(C,H,[T|Ts]) :-
        abs(H-T) #>= C,
        all_min_dist_(C,H,Ts).


%%
%% alldiffer_on_intersection(Xs,Ys) 
%%
%% The values that both occur in the Xs and Ys collections 
%% have only one occurrence.
%%
alldiffer_on_intersection(Xs,Ys) :-
   count_A_in_B(Xs,Ys),
   count_A_in_B(Ys,Xs).

count_A_in_B([],_Bs).
count_A_in_B([A|As],Bs) :-
        count_occurrences(Bs,A,Count),
        Count #=< 1,
        count_A_in_B(As,Bs).


%%
%% among(N, X, V)
%%
%% Requires exactly N variables in X to take one of the values in V.
%%
among(N,X,V) :-
        among_(V,X,0,S),
        S #= N.

among_([],_X,Sum,Sum).
among_([V|Vs],X,Sum0,Sum) :-
        count_occurrences(X,V,VSum),
        Sum1 #= Sum0 + VSum,
        among_(Vs,X,Sum1,Sum).


%%
%%  among_seq(Low, High, SeqLen, X, V)
%%
%%  Ensures that all sequences of length SeqLen in the list X 
%%  contains at least Low and at most High occurrences of V.
%%
among_seq(Low,High,SeqLen,X,V) :-
   length(X,Len),
   Size #= Len-SeqLen+1,
   numlist(1,Size,Is),
   maplist(among_seq_(Low,High,SeqLen,X,V),Is).

among_seq_(Low,High,SeqLen,X,V,I) :-
        Len #= I+SeqLen-1,
        numlist(I,Len,Js),
        extract_from_indices(Js,X,Seq),
        among_range(Low,High,Seq,V).
        


%%
%%  among_range(Low, High, X, V)
%%
%%  Ensures that the list X contains at least Low and at most High
%%  occurrences of the elements in V.
%%
among_range(Low, High,X,V) :-
        among(N,X,V),
        N #>= Low,
        N #=< High.




%%% EXPERIMENTAL %%%
%%%
%%
%% print_attrs_list(Vs)
%%
%% Prints the get_attrs/2 value (attributed variables info)
%%
print_attrs_list([]).
print_attrs_list([V|Vs]) :-
        get_attrs(V,Attributes),
        writeln(v=V),
        writeln(attributes=Attributes),
        print_attrs_list(Vs).

