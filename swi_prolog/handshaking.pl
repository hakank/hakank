/*

  Halmos' handshake problem in SWI Prolog

  Problem formulation from Alloy (examples/puzzles/handshake)
  """
  Alloy model of the Halmos handshake problem
  
  Hilary and Jocelyn are married. They invite four couples who are friends for dinner. When
  they arrive, they shake hands with each other. Nobody shakes hands with him or herself
  or with his or her spouse. After there has been some handshaking, Jocelyn jumps up on
  a chair and says "Stop shaking hands!", and then asks how many hands each person has
  shaken. All the answers are different. How many hands has Hilary shaken?
  
  The Alloy model represents the problem as a set of constraints. Properties of the spouse
  relationship and of handshaking in general are given as facts. The particular situation
  is cast as a function.
  
  There are 9 people answering, and all answers are different. Nobody can shake more than
  8 hands. So answers must be 0..8. The one (p8 say) who answered 8 has shaken everybody's
  hand except for his or her own, and his or her spouse's. Now consider the person who shook
  0 hands (p0 say). The persons p0 and p8 are distinct. If they are not married, then p8 cannot
  have shaken 8 hands, because he or she did not shake the hand of p0 or of his or her spouse.
  So p8's spouse to p0. Now imagine Jocelyn asking the question again, with p0 and p8 out of
  the room, and excluding hand shakes with them. Since p8 shook hands with everyone else
  except p0 and p8, everyone gives an answer one smaller than they did before, giving 0..6.
  The argument now applies recursively. So Hilary is left alone, having shaken 4 hands. 
  """
  Alloy is here: http://alloy.mit.edu/alloy
  
  Also, see the following that discuss Halmos' Handshake problem
  http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/mathhome.htm#halmos
      http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/shakeanswer.htm
  
  The origin of the problem seems to be
  P.R. Halmos: "To Count or to Think, That is the Question", page 1ff
  http://bernoulli.math.rug.nl/vorigelezingen/lezing03/lezing03.pdf


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

/*

  The solution for N=10 (with symmetry breaking)

  x:[4,4,0,8,1,7,2,6,3,5]

  Who shake hands with whom:
  [0,0,0,1,0,1,0,1,0,1]
  [0,0,0,1,0,1,0,1,0,1]
  [0,0,0,0,0,0,0,0,0,0]
  [1,1,0,0,1,1,1,1,1,1]
  [0,0,0,1,0,0,0,0,0,0]
  [1,1,0,1,0,0,1,1,1,1]
  [0,0,0,1,0,1,0,0,0,0]
  [1,1,0,1,0,1,0,0,1,1]
  [0,0,0,1,0,1,0,1,0,0]
  [1,1,0,1,0,1,0,1,0,0]

  Person 1 shake hands with [4,6,8,10] (4 shakes)
  Person 2 shake hands with [4,6,8,10] (4 shakes)
  Person 3 shake hands with [] (0 shakes)
  Person 4 shake hands with [1,2,5,6,7,8,9,10] (8 shakes)
  Person 5 shake hands with [4] (1 shakes)
  Person 6 shake hands with [1,2,4,7,8,9,10] (7 shakes)
  Person 7 shake hands with [4,6] (2 shakes)
  Person 8 shake hands with [1,2,4,6,9,10] (6 shakes)
  Person 9 shake hands with [4,6,8] (3 shakes)
  Person 10 shake hands with [1,2,4,6,8] (5 shakes)


*/

go :-
        N = 4, % 5 pairs
        time(handshake(N,symmetrybreaking,print)).

go1 :-
        N = 10,
        length(X,N),
        calculate_x(N,X),
        writeln(X),
        nl.

go2 :-
        N = 10,
        findall(_,handshake(N,nosymmetrybreaking,noprint),L),
        length(L,Len),
        writeln(len:Len),
        nl.

go3 :-
        N = 100,
        time(handshake(N,symmetrybreaking,print)).

go4 :-
        N = 300,
        time(handshake(N,symmetrybreaking,noprint)).

go5 :-
        N = 350,
        time(handshake(N,symmetrybreaking,print)).



handshake(N,Symmetry,Print) :-

        %% Nunber of handshakes for person X[I].
        %% A person can shake max n-2 hands
        %%   coded X = [Pair1a,Pair1b,  Pair2a,Pair2b, ...]
        %%
        length(X, N),
        N2 #= N-2,
        X ins 0..N2,

        % who shake with whom:
        %  (not him/herself and not his/her spouse)
        new_matrix(N,N,0..1,Y),
        
        %% We assume that Hilary is in position x[1]
        %% (and Hilary's spouse - Jocelyn - in x[2])
        %% All except Hilary's counts are different
        X = [_|X2],        
        % all_different(X2),
        all_distinct(X2),
        
        %% don't shake hand with spouse
        dont_shake_hand_with_spouse(Y,N),

        %% don't shake hand with oneself
        numlist(1,N,Is),
        maplist(no_self_shakes(Y),Is),

        %% how many hands has X[I] shaken
        maplist(sum_shaken_hands,X,Y),

        %% symmetry of handshaking:
        %%   a shake hands with b <-> b shake hands with a
        symmetry_of_handshaking(N,Y),
        
        %% Symmetry breaking which orders the other couples (besides the hosts)
        %% Without it: 384 solutions (all x = [4,4,.....])
        %% With it: 1 solution: x: [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] 
        %%                         (since we order 0,1,2,3 shakes)
        %% 
        %% Note that all number of handshaking of the pairs sums to 8, 
        %% i.e. 4+4, 0+8, 1+7, 2+6, 3+5
        %% More general: The number of handshaking per pair sums to n-2.
        %%
        %% Note: We calculate this in calculate_x/2 and exploit it
        %% when using symmetry breaking.
        %%
        (Symmetry == symmetrybreaking
        ->
         ((N #> 4, N mod 2 #= 0)
         ->
          calculate_x(N,X)
          ;
          true
         ),
         symmetry_breaking(N,X)
        ;
         true
        ),
        
        %
        % search
        %
        writeln(solve),
        flatten([X,Y], Vars),
        labeling([down],Vars),
        (
         Print = print
        ->
         writeln(x:X),
         nl,
         writeln('Who shake hands with whom:'),
         maplist(writeln,Y),
         nl,
         findall([I,Row,RowLen],
                 (between(1,N,I),
                  findall(J,
                          (
                           between(1,N,J),
                           matrix_element5(Y,I,J,1)
                          ),
                          Row
                         ),
                  length(Row,RowLen)
                 ),
                 Sol),
         maplist(format("Person ~d shake hands with ~w (~d shakes)~n"),Sol),
         nl
        ;
         true
        ).


%% don't shake hand with spouse
dont_shake_hand_with_spouse(Y,N) :-
        N21 #= (N div 2)-1,
        numlist(0,N21,Is),
        maplist(dont_shake_hand_with_spouse_(Y),Is).
dont_shake_hand_with_spouse_(Y,I) :-
        I1 #= 2*I+1,
        I2 #= 2*I+2,
        matrix_element5(Y,I1,I2,0),
        matrix_element5(Y,I2,I1,0).


%% don't shake hand with oneself
no_self_shakes(Y,I) :-
        matrix_element2(Y,I,I,0).

%% how many hands has X[I] shaken?
sum_shaken_hands(X,YRow) :-
        sum(YRow,#=,X).

%% symmetry of handshaking:
%%    a shake hands with b <-> b shake hands with a
symmetry_of_handshaking(N,Y) :-
        findall([I,J],
                (between(1,N,I),
                 between(1,N,J),
                 I #< J
                ),
                IJs),
        maplist(symmetry_of_handshaking_(Y),IJs).
symmetry_of_handshaking_(Y,[I,J]) :-
        matrix_element5(Y,I,J,YIJ),
        matrix_element5(Y,J,I,YIJ).

symmetry_breaking(N,X) :-
        N2 #= (N div 2) - 2,
        findall(I2,
                (between(0,N2,I),
                 I2 #= 3+2*I
                ),
                Is),
        extract_from_indices(Is,X,Xs),
        increasing(Xs),
        maplist(symmetry_breaking2(X),Is).
symmetry_breaking2(X,I) :-
        I1 #= I+1,
        element(I,X,XI),
        element(I1,X,XI1),
        XI #< XI1.
        

%%
%% The formula for calulating the distinct solution of X
%% (the number of shaken hands) for a certain N and with symmetry breaking:
%%
%%   N2 #= N-2
%%   [N div 2, N div 2, 0, N2-2, 1, N2-3, 2, N2-4, 3, ...,  1+ (N div 2)]
%%
%% For N=10, X = [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] 
%%
calculate_x(N,X) :-
        Ndiv2_1 #= (N div 2)-1,       
        N2 #= N - 2,
        element(1,X,Ndiv2_1),
        element(2,X,Ndiv2_1),
        element(3,X,0),
        element(4,X,N2),
        numlist(5,N,Is),
        maplist(calculate_x_(N,X),Is).
calculate_x_(N,X,I) :-
        (I mod 2 #= 1
        ->
         T #= (I div 2) -1
        ;
         T #= N-(I div 2)
        ),
        element(I,X,T).