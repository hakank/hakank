/*

  The Family Puzzle in SWI Prolog

  From Drools Puzzle Round 2: The Familiy Puzzle
  http://blog.athico.com/2007/08/drools-puzzle-round-2-familiy-puzzle.html
  """
  
  * Three men, Abel, Locker and Snyder are married to Edith, Doris and Luisa, 
    but not necessarily in this order.
  * Each couple has one son.
  * The sons are called Albert, Henry and Victor.
  * Snyder is nor married to Luisa, neither is he Henry's father.
  * Edit is not married to Locker and not Albert's mother.
  * If Alberts father is either Locker or Snyder, then Luisa is Victor's mother.
  * If Luisa is married to Locker, then Doris is not Albert's mother. 
  
  Who is married to whom and what are their sons called?

  Taken from the German book "Denken als Spiel" by Willy Hochkeppel, 1973 
  (Thinking as a Game).  
  """
  
  Solutions and discussions
  http://ningning.org/blog2/2008/05/25/drools-puzzles-result-round-2-the-familiy-puzzle
  http://rbs.gernotstarke.de/samples/page21/page21.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 3,

        Abel = 1,
        Locker = 2,
        Snyder = 3,
        Men = [Abel,Locker,Snyder],
        MenS = ['Abel','Locker','Snyder'],

        WomenS = ['Edith', 'Doris', 'Luisa'],
        Women = [Edith, Doris, Luisa],
        Women ins 1..N,

        SonsS = ['Albert','Henry', 'Victor'],
        Sons = [Albert,Henry, Victor],
        Sons ins 1..N,


        all_different(Women),
        all_different(Sons),

        % Snyder is nor married to Luisa, neither is he Henry's father.
        Snyder #\= Luisa,
        Snyder #\= Henry,
        
        % Edith is not married to Locker and not Albert's mother.
        Edith #\= Locker,
        Edith #\= Albert,
        
        % If Alberts father is either Locker or Snyder, 
        % then Luisa is Victor's mother.
        (
            (Albert #= Locker #\/ Albert #= Snyder) #==> 
                Luisa #= Victor
        ),

        % If Luisa is married to Locker, 
        % then Doris is not Albert's mother. 
        (
            Luisa #= Locker #==> 
                Doris #\= Albert
        ),

        flatten([Women,Sons], Vars),
        labeling([],Vars),

        writeln(men=Men),
        writeln(women=Women),
        writeln(sons=Sons),
        nl,
        findall([Man,Woman,Son],
                (between(1,N,I),
                 lookup_list(I,Men,MenS,Man),
                 lookup_list(I,Women,WomenS,Woman),
                 lookup_list(I,Sons,SonsS,Son)
                ),
                Sol),
        maplist(writeln,Sol),
        nl.
        

%%
%% lookup_list(I,Ls,S,V)
%%
%% Lookup the corresponding element V in S of the I'th element in Ls.
%% I.e.:
%%   S[Ls[V]] = V
%%
lookup_list(I,Ls,S,V) :-
   nth1(I, Ls, E),
   nth1(E, S, V).
