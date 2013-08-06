/*

  Lichtenstein coloring problem in ECLiPSe.

  From 
  http://bit-player.org/2008/the-chromatic-number-of-liechtenstein
  and
  """
  It seems that Liechtenstein is divided into 11 communes, which 
  emphatically do not satisfy the connectivity requirement of the four 
  color map theorem. Just four of the communes consist of a single 
  connected area (Ruggell, Schellenberg and Mauren in the north, and 
  Triesen in the south). 
  ...
  In the map above, each commune is assigned its own color, and so we 
  have an 11-coloring. Itâ€™s easy to see we could make do with fewer 
  colors, but how many fewer? I have found a five-clique within the map; 
  that is, there are five communes that all share a segment of border 
  with one another. It follows that a four-coloring is impossible. Is 
  there a five-coloring? What is the chromatic number of Liechtenstein?
  """
 
  Also see
  http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/lichtenstein_coloring.mzn
  * Comet   : http://www.hakank.org/comet/lichtenstein_coloring.co
  * SICStus : http://www.hakank.org/sicstus/lichtenstein_coloring.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).
:-lib(branch_and_bound).
%:-lib(ic_global).
%:-lib(ic_search).

%:-lib(propia).



go :-
        %
        % communes
        %
        BalzersC      = 1,
        EschenC       = 2,
        GamprinC      = 3,
        MaurenC       = 4,
        PlankenC      = 5,
        RuggellC      = 6,
        SchaanC       = 7,
        SchellenbergC = 8,
        TriesenC      = 9,
        TriesenbergC  = 10,
        VaduzC        = 11,
        
        %
        % enclaves/exclaves
        %
        Balzers1     = 1,
        Balzers2     = 2,
        Balzers3     = 3,
        Eschen1      = 4,
        Eschen2      = 5,
        Eschen3      = 6,
        Gamprin1     = 7,
        Gamprin2     = 8,
        Mauren       = 9,
        Planken1     = 10,
        Planken2     = 11,
        Planken3     = 12,
        Planken4     = 13,
        Ruggell      = 14,
        Schaan1      = 15,
        Schaan2      = 16,
        Schaan3      = 17,
        Schaan4      = 18,
        Schaan5      = 19,
        Schellenberg = 20,
        Triesen      = 21,
        Triesenberg1 = 22,
        Triesenberg2 = 23,
        Vaduz1       = 24,
        Vaduz2       = 25,
        Vaduz3       = 26,
        Vaduz4       = 27,
        Vaduz5       = 28,
        Vaduz6       = 29,
        
        
        NumCommunes = 11,
        NumColors   = 11,
        NumEnclaves = 29,
        
        %
        % the enclaves and corresponding commune
        %
        CC = 
        [
            BalzersC, BalzersC, BalzersC, 
            EschenC, EschenC, EschenC, 
            GamprinC, GamprinC, 
            MaurenC, 
            PlankenC, PlankenC, PlankenC, PlankenC, 
            RuggellC, 
            SchaanC, SchaanC, SchaanC, SchaanC, SchaanC, 
            SchellenbergC, 
            TriesenC, 
            TriesenbergC, TriesenbergC,
            VaduzC, VaduzC, VaduzC, VaduzC, VaduzC, VaduzC
        ],
        

        % This map of Lichtenstein is from
        % http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/
        Lichtenstein = [
                           [Ruggell, Schellenberg],
                           [Ruggell, Gamprin1],
                           [Schellenberg, Mauren],
                           [Schellenberg, Eschen1],
                           [Mauren, Eschen1],
                           [Gamprin1, Eschen2],
                           [Gamprin1, Vaduz2],
                           [Gamprin1, Schaan1],
                           [Gamprin1, Planken3],
                           [Gamprin1, Eschen1],
                           [Eschen1, Gamprin2],
                           [Eschen1, Planken1],
                           [Eschen2, Schaan1],
                           [Vaduz3, Schaan1],
                           [Vaduz2, Schaan1],
                           [Planken3, Schaan1],
                           [Planken2, Schaan1],
                           [Schaan1, Planken1],
                           [Schaan1, Planken4],
                           [Schaan1, Vaduz1],
                           [Gamprin2, Eschen3],
                           [Eschen3, Vaduz4],
                           [Eschen3, Schaan2],
                           [Vaduz4, Schaan2],
                           [Vaduz4, Planken1],
                           [Schaan2, Planken1],
                           [Planken1, Schaan3],
                           [Vaduz1, Triesenberg1],
                           [Vaduz1, Triesen],
                           [Planken4, Triesenberg1],
                           [Planken4, Balzers2],
                           [Balzers2, Vaduz5],
                           [Balzers2, Schaan4],
                           [Vaduz5, Schaan4],
                           [Schaan4, Triesenberg1],
                           [Schaan4, Vaduz6],
                           [Schaan4, Triesenberg2],
                           [Triesenberg1, Vaduz6],
                           [Triesenberg1, Triesen],
                           [Triesenberg1, Balzers3],
                           [Triesen, Balzers3],
                           [Triesen, Balzers1],
                           [Triesen, Schaan5],
                           [Vaduz6, Schaan5],
                           [Triesenberg2, Schaan5]
                       ],

      
        % colors for the en-/exclaves
        length(Color, NumEnclaves),
        Color :: 1..NumCommunes,

        % colors for the communes
        length(ColorCommunes,NumCommunes),
        ColorCommunes :: 1..NumColors,

        % what colors are used (for minimizing number of colors)
        length(ColorUsed,NumColors),
        ColorUsed :: 0..1,

        % all neighbours must have different colors
        ( foreach([N1,N2], Lichtenstein),
          param(Color) do
              nth1(N1,Color,CN1),
              nth1(N2,Color,CN2),
              CN1 #\= CN2
        ),
      
        % first commune (Balzers) has color 1
        nth1(1,ColorCommunes,1),

        % exclaves of the same commune must have the same color
        ( for(I,1,NumEnclaves),
          param(NumEnclaves,CC,Color) do
              ( for(J,1,NumEnclaves),
                param(CC,Color,I) do
                    I \= J -> 
                    (
                        nth1(I,CC,CCI),
                        nth1(J,CC,CCJ),
                        nth1(I,Color,CI),
                        nth1(J,Color,CJ),
                        (CCI #= CCJ) => (CI #= CJ) 
                    )
              ;
                    true
              )
        ),
          
        % connection between commune and en-/exclaves
        ( for(C,1,NumCommunes),
          param(ColorCommunes,Color,CC,NumEnclaves) do
              E :: 1..NumEnclaves,
              nth1(C,ColorCommunes,CCC),
              nth1(E,Color,CE),
              nth1(E,CC,CCE),
              indomain(E),
              CE #= CCC,
              CCE #= C
        ),

        % Color used
        ( foreach(CU,ColorUsed),
          count(IC,1,_),
          param(Color) do
              ( foreach(C,Color),
                fromto(0,In,Out,Sum),
                param(IC) do
                    Reif :: 0..1,
                    Reif #= (IC #= C),
                    Out #= In + Reif
              ),
              (CU #= 1) #= (Sum #> 0)
        ),

        % number of colors used (to minimize)
        sum(ColorUsed)#=NColors,


        % search
        term_variables([Color,ColorCommunes,ColorUsed],Vars),
        minimize(search(Vars,0,occurrence,indomain_min,complete, [backtrack(Backtracks)]),NColors),

        % output
        write(color:Color),nl,
        write(color_communes:ColorCommunes),nl,
        write(n_colors:NColors),nl,
        write(color_used:ColorUsed),nl,
        write(backtracks:Backtracks),nl.
        

% all nth1s have the same colors
same_colors(X,Colors) :-
        ( fromto(X, [This,Next | Rest], [Next|Rest],[_]),
          param(Colors) do
              nth1(This,Colors,C1),
              nth1(Next,Colors,C2),
              C1 #= C2
        ).
      
my_ordered(P,List) :-
        ( fromto(List, [This,Next | Rest], [Next|Rest],[_]),
          param(P)
        do
          call(P,This,Next)
        ).
