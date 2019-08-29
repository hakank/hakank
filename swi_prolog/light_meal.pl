/*

  Light meal problem in SWI Prolog

  From A. Colmerauer: "An introduction to Prolog III", 1990]
  """
  [T]his is our first example of a Prolog III program. It is an 
  improvement on a program which is perhaps too well-known, but which 
  remains a useful pedagogical tool: the calculation of 
  well-balanced meals...

  LightMeal(h,m,d) ->
    HorsDoeuvre(h,i),
    MainCourse(m,j),
    Dessert(d,k),
    {i=0,j=0,k=0,i+j+k=10};

  MainCourse(m,i) -> Meat(m,i);
  MainCourse(m,i) -> Fish(m,i);

  HorsDoeuvre(radishes,1) ->;
  HorsDoeuvre(pâté,6) ->;

  Meat(beef,5) ->;
  Meat(pork,7) ->;

  Fish(sole,2) ->;
  Fish(tuna,4) ->;

  Dessert(fruit,2) ->;
  Dessert(icecream,6) ->; 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
  lightmeal(A,AC,M,MC,D,DC,Cal),
  writeln(horsdoevre=[A,AC]),
  writeln(maincourse=[M,MC]),
  writeln(dessert=[D,DC]),
  writeln(cal=Cal),
  nl,
  fail,  

  nl.

go.

go2 :-
  balanced_meal(E,P,D,V),
  writeln([E,P,D,cal=V]),

  fail,
  
  nl.

go2.


lightmeal(A, AC, M, MC, D, DC,Cal) :-
        horsdoevre(A,AC),
        maincourse(M,MC),
        dessert(D, DC),
        AC #> 0, MC #> 0, DC #> 0,
        Cal #= AC+MC+DC,
        Cal #=< 10.

maincourse(M,I) :-
        meat(M,I).
maincourse(M,I) :-
        fish(M,I).

horsdoevre(radishes, 1).
horsdoevre('pâté', 6).

meat(beef, 5).
meat(pork, 7).

fish(sole, 2).
fish(tuna,4).

dessert(fruit, 2).
dessert(icecream, 6).


hors_d_oeuvre(artichauts_melanie).
hors_d_oeuvre(truffes_sous_le_sel).
hors_d_oeuvre(cresson_oeuf_poche).

meat(grillade_de_boeuf).
meat(poulet_au_tilleul).

fish(bar_aux_algues).
fish(chapon_farci).

dessert(sorbet_aux_poires).
dessert(fraises_chantilly).
dessert(melon_en_surprise).

% "main-course"
main_course(P) :-
        meat(P).
main_course(P) :-
        fish(P).

% "composition of a meal"
meal(E,P,D) :-
        hors_d_oeuvre(E),
        main_course(P),
        dessert(D).

% "calorific value of a portion"
calories(artichauts_Melanie,150).
calories(cresson_oeuf_poche,202).
calories(truffes_sous_le_sel,212).
calories(grillade_de_boeuf,532).
calories(poulet_au_tilleul,400).
calories(bar_aux_algues,292).
calories(chapon_farci,254).
calories(sorbet_aux_poires,223).
calories(fraises_chantilly,289).
calories(melon_en_surprise,122).

% "calorific value of a meal"
value(E,P,D,V) :-
        calories(E,X),
        calories(P,Y),
        calories(D,Z),
        V #= X + Y + Z.

% "balanced meal"
balanced_meal(E,P,D,V) :-
        meal(E,P,D),
        value(E,P,D,V),
        V #< 800.

