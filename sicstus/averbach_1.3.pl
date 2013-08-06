/*

  Recreational mathematics in SICStus Prolog.

  Problem 1.3 from 
  Averbach & Chein "Problem Solving Through Recreational Mathematics", page 2.
  """
  Armand Alloway, Basil Bennington, Col. Carton Cunningham, Durwood Dunstan, and 
  Everitt Elmsby, Esq are the five senior members of the Devonshire Polo Club. 
  Each owns a pony that is named of the wife of one of the others.
  
  - Mr Alloway's pony is named Geogette; 
  - Col Cunningham owns Jasmine
  - Mr Elmsby owns Inez
  - Francine, owned by Mr Dunstan is named after Alloways wife
  - Georgettes husband owns the pony that is named after Mr Bennington's wife
  - Helene Cunningham is the only wife who knows how to ride a horse.
  
  Who is Jasmine's husband? Who owns Helene?
  """


  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.3.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-

        % the men
        Men = [Alloway,Bennington,Cunningham,Dunstan,Elmsby],
        Men = [1,2,3,4,5],
        % the name of the wifes, and the names of the ponies
        Francine = 1,
        Georgette = 2,
        Helene = 3,
        Inez = 4,
        Jasmine = 5,

        length(Wife,5),
        domain(Wife,1,5),
        length(Pony,5),
        domain(Pony,1,5),
               
        all_different(Wife),
        all_different(Pony),

        % wife and pony don't have the same name
        ( foreach(W, Wife),
          foreach(P, Pony) do
              P #\= W
        ),

        % Mr Alloway's pony is named Geogette, 
        element(Alloway,Pony,PonyAlloway),
        PonyAlloway #= Georgette,
        element(Alloway,Wife,WifeAlloway),
        WifeAlloway #\= Georgette,

        % Col Cunningham owns Jasmine
        element(Cunningham,Pony,PonyCunningham),
        PonyCunningham #= Jasmine,
        element(Cunningham,Wife,WifeCunningham),
        WifeCunningham #\= Jasmine,
 
        % Mr Elmsby owns Inez
        element(Elmsby,Pony,PonyElmsby),
        PonyElmsby #= Inez,
        element(Elmsby,Wife,WifeElmsby),
        WifeElmsby #\= Inez,

        % Francine, owned by Mr Dunstan is named after Alloways wife
        element(Dunstan,Pony,PonyDunstan),
        PonyDunstan #= Francine,
        WifeAlloway #= Francine,

        % Georgettes husband owns the pony that is named after 
        % Mr Bennington's wife
        % This is translated to:
        % "There is an X such that X is is Georgettes husband and X 
        % owns a pony with the same name as Bennington's wife."
        X in 1..5,
        element(X,Wife,WifeX),
        element(X,Pony,PonyX),
        element(Bennington,Wife,WifeBennington),
        (WifeX #= Georgette #/\ PonyX #= WifeBennington),

        % Helene Cunningham is the only wife who knows how to ride a horse.
        WifeCunningham = Helene,

        append(Wife,Pony,Vars),
        labeling([], Vars),

        write(wife:Wife),nl,
        write(pony:Pony),nl,
        fd_statistics.

