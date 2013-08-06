/*

  Recreational mathematics in ECLiPSe.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.3.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/averbach_1.3.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
:-lib(listut).
%:-lib(propia).



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
        Wife :: 1..5,
        length(Pony,5),
        Pony :: 1..5,
               
        alldifferent(Wife),
        alldifferent(Pony),

        % wife and pony don't have the same name
        ( foreach(W, Wife),
          foreach(P, Pony) do
              P #\= W
        ),

        % Mr Alloway's pony is named Geogette, 
        nth1(Alloway,Pony,PonyAlloway),
        PonyAlloway #= Georgette,
        nth1(Alloway,Wife,WifeAlloway),
        WifeAlloway #\= Georgette,

        % Col Cunningham owns Jasmine
        nth1(Cunningham,Pony,PonyCunningham),
        PonyCunningham #= Jasmine,
        nth1(Cunningham,Wife,WifeCunningham),
        WifeCunningham #\= Jasmine,
 
        % Mr Elmsby owns Inez
        nth1(Elmsby,Pony,PonyElmsby),
        PonyElmsby #= Inez,
        nth1(Elmsby,Wife,WifeElmsby),
        WifeElmsby #\= Inez,

        % Francine, owned by Mr Dunstan is named after Alloways wife
        nth1(Dunstan,Pony,PonyDunstan),
        PonyDunstan #= Francine,
        WifeAlloway #= Francine,

        % Georgettes husband owns the pony that is named after 
        % Mr Bennington's wife
        % This is translated to:
        % "There is an X such that X is is Georgettes husband and X 
        % owns a pony with the same name as Bennington's wife."
        X :: 1..5,
        nth1(X,Wife,WifeX),
        nth1(X,Pony,PonyX),
        nth1(Bennington,Wife,WifeBennington),
        (WifeX #= Georgette, PonyX #= WifeBennington),

        % Helene Cunningham is the only wife who knows how to ride a horse.
        WifeCunningham #= Helene,

        term_variables([Wife,Pony],Vars),
        labeling(Vars),

        write(wife:Wife),nl,
        write(pony:Pony),nl.

