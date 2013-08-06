/*

  Lecture series puzzle (Dell Logic Puzzles) in SICStus Prolog.

  From http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
  """
  Title: Lecture Series
  Author: Alex Knight
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 10
  Stars: 2

  Last week at school was made varied by a series of lectures, one
  each day 
   (Monday through Friday), 
  in the auditorium. None of the lectures was particularly 
  interesting 
     (on choosing a college, physical hygiene, modern art, nutrition, 
     and study habits), 
   but the students figured that anything that got them out of fourth 
   period was okay. The lecturers were 
       two women named Alice and Bernadette, and three men 
       named Charles, Duane, and Eddie; 
   last names were 
       Felicidad, Garber, Haller, Itakura, and Jeffreys. 
   Can you find each day's lecturer and subject?

  1. Alice lectured on Monday.
  2. Charles's lecture on physical hygiene wasn't given on Friday.
  3. Dietician Jeffreys gave the lecture on nutrition.
  4. A man gave the lecture on modern art.
  5. Ms. Itakura and the lecturer on proper study habits spoke on 
     consecutive days, in one order or the other.
  6. Haller gave a lecture sometime after Eddie did.
  7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
  
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/lecture_series.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/lecture_series.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 5,

        Monday = 1,
        Tuesday = 2,
        Wednesday = 3,
        Thursday = 4,
        Friday = 5,
        Days = [Monday,Tuesday,Wednesday,Thursday,Friday],
        DaysS = ['Monday','Tuesday','Wednesday','Thursday','Friday'],

        Lectures = [_ChoosingCollege, PhysicalHygiene, ModernArt, Nutrition, 
                    StudyHabits],
        LecturesS = ['Choosing College', 'Physical Hygiene', 'Modern Art', 'Nutrition', 
                    'Study Habits'],
        domain(Lectures,1,N),

        FirstName = [Alice, Bernadette, Charles, Duane, Eddie],
        FirstNameS = ['Alice', 'Bernadette', 'Charles', 'Duane', 'Eddie'],
        domain(FirstName,1,N),

        LastName  = [Felicidad, _Garber, Haller, Itakura, Jeffreys],
        LastNameS  = ['Felicidad', 'Garber', 'Haller', 'Itakura', 'Jeffreys'],
        domain(LastName,1,N),

        all_different(Lectures),
        all_different(FirstName),
        all_different(LastName),


        % 1. Alice lectured on Monday.
        Alice #= Monday,

        % 2. Charles's lecture on physical hygiene wasn't given on
        % Friday.
        Charles #= PhysicalHygiene,
        Charles #\= Friday,
        PhysicalHygiene #\= Friday,

        % 3. Dietician Jeffreys gave the lecture on nutrition.
        Jeffreys #= Nutrition,

        % 4. A man gave the lecture on modern art.
        (
            ModernArt #= Charles 
        #\/ 
            ModernArt #= Duane   
        #\/
            ModernArt #= Eddie
        ),

        % 5. Ms. Itakura and the lecturer on proper study habits spoke on 
        %    consecutive days, in one order or the other.
        (
            Itakura #= Alice 
        #\/ 
            Itakura #= Bernadette
        ),
        (
          abs(Itakura - StudyHabits) #= 1
        ),
        
        % 6. Haller gave a lecture sometime after Eddie did.
        Haller #> Eddie,

        % 7. Duane Felicidad gave his lecture sometime before the
        %    modern art lecture. 
        Duane #= Felicidad,
        Duane #< ModernArt,
        Felicidad #< ModernArt,

        % search
        append(Lectures,FirstName,Vars1),
        append(Vars1,LastName, Vars),
        labeling([], Vars),

        % print solution
        print_all(Days,DaysS),
        print_all(FirstName,FirstNameS),
        print_all(LastName,LastNameS),
        print_all(Lectures,LecturesS).
        



print_all(X,S) :-
        length(X,Len),
        (for(I,1,Len), param(X,S,Format) do            
             nth1(IX,X,I),
             nth1(IX,S,This),
             format('<~w>\t', [This])
        ),nl.
