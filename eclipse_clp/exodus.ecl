/*

   Exodus puzzles (Dell Logic Puzzles) in ECLiPSe.

   From 
   http://brownbuffalo.sourceforge.net/ExodusClues.html
   """
   Title: Exodus
   Author: Sophy McHannot
   Publication: Dell Logic Puzzles
   Issue: April, 1998
   Page: 14
   Stars: 2

   In preparation for Passover, five children at Hebrew school 
   (Bernice,Carl,Debby,Sammy, and Ted) 
   have been chosen to present
   different parts of the story of the Exodus from Egypt 
    (burning bush, captivity,
     Moses's youth, Passover, or the Ten Commandments). 
   Each child is a different age 
     (three, five, seven, eight, or ten), 
   and the family of each child has recently made its own exodus 
   to America from a different country 
   (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
   Can you find the age of each child, his or her family's country of 
   origin, and the part of the Exodus story each related?

    1. Debby's family is from Lithuania.
    2. The child who told the story of the Passover is two years older
       than Bernice.
    3. The child whose family is from Yemen is younger than the child from
       the Ethiopian family.
    4. The child from the Moroccan family is three years older than Ted.
    5. Sammy is three years older than the child who told the story of
       Moses's youth in the house of the Pharaoh.
    6. Carl related the story of the captivity of the Israelites in Egypt.
    7. The five-year-old child told the story of the Ten Commandments.
    8. The child who told the story of the burning bush is either two or
       three years older than the one whose family came from
       Kazakhstan.

   Determine: Age -- Child -- Country -- Story
   """

   This solution is quite different from the solution given at the
   Brown Buffalo site.


   Model created by Hakan Kjellerstrand, hakank@bonetmail.com
   See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(propia).
:-lib(listut).

go :-
        N = 5,
        Range = 1..N,

        Bernice = 1,
        Carl    = 2,
        Debby   = 3,
        Sammy   = 4, 
        Ted     = 5,
        Children = [Bernice, Carl, Debby, Sammy, Ted],
        ChildrenS = ["Bernice", "Carl", "Debby", "Sammy", "Ted"],

        Story = [BurningBush, Captivity, MosessYouth, Passover,
                 TenCommandments],
        Story :: Range,
        StoryS = ["Burning Bush", "Captivity", "MosessYouth", "Passover",
                 "Ten Commandments"],

        dim(Age,[N]),
        Age :: [3,5,7,8,10],

        Country = [Ethiopia, Kazakhstan, Lithuania, Morocco, Yemen],
        Country :: Range,
        CountryS = ["Ethiopia", "Kazakhstan", "Lithuania", "Morocco", "Yemen"],

        alldifferent(Story),
        alldifferent(Age),
        alldifferent(Country),

        % constraints 
        Debby #= Lithuania,
        Age[Passover] #= Age[Bernice] + 2,
        Age[Yemen] #< Age[Ethiopia],
        Age[Morocco] #= Age[Ted] + 3,
        Age[Sammy] #= Age[MosessYouth] + 3,
        Carl #= Captivity,
        Age[TenCommandments] #= 5,
        ( 
            (Age[BurningBush] #= Age[Kazakhstan] + 2)
        or
            (Age[BurningBush] #= Age[Kazakhstan] + 3)
        ),

        % search
        term_variables([Story,Age,Country], Vars),
        labeling(Vars),

        % print solution
        writeln(story:Story),
        writeln(country:Country),
        writeln(age:Age),
        calc_size([ChildrenS,StoryS,CountryS], Size, 2),
        print_all(Children,ChildrenS,Size),
        print_all(Story,StoryS,Size),
        print_all(Country,CountryS,Size),
        % print ages
        concat_string(["%",Size, "d"],Format),
        ( foreacharg(A, Age), param(Format) do
              printf(Format, [A])
        ),
        nl.

print_all(X,S,Size) :-
        length(X,Len),
        concat_string(["%",Size, "s"],Format),
        (for(I,1,Len), param(X,S,Format) do            
             nth1(IX,X,I),
             nth1(IX,S,This),
             printf(Format, [This])
        ),nl.


%
% Size is 2 + the length of the largest string.
% Used for the presentation of the result.
% 
calc_size(A, Size, Add) :-
        flatten(A,List),
        ( foreach(String,List),
          foreach(Len, Lengths) do
              string_length(String,Len)
        ),
        Size is maxlist(Lengths) + Add.
