#=
   From BLOG examples/grades.blog
   """
   A standard toy example used to explain probabilistic relational 
   models (PRMs) and directed acyclic probabilistic entity-relationship 
   (DAPER) models.  This version follows Heckerman, Meek and 
   Koller (2004). 
   "Probabilistic models for relational data". Microsoft Research 
   TR 2004-30.
   """

   (The comments in quotes below are from the BLOG model.)

    Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

    gradeJohnCS106    1.9944    0.9891     0.0099    0.0660   175.5046    1.0000        3.8754
    gradeMaryCS106    1.7528    0.8933     0.0089    0.0596   164.4544    1.0007        3.6314
    gradeFredCS106    1.9821    0.8693     0.0087    0.0546   189.1307    1.0000        4.1763


    Distributions of variable gradeJohnCS106
    A          =>    3760  (0.376000)
    B          =>    3460  (0.346000)
    C          =>    2091  (0.209100)
    D          =>     454  (0.045400)
    F          =>     235  (0.023500)

    Distributions of variable gradeMaryCS106
    A          =>    4833  (0.483300)
    B          =>    3380  (0.338000)
    C          =>    1337  (0.133700)
    D          =>     326  (0.032600)
    F          =>     124  (0.012400)

    Distributions of variable gradeFredCS106
    B          =>    4059  (0.405900)
    A          =>    3314  (0.331400)
    C          =>    2166  (0.216600)
    D          =>     414  (0.041400)
    F          =>      47  (0.004700)

    mean(difficuly of courses): [1.2826 1.1714 1.7967]
    mean(intelligence): [2.1399 1.6545 2.2306]


   Cf ~blog/grades.blog
      ~/webppl/grades.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function grades()
    Smith = 1
    Jones = 2
    Moriarty = 3
    Professors = [Smith, Jones, Moriarty]
    num_professors = length(Professors)

    John = 1
    Mary = 2
    Fred = 3
    Students =  [John,Mary,Fred]
    num_students = length(Students)

    CS106 = 1
    Phil80 = 2
    Stat10 = 3
    Courses = [CS106, Phil80, Stat10];
    num_courses = length(Courses)

    A = 1
    B = 2
    C = 3
    D = 4
    F = 5
    grades = [A,B,C,D,F];

    Easy = 1
    Hard = 2

    Smart = 1
    Average = 2
    Weak = 3
    
    #=
     * """
     * Relational skeleton and evidence.
     *
     * To specify the interpretations of the non-random Boolean functions, 
     * we use the ListInterp class.  The first parameter to ListInterp is
     * the number of arguments to the function.  If the number of arguments
     * is k, then the remaining parameters are interpreted in groups of k,
     * as k-tuples for which the function returns true.  
     * 
     * Given this evidence, Mary has a high probability of getting an A in 
     * CS106, because she got an A in Phil80.  Since Fred got a C in Stat10, 
     * his expected grade in CS106 is lower than Mary's.  John got the same 
     * grade as Fred in Stat10, but has an advisor; since his advisor might 
     * be friends with one of the CS106 teachers, John has a higher 
     * probability of getting an A.  
     * """
    =#
    teaches = tzeros(num_professors,num_courses)
    for p in 1:num_professors, c in 1:num_courses
        teaches[p,c] ~ Dirac(
            (p == Smith    && c == CS106)  ||
            (p == Jones    && c == CS106)  ||
            (p == Moriarty && c == Phil80) ||
            (p == Jones    && c == Stat10)
        )
    end
    
    advises = tzeros(num_professors,num_students)
    for p in 1:num_professors, s in 1:num_students 
        advises[p,s] ~ Dirac(p == Moriarty && s == John)
    end
    
    takes = tzeros(num_students,num_courses)
    for s in 1:num_students, c in 1:num_courses 
        takes[s,c] ~ Dirac(
            (s == John && c == Stat10) ||
            (s == John && c == CS106)  ||
            (s == Mary && c == Phil80) ||
            (s == Mary && c == CS106)  ||
            (s == Fred && c == Stat10) ||
            (s == Fred && c == CS106)
        )
    end
    
    #=
     * """
     * In the DAPER paper this relation is called "Friend", but there is no 
     * attempt to ensure that it's symmetric, and ensuring that in BLOG is 
     * hard. Here we use "Likes", which doesn't imply symmetry.
     * """
    =#
    likes = tzeros(num_professors,num_professors)
    for p1 in 1:num_professors, p2 in 1:num_professors 
        likes[p1,p2] ~ p1 == p2 ? Dirac(true) : flip(0.2) 
     end

    difficulty = tzeros(num_courses)
    for c in 1:num_courses
        difficulty[c] ~ Categorical([0.7,0.3]) # [Easy, Hard]
    end

    intelligence = tzeros(num_students)
    for s in 1:num_students 
        intelligence[s] ~ Categorical([0.2,0.6,0.2]) # [Smart, Average, Weak]
    end

    #= 
     * """
     * If one of the teachers of course c likes an advisor of student s, 
     * then student s usually gets an A.  Otherwise the grade depends on 
     * the student's intelligence and the course's difficulty.  
     * """
    =#
    grade_obtained = tzeros(num_students, num_courses)
    for s in 1:num_students, c in 1:num_courses 
        takes_s_c = takes[s, c]
        ss = sum([teaches[p1,c] == true && advises[p2,s] == true && likes[p1,p2] == true 
                                    for p1 in Professors for p2 in Professors if p1 != p2  ]) > 0
        if takes_s_c == 1 && ss == true 
            grade_obtained[s,c] ~ Categorical([0.85, 0.1, 0.03, 0.01,0.01]) # grades
        else
            if takes_s_c == 1
                IntelligenceS = intelligence[s]
                DifficultyS = difficulty[c]
                grade_obtained[s,c] ~ (IntelligenceS==Weak    &&  DifficultyS==Easy) ? Categorical([0.2, 0.4, 0.3, 0.07, 0.03])   : 
                                      (IntelligenceS==Weak    &&  DifficultyS==Hard) ? Categorical([0.05, 0.1, 0.55, 0.2, 0.1])   : 
                                      (IntelligenceS==Average &&  DifficultyS==Easy) ? Categorical([0.3, 0.55, 0.10, 0.04, 0.01]) :
                                      (IntelligenceS==Average &&  DifficultyS==Hard) ? Categorical([0.15, 0.3, 0.45, 0.07, 0.03]) :
                                      (IntelligenceS==Smart   &&  DifficultyS==Easy) ? Categorical([0.85, 0.1, 0.03, 0.01, 0.01]) : 
                                      (IntelligenceS==Smart   &&  DifficultyS==Hard) ? Categorical([0.60, 0.25, 0.1, 0.03, 0.02]) : Dirac(F)
            else
                # Student don't take this course
                # grade_obtained[s,c] ~ Dirac(F) # Default F grade
            end
        end    
    end

    # Evidence
    true ~ Dirac(grade_obtained[John, Stat10] == C)
    true ~ Dirac(grade_obtained[Mary, Phil80] == A)
    true ~ Dirac(grade_obtained[Fred, Stat10] == C)
    
    gradeJohnCS106 ~ Dirac(grade_obtained[John, CS106])
    gradeMaryCS106 ~ Dirac(grade_obtained[Mary, CS106])
    gradeFredCS106 ~ Dirac(grade_obtained[Fred, CS106])

end

model = grades()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns[[:gradeJohnCS106,:gradeMaryCS106,:gradeFredCS106]])
# display(plot(chns))

show_var_dist_pct(chns, :gradeJohnCS106,["A","B","C","D","F"])
println()
show_var_dist_pct(chns, :gradeMaryCS106,["A","B","C","D","F"])
println()
show_var_dist_pct(chns, :gradeFredCS106,["A","B","C","D","F"])
println()
println("mean(difficuly of courses): $( mean(group(chns,:difficulty).value.data,dims=1))")
println("mean(intelligence): $( mean(group(chns,:intelligence).value.data,dims=1))")
