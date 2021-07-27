#=

  https://edu.swi-prolog.org/mod/assign/view.php?id=249
  """
  Medical diagnosis

  Develop an expert system for medical diagnosis.

  Consider three diseases: flu, gastroenteritis and bronchitis.

  A priori, flu has probability 0.3, gastroenteritis 0 0.2, and bronchitis 0.25.

  If you have the flu, you may have the following symptons, associated with their
  probabilities (symptoms are not mutually exclusive):

    fever, 0.8
    cough 0.6
    sore throat 0.5
    headaches 0.4
    aches 0.7

  If you have gastroenteritis, you may have the following symptons, associated with their
  probabilities (symptoms are not mutually exclusive):

    diarrhea 0.8
    aches 0.7
    nausea 0.4
    fatigue 0.3

  If you have bronchitis, you may have the following symptons, associated with their probabilities
  (symptoms are not mutually exclusive):

    cough 0.8
    fatigue 0.7
    fever 0.3

  Compute the probability of each disease given that the patient has the symptoms fever and aches.

  Do the same supposing the patient adds that he also experiences fatigue.
  """

  Cf ~/cplint/course_medical_diagnosis.pl
     ~/blog/medical_diagnosis.blog
     ~/psi/medical_diagnosis.psi
     ~/webppl/medical_diagnosis.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function medical_diagnosis()
    #=
      Consider three diseases: flu, gastroenteritis and bronchitis.

      A priori, flu has probability 0.3, gastroenteritis 0.2, and bronchitis 0.25.
    =#

    flu ~ flip(0.3)
    gastroenteritis ~ flip(0.2)
    bronchitis ~ flip(0.25)

    #=
      If you have the flu, you may have the following symptons, associated with their
      probabilities (symptoms are not mutually exclusive):

      fever, 0.8
      cough 0.6
      sore throat 0.5
      headaches 0.4
    aches 0.7
    =#

    fever ~ flu ? flip(0.8) : ( bronchitis ? flip(0.3) : flip(0.0))
    cough ~ flu ? flip(0.6) : ( bronchitis ? flip(0.8) : flip(0.0))
    sore_throat ~ flu ? flip(0.5) : flip(0.0)
    headaches ~ flu ? flip(0.4) : flip(0.0)
    aches ~ flu ? flip(0.7) : (gastroenteritis ? flip(0.7) : flip(0.0))

    #=
      If you have gastroenteritis, you may have the following symptons, associated with their
      probabilities (symptoms are not mutually exclusive):

      diarrhea 0.8
      aches 0.7
      nausea 0.4
      fatigue 0.3
    =#

    diarrhea ~ gastroenteritis ? flip(0.8) : flip(0.0)
    nausea  ~ gastroenteritis ? flip(0.4) : flip(0.0)
    fatigue ~ gastroenteritis ? flip(0.3) : ( bronchitis ? flip(0.7) : flip(0.0))

    #=
      If you have bronchitis, you may have the following symptons, associated with their probabilities
      (symptoms are not mutually exclusive):

      cough 0.8
      fatigue 0.7
      fever 0.3
    =#
    cough ~ bronchitis ? flip(0.8) : flip(0.0)
    fatigue ~ bronchitis ? flip(0.7) : flip(0.0)
    fever ~ bronchitis ? flip(0.3) : flip(0.0)

    # The observations
    true ~ Dirac(fever)
    true ~ Dirac(aches)

end

model = medical_diagnosis()
num_chns = 4

# HH has problem with this!
# chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, MH(), 40_000)

# chns = sample(model, PG(20), MCMCThreads(), 1000, num_chns)
chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)

# Note: IS don't generate chns the same way as MH, PG, and SMC!
# chns = sample(model, IS(), MCMCThreads(), 1000, num_chns)

display(chns)

show_var_dist_pct(chns,:flu)
show_var_dist_pct(chns,:gastroenteritis)
show_var_dist_pct(chns,:bronchitis)
