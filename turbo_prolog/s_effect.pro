
/**********************************************************************
*                BORING's spelprogram                                 *
*                                                                     *
*    (c) H†kan Kjellerstrand                                          *
***********************************************************************
                 
******* + "tone" fr†n "Advanced programming techniques" sid 301 *******/



PREDICATES
   play_down(integer, integer, integer)
   play_up(integer, integer, integer)
   play_duration(integer, integer, real)
   play_random(integer)
   play_random_2(integer)
   rand_dur(integer, integer)
   rand_freq(integer, integer)
   main
   spela
/*   tone(integer, integer, integer, integer)*/
      
CLAUSES

/**********************************************************/

play_down(X,Y,_) if X<=Y, !.
play_down(Freq_Begin, Freq_Bottom, Duration) if       % Att spela ner en skala
  sound(Duration, Freq_Begin),                        % Freq_Begin  = startfrekvens
  Freq_Begin > Freq_Bottom,                           % Freq_Bottom = slutfrekvens
  New_Freq=Freq_Begin-40,                             % Duration    = l„ngd
  play_down(New_Freq, Freq_Bottom, Duration).   

play_up(X,Y,_) if X>Y, !.
play_up(Freq_Begin, Freq_Upper, Duration) if          % Att spela upp en skala
  sound(Duration, Freq_Begin),                        % Freq_Upper  = slutfrekvens 
  Freq_Begin < Freq_Upper, 
  New_Freq=Freq_Begin+40, 
  play_up(New_Freq, Freq_Upper, Duration).   

/***************************************************/

play_duration(Freq_Begin, Freq_Bottom, Duration) if   % En liten test p† h”rseln
  sound(Duration, Freq_Begin),
  Freq_Begin > Freq_Bottom, write(Freq_Begin, "\n"),
  New_Freq=Freq_Begin-90, write(Freq_Begin, "\n"),
  play_duration(New_Freq, Freq_Bottom, Duration).   

/****************************************************/

play_random(1) if  !.
play_random(Time) if                                    % Spela slumpmusik
   random(50, Duration), random(500, Frequency),        % random(X, Duration)
   sound(Duration, Frequency),                          % random(Y, Frequency)       
   Time > 0, New_Time = Time-1, play_random(New_Time).  % d„r X = maxduration (l„ngd)
                                                        % och Y = maxfrequency
/*************************************************/

play_random_2(0) if !.                                  % Dito med gr„nser  
play_random_2(Time) if
   rand_dur(X, Duration), rand_freq(C, Frequency),
   write(Time, ": "),
   write(Duration , "   ",Frequency, "\n"), 
   sound(Duration, Frequency),
   Time > 0, New_Time = Time -1,
   play_random_2(New_Time). 

rand_dur(15, Dur) if              % gr„nsen f”r duration
    random(15, Dur),              % „ndra siffran p† alla st„llena
    Dur > 5, !.                   % p† denna rad ligger under gr„nsen!!
    
rand_dur(15, Dur) if rand_dur(15, Dur). % om f”rsta clausen misslyckas -> 
                                        % om igen!!!  
 
rand_freq(1000, Freq) if
    random(1000 , Freq),
    Freq > 200, !.
rand_freq(1000, Freq) if rand_freq(1000, Freq).
    
/*****************************************************/

spela if
  write("Det kommer f”rst lite slumpmusik.\n"),
  play_random(40),
  system("CLS"),
  write("\n\nNu blir det h”rselprov:\nVad kan ni h”ra?"),
  play_duration(18000, 50, 9.6).

spela if
  system("CLS"), 
  write("\nNu kommer det n†got annat.\n\n\n"),
  play_down(300, 200, 17),
  play_up(200, 450, 6),
  play_down(500, 250,7),
  play_down(750, 620,5),
  play_down(390, 350,4),
  play_down(500, 250,7),
  play_down(300, 200,3),
  play_up(200, 300,8),
  play_down(600, 500,5),
  play_up(600, 700,6),
  play_down(300, 250,5),
  play_down(500, 450,5),
  play_down(450, 350, 9),
  play_down(350, 200, 7),
  play_down(300, 150, 5), fail. 

spela if
  play_random_2(30), 
  play_random(20),
  play_up(200, 700, 5),
  play_down(700, 200, 6),
  play_up(200, 700, 7),
  play_down(700, 200, 8),
  play_up(200, 700, 9),
  play_down(700, 200, 10),
  play_up(200, 700, 5),
  play_down(700, 200, 5),
  play_up(200, 500, 70),
  fail.  



/*
tone(0, _,_,_).
tone(Repeats, Duration, RestDur, Frequency) if
    R = Repeats -1,
    D = Duration + 1,
    F = Frequency +1,
    sound(Duration, Frequency),
    sound(RestDur, 32000),
    tone(R, D, RestDur, F). */

main if
    write("Detta „r exempel p† vad man kan g”ra med musik.\n"),
    write("Lyssna er tillbaka och luta!\n"),
    spela.

main if
    system("CLS"),
    write("\nThat's all folks!\n").

    
goal 
    main.







