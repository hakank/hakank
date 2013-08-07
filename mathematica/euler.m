(*
   Some Project Euler solutions in Mathematica (v 9).

   Created by Hakan Kjellerstrand (hakank@gmail.com)

*)

(* 
   Run this from the command line: 
      $ math -script euler.m 
*)

Print["Euler problem 1"]

euler1[] := Print[Total@Select[Range[999],Mod[#,3]==0||Mod[#,5]==0&]]
euler1[]

euler1b[]:= Print@Total@Select[Range[999],#~Mod~3==0||#~Mod~5==0&]
euler1b[]

euler1c[]:= Print@Total@(Select[r = Range[999], #~Mod~3 == 0 &] 
                         ~ Union ~ 
                         Select[r, #~Mod~5 == 0 &])
euler1c[]

euler1d[] := GatherBy[Range[999],Mod[#,3]>0&&Mod[#,5]>0&][[2]]//Total//Print
euler1d[]


Print["Euler problem 2"]

euler2[] := Print[Total@ Select[Fibonacci[#] &@Range[100], # < 4000000 && Mod[#, 2] == 0 &]]
euler2[];


Print["Euler problem 3"]

euler3[] := Print[First@Last@FactorInteger[600851475143]]
euler3[];


Print["Euler problem 4"]

palindromicQ[n_Integer,base_:2] := (x = IntegerDigits[n]) == Reverse[x];
euler4[] := (
              Print[Select[Table[i*j, {i, 100, 999}, {j, i, 999}] // Flatten // 
              DeleteDuplicates, palindromicQ] // Max]
            )
euler4[];

(* Without the help function *)
euler4b[] := Print[Select[Table[i*j, {i, 100, 999}, {j, i, 999}] // Flatten // 
             DeleteDuplicates, (x = IntegerDigits[#]) == Reverse[x] &] // Max]
euler4b[]


Print["Euler problem 5"]

euler5[] := Print[Fold[LCM[#1,#2]&,Range[20]]]
euler5[]


Print["Euler problem 6"]

euler6[] := Print[(Total@(r=Range[100])^2)-Total@(#^2&/@ r)]
euler6[]


Print["Euler problem 7"]

euler7[] := Print[Prime[10001]]
euler7[]


Print["Euler problem 8"]

euler8a[] := (
           c = Characters[
   n = "731671765313306249192251196744265747423553491949349698352031277\
4506326239578318016984801869478851843858615607891129494954595017379583\
3195285320880551112540698747158523863050715693290963295227443043557668\
9664895044524452316173185640309871112172238311362229893423380308135336\
2766142828064444866452387493035890729629049156044077239071381051585930\
7960866701724271218839987979087922749219016997208880937766572733300105\
3367881220235421809751254540594752243525849077116705560136048395864467\
0632441572215539753697817977846174064955149290862569321978468622482839\
7224137565705605749026140797296865241453510047482166370484403199890008\
8952434506585412275886668811642717147992444292823086346567481391912316\
2824586178664583591245665294765456828489128831426076900422421902267105\
5626321111109370544217506941658960408071984038509624554443629812309878\
7992724428490918884580156166097919133875499200524063689912560717606058\
8611646710940507754100225698315520005593572972571636269561882670428252\
483600823257530420752963450"];
         a = {}; 
         For[i = 1, i <= Length[c] - 4, i++,
           AppendTo[a, FromDigits[#] & /@ c[[i;;i+4]]]
         ]
         Print[Map[Fold[Times, #] &, a] // Max]
         Clear[a,i,c];
       )

(* Neater using Partition *)
euler8[] := (
           c = Characters[
   n = "731671765313306249192251196744265747423553491949349698352031277\
4506326239578318016984801869478851843858615607891129494954595017379583\
3195285320880551112540698747158523863050715693290963295227443043557668\
9664895044524452316173185640309871112172238311362229893423380308135336\
2766142828064444866452387493035890729629049156044077239071381051585930\
7960866701724271218839987979087922749219016997208880937766572733300105\
3367881220235421809751254540594752243525849077116705560136048395864467\
0632441572215539753697817977846174064955149290862569321978468622482839\
7224137565705605749026140797296865241453510047482166370484403199890008\
8952434506585412275886668811642717147992444292823086346567481391912316\
2824586178664583591245665294765456828489128831426076900422421902267105\
5626321111109370544217506941658960408071984038509624554443629812309878\
7992724428490918884580156166097919133875499200524063689912560717606058\
8611646710940507754100225698315520005593572972571636269561882670428252\
483600823257530420752963450"];
       Map[Times @@#&, Partition[FromDigits[#] & /@ c,5,1]]//Max//Print;
            )

euler8[]


Print["Euler problem 9"]

isPythQ[a_, b_, c_] := a^2 + b^2 == c^2
euler9[] := (
   n = 1000; 
   c = b = 0;
   For[c = 1, c <= Ceiling[n - c/2], c++,   
      For[b = c, b >= Round[n - c/2] - c, b--,
          If[n - b - c > 0 && isPythQ[n - b - c, b, c], 
             Print[{n - b - c, b, c, (n - b - c)*b*c}]
          ]
          
      ]
  ]
  )
euler9[]


Print["Euler problem 10"]

euler10[] := Print[Total@Select[Range[2000000], PrimeQ[#]&]]
euler10[]


Print["Euler problem 11"]

triangleNumber[n_Integer] := Fold[Plus, Range[n]]
euler11[] := (
          i = 1;t = 0;
          While[Length[Divisors[t = triangleNumber[i]]] <= 500, i++];
          Print[t];
          Clear[t,i];
          )
euler11[]

(* Simpler version *)
euler11b[] := (
           t = 0;i = 1;
           While[Length[Divisors[t = t + i]] <= 500, i++]
           Print[t]
           Clear[t,i];
           )
euler11b[]


(* Slightly different version, putting i++ inside the body *)
euler11c[] := (
           t = 0;i = 0;
           While[Length[Divisors[t = t + i++]] <= 500]
           Print[t]
           Clear[t,i];
           )
euler11c[]


Print["Euler problem 12"]

euler12[] := (
      t = i = 0;
      While[Length[Divisors[t = t + i++]] <= 500]
      Print[t];
      Clear[i,t]
    )

euler12[]


Print["Euler problem 13"]

euler13[] := (
          Print[StringTake[
           IntegerString[
             Total@(FromDigits[#] & /@ 
                  StringSplit[
                        Import["euler_project_problem13_string.txt"]])], 10]]
           )
euler13[]


Print["Euler problem 14"]

(* 
   Memoization is done by the construct: Collatz[n_] :=  Collatz[n] = ....  
   Though it takes a lot of memory...
*)

collatz[n_] := collatz[n] = If[Mod[n, 2] == 0, n / 2, 3 n + 1];
(* collatzLength[n_] := collatzLength[n] = Length[NestWhileList[collatz[#] &, n, # != 1 &]]; *)
collatzLength[n_] := Length[NestWhileList[collatz[#] &, n, # != 1 &]];

(* alternative versions, using FixedPointList for the length. *)
collatz2[1] := 1 (* base case, the stopping criteria *)
collatz2[n_] := collatz2[n] = If[Mod[n, 2] == 0, n / 2, 3 n + 1];
collatzLength2[n_] := Length[FixedPointList[collatz2 , n]] - 1
collatzLength3[n_] := Length[Most@NestWhileList[collatz, n, Unequal, All]]
collatzList[n_] := Most@NestWhileList[If[Mod[#, 2] == 0, #/2, 3 # + 1] &, n, Unequal, All]

euler14Slow[] := (
          Print["This will take some time ..."]
          Clear[maxlen, maxn, len, i];
          maxlen = 0;maxn = 0; len = 0;
          For[i = 2, i < 1000000, i++, 
               len = collatzLength[i];
               If[len > maxlen, maxlen = len; maxn = i]
          ]
          Print[{maxlen, maxn}]
          Clear[maxlen, maxn, len]
        )

(* this is faster faster, but still slow... *)
collatzLength[1] = 1;
collatzLength[2] = 2;
collatzLength[n_] := 
          collatzLength[n] = collatzLength[n] = If[Mod[n, 2] == 0, 
                                                     collatzLength[n/2] + 1, 
                                                     collatzLength[3 n + 1]];
euler14[] := (
           Sort[{#, collatzLength[#]} & /@ 
                      Range[1, 10^6], #1[[2]] > #2[[2]] &][[1]] // Print
          )
euler14[]

(* To remove the cached values: *)
(* Clear[collatzLength] *)


Print["Euler problem 15"]

euler15[] := Print[(Times@@Range[21, 40])/(Times@@Range[2, 20])]
euler15[]


Print["Euler problem 16"]

euler16[] := Print[Total@IntegerDigits[2^1000]]
euler16[]


Print["Euler problem 17"]

(* This is borrowed from 
http://mathematica.stackexchange.com/questions/1065/how-to-express-an-integer-number-in-english-words *)

inWords[n_] := 
  Module[{r, 
    numNames = {"", " one", " two", " three", " four", " five", 
      " six", " seven", " eight", " nine"}, 
    teenNames = {" ten", " eleven", " twelve", " thirteen", 
      " fourteen", " fifteen", " sixteen", " seventeen", " eighteen", 
      " nineteen"}, 
    tensNames = {"", " ten", " twenty", " thirty", " forty", " fifty",
       " sixty", " seventy", " eighty", " ninety"}, 
    decimals = {"", " thousand", " million", " billion", " trillion", 
      " quadrillion", " quintillion", " sextillion", " septillion", 
      " octillion", " nonillion", " decillion", " undecillion", 
      " duodecillion", " tredecillion", " quattuordecillion", 
      " quindecillion", " sexdecillion", " septendecillion", 
      " octodecillion", " Novemdecillion", " Vigintillion", 
      " 66illion", " 69illion"}}, 
   r = If[# != 0, 
        numNames[[# + 1]] <> " hundred" <> 
         If[#2 != 0 || #3 != 0, " and", ""], ""] <> 
       Switch[#2, 0, numNames[[#3 + 1]], 1, teenNames[[#3 + 1]], _, 
        tensNames[[#2 + 1]] <> numNames[[#3 + 1]]] & @@@ (PadLeft[
         FromDigits /@ Characters@StringReverse@#, 3] & /@ 
       StringCases[StringReverse@IntegerString@n, 
        RegularExpression["\\d{1,3}"]]);
   StringJoin@
    Reverse@MapThread[
      If[# != "", StringJoin[##], ""] &, {r, 
       Take[decimals, Length@r]}]];

euler17[] := StringJoin[Map[inWords[#] &, Range[1000]], ""] // 
             StringReplace[#, " " -> ""] & // StringSplit[#, ""] & // Length//Print
euler17[]


Print["Euler problem 18"]

(* Using dynamic programming *)
euler18[] := (
       t = FromDigits[#] & /@ StringSplit[#] & /@ 
                 StringSplit[
                 "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 \
                 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 \
                 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 \
                 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 \
                 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 \
                 27 23 09 70 98 73 93 38 53 60 04 23", "\n"];
           For[i = Length[t] - 1, i >= 0, i--,
              For[j = 1, j <= i, j++,
                 t[[i, j]] = t[[i, j]] + Max[t[[i + 1, j]], t[[i + 1, j + 1]]]
             ]
           ];
       Print[t[[1, 1]]]
      )
euler18[]


Print["Euler problem 19"]

euler19[] := (
    Select[DateRange[DateList[{1901, 1, 1}], 
      DateList[{2000, 12, 31}], {1, "Month"}], 
      DateString[#, "DayName"] == "Sunday" &] // Length // Print
    )
euler19[]


Print["Euler problem 20"]

euler20[] := Print[Total@IntegerDigits[100!]]
euler20[]


Print["Euler problem 22"]

euler22[] := (
          names=Sort@StringSplit[
             StringReplace[Import["http://projecteuler.net/project/names.txt"], 
             "\"" -> ""], ","];
          Print[Total@MapIndexed[Last[#2]*Total[(ToCharacterCode[#1] - 64)] &, names]]
          Clear[names]
          )
euler22[]


Print["Euler problem 24"]

euler24[] := Print[FromDigits[Permutations[Range[0,9]][[10^6]]]]
euler24[]


Print["Euler problem 25"]

euler25[] := (
          i = 1; f = 1;
          While[Length[IntegerDigits[f]]<1000,f=Fibonacci[i++]]
          Print[i - 1]
          )
euler25[]


Print["Euler problem 26"]

euler26[] := (
          m = Max[l = Map[Length[Flatten@First@RealDigits[1/#]] &, Range[1000]]];
          Print[l[[Position[l, m] // Flatten]]]
          )
euler26[]


Print["Euler problem 28"]

euler28[] := Print[1+Total@Map[4 #^2 - 6 # + 6 &, Range[3, 1001, 2]]]
euler28[]


Print["Euler problem 29"]

euler29[] := Print@Length@Union@Flatten@Table[a^b, {a, 2, 100}, {b, 2, 100}]
euler29[]


Print["Euler problem 30"]

euler30[] := Total@Select[Range[10, 6 9^5], Total[Map[#1^5 &, IntegerDigits[#]]] ==  # &] // Print
euler30[]


Print["Euler problem 31"]

coins[_, _, 8] := 1;
coins[pcoins_, money_, m_] :=
 Module[{ssum, i},  
      ssum = 0;    
      For[i = m, i <= 8, i++,
        t = money - pcoins[[i]];
        ssum += 
    If[t == 0, 1, 0] + If[t > 0, coins[pcoins, t, i], 0] ;
      ]; 
       ssum
  ];

euler31[] := Print[coins[{200, 100, 50, 20, 10, 5, 2, 1}, 200, 1]]
euler31[];

(* Solve the Diophantine equation *)
euler31b[] := FrobeniusSolve[{200,100,50,20,10,5,2,1},200]//Length//Print
euler31b[]

Print["Euler problem 34"]

euler34[] := Total@Select[Range[10, 100000], Total@Map[#! &, IntegerDigits[#]] == # &]//Print
euler34[]


Print["Euler problem 36"]

euler36[] := Print@Total@Select[Range[999999], palindromicQ[#] && palindromicQ[#, 2] &]
euler36[]


Print["Euler problem 48"]

euler48[] := IntegerDigits[Total@Map[#^# &, Range[1000]]][[-10 ;; -1]]//Print;

