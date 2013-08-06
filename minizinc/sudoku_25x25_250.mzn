% 
% Sudoku 25 x 25 in MiniZinc.
% 
% Prolog benchmark problem (BProlog)
% """
%   File   : sudoku81.pl (in B-Prolog)
%   Author : Neng-Fa ZHOU
%   Date   : 1996
%   Purpose: solve a Japanese arithmetic puzzle (25*25)
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 1..25;

var r: A11; var r:A12; var r:A13; var r:A14; var r:A15; var r:B11; var r:B12; var r:B13; var r:B14; var r:B15; var r:C11; var r:C12; var r:C13; var r:C14; var r:C15; var r: D11; var r:D12; var r:D13; var r:D14; var r:D15; var r:E11; var r:E12; var r:E13; var r:E14; var r:E15; 
var r:A21; var r:A22; var r:A23; var r:A24; var r:A25; var r:B21; var r:B22; var r:B23; var r:B24; var r:B25; var r:C21; var r:C22; var r:C23; var r:C24; var r:C25; var r:D21; var r:D22; var r:D23; var r:D24; var r:D25; var r:E21; var r:E22; var r:E23; var r:E24; var r:E25; 
var r:A31; var r:A32; var r:A33; var r:A34; var r:A35; var r:B31; var r:B32; var r:B33; var r:B34; var r:B35; var r:C31; var r:C32; var r:C33; var r:C34; var r:C35; var r:D31; var r:D32; var r:D33; var r:D34; var r:D35; var r:E31; var r:E32; var r:E33; var r:E34; var r:E35; 
var r:A41; var r:A42; var r:A43; var r:A44; var r:A45; var r:B41; var r:B42; var r:B43; var r:B44; var r:B45; var r:C41; var r:C42; var r:C43; var r:C44; var r:C45; var r:D41; var r:D42; var r:D43; var r:D44; var r:D45; var r:E41; var r:E42; var r:E43; var r:E44; var r:E45; 
var r:A51; var r:A52; var r:A53; var r:A54; var r:A55; var r:B51; var r:B52; var r:B53; var r:B54; var r:B55; var r:C51; var r:C52; var r:C53; var r:C54; var r:C55; var r:D51; var r:D52; var r:D53; var r:D54; var r:D55; var r:E51; var r:E52; var r:E53; var r:E54; var r:E55; 
var r:F11; var r:F12; var r:F13; var r:F14; var r:F15; var r:G11; var r:G12; var r:G13; var r:G14; var r:G15; var r:H11; var r:H12; var r:H13; var r:H14; var r:H15; var r:I11; var r:I12; var r:I13; var r:I14; var r:I15; var r:J11; var r:J12; var r:J13; var r:J14; var r:J15; 
var r:F21; var r:F22; var r:F23; var r:F24; var r:F25; var r:G21; var r:G22; var r:G23; var r:G24; var r:G25; var r:H21; var r:H22; var r:H23; var r:H24; var r:H25; var r:I21; var r:I22; var r:I23; var r:I24; var r:I25; var r:J21; var r:J22; var r:J23; var r:J24; var r:J25; 
var r:F31; var r:F32; var r:F33; var r:F34; var r:F35; var r:G31; var r:G32; var r:G33; var r:G34; var r:G35; var r:H31; var r:H32; var r:H33; var r:H34; var r:H35; var r:I31; var r:I32; var r:I33; var r:I34; var r:I35; var r:J31; var r:J32; var r:J33; var r:J34; var r:J35; 
var r:F41; var r:F42; var r:F43; var r:F44; var r:F45; var r:G41; var r:G42; var r:G43; var r:G44; var r:G45; var r:H41; var r:H42; var r:H43; var r:H44; var r:H45; var r:I41; var r:I42; var r:I43; var r:I44; var r:I45; var r:J41; var r:J42; var r:J43; var r:J44; var r:J45; 
var r:F51; var r:F52; var r:F53; var r:F54; var r:F55; var r:G51; var r:G52; var r:G53; var r:G54; var r:G55; var r:H51; var r:H52; var r:H53; var r:H54; var r:H55; var r:I51; var r:I52; var r:I53; var r:I54; var r:I55; var r:J51; var r:J52; var r:J53; var r:J54; var r:J55; 
var r:K11; var r:K12; var r:K13; var r:K14; var r:K15; var r:L11; var r:L12; var r:L13; var r:L14; var r:L15; var r:M11; var r:M12; var r:M13; var r:M14; var r:M15; var r:N11; var r:N12; var r:N13; var r:N14; var r:N15; var r:O11; var r:O12; var r:O13; var r:O14; var r:O15; 
var r:K21; var r:K22; var r:K23; var r:K24; var r:K25; var r:L21; var r:L22; var r:L23; var r:L24; var r:L25; var r:M21; var r:M22; var r:M23; var r:M24; var r:M25; var r:N21; var r:N22; var r:N23; var r:N24; var r:N25; var r:O21; var r:O22; var r:O23; var r:O24; var r:O25; 
var r:K31; var r:K32; var r:K33; var r:K34; var r:K35; var r:L31; var r:L32; var r:L33; var r:L34; var r:L35; var r:M31; var r:M32; var r:M33; var r:M34; var r:M35; var r:N31; var r:N32; var r:N33; var r:N34; var r:N35; var r:O31; var r:O32; var r:O33; var r:O34; var r:O35; 
var r:K41; var r:K42; var r:K43; var r:K44; var r:K45; var r:L41; var r:L42; var r:L43; var r:L44; var r:L45; var r:M41; var r:M42; var r:M43; var r:M44; var r:M45; var r:N41; var r:N42; var r:N43; var r:N44; var r:N45; var r:O41; var r:O42; var r:O43; var r:O44; var r:O45; 
var r:K51; var r:K52; var r:K53; var r:K54; var r:K55; var r:L51; var r:L52; var r:L53; var r:L54; var r:L55; var r:M51; var r:M52; var r:M53; var r:M54; var r:M55; var r:N51; var r:N52; var r:N53; var r:N54; var r:N55; var r:O51; var r:O52; var r:O53; var r:O54; var r:O55; 
var r:P11; var r:P12; var r:P13; var r:P14; var r:P15; var r:Q11; var r:Q12; var r:Q13; var r:Q14; var r:Q15; var r:R11; var r:R12; var r:R13; var r:R14; var r:R15; var r:S11; var r:S12; var r:S13; var r:S14; var r:S15; var r:T11; var r:T12; var r:T13; var r:T14; var r:T15; 
var r:P21; var r:P22; var r:P23; var r:P24; var r:P25; var r:Q21; var r:Q22; var r:Q23; var r:Q24; var r:Q25; var r:R21; var r:R22; var r:R23; var r:R24; var r:R25; var r:S21; var r:S22; var r:S23; var r:S24; var r:S25; var r:T21; var r:T22; var r:T23; var r:T24; var r:T25; 
var r:P31; var r:P32; var r:P33; var r:P34; var r:P35; var r:Q31; var r:Q32; var r:Q33; var r:Q34; var r:Q35; var r:R31; var r:R32; var r:R33; var r:R34; var r:R35; var r:S31; var r:S32; var r:S33; var r:S34; var r:S35; var r:T31; var r:T32; var r:T33; var r:T34; var r:T35; 
var r:P41; var r:P42; var r:P43; var r:P44; var r:P45; var r:Q41; var r:Q42; var r:Q43; var r:Q44; var r:Q45; var r:R41; var r:R42; var r:R43; var r:R44; var r:R45; var r:S41; var r:S42; var r:S43; var r:S44; var r:S45; var r:T41; var r:T42; var r:T43; var r:T44; var r:T45; 
var r:P51; var r:P52; var r:P53; var r:P54; var r:P55; var r:Q51; var r:Q52; var r:Q53; var r:Q54; var r:Q55; var r:R51; var r:R52; var r:R53; var r:R54; var r:R55; var r:S51; var r:S52; var r:S53; var r:S54; var r:S55; var r:T51; var r:T52; var r:T53; var r:T54; var r:T55; 
var r:U11; var r:U12; var r:U13; var r:U14; var r:U15; var r:V11; var r:V12; var r:V13; var r:V14; var r:V15; var r:W11; var r:W12; var r:W13; var r:W14; var r:W15; var r:X11; var r:X12; var r:X13; var r:X14; var r:X15; var r:Y11; var r:Y12; var r:Y13; var r:Y14; var r:Y15; 
var r:U21; var r:U22; var r:U23; var r:U24; var r:U25; var r:V21; var r:V22; var r:V23; var r:V24; var r:V25; var r:W21; var r:W22; var r:W23; var r:W24; var r:W25; var r:X21; var r:X22; var r:X23; var r:X24; var r:X25; var r:Y21; var r:Y22; var r:Y23; var r:Y24; var r:Y25; var r:
U31; var r:U32; var r:U33; var r:U34; var r:U35; var r:V31; var r:V32; var r:V33; var r:V34; var r:V35; var r:W31; var r:W32; var r:W33; var r:W34; var r:W35; var r:X31; var r:X32; var r:X33; var r:X34; var r:X35; var r:Y31; var r:Y32; var r:Y33; var r:Y34; var r:Y35; 
var r:U41; var r:U42; var r:U43; var r:U44; var r:U45; var r:V41; var r:V42; var r:V43; var r:V44; var r:V45; var r:W41; var r:W42; var r:W43; var r:W44; var r:W45; var r:X41; var r:X42; var r:X43; var r:X44; var r:X45; var r:Y41; var r:Y42; var r:Y43; var r:Y44; var r:Y45; 
var r: U51; var r:U52; var r:U53; var r:U54; var r:U55; var r:V51; var r:V52; var r:V53; var r:V54; var r:V55; var r:W51; var r:W52; var r:W53; var r:W54; var r:W55; var r:X51; var r:X52; var r:X53; var r:X54; var r:X55; var r:Y51; var r:Y52; var r:Y53; var r:Y54; var r:Y55;



array[1..625] of var r:   
     Vars=[A11,A12,A13,A14,A15,B11,B12,B13,B14,B15,C11,C12,C13,C14,C15,
       	  D11,D12,D13,D14,D15,E11,E12,E13,E14,E15,
          A21,A22,A23,A24,A25,B21,B22,B23,B24,B25,C21,C22,C23,C24,C25,
       	  D21,D22,D23,D24,D25,E21,E22,E23,E24,E25,
          A31,A32,A33,A34,A35,B31,B32,B33,B34,B35,C31,C32,C33,C34,C35,
       	  D31,D32,D33,D34,D35,E31,E32,E33,E34,E35,
          A41,A42,A43,A44,A45,B41,B42,B43,B44,B45,C41,C42,C43,C44,C45,
       	  D41,D42,D43,D44,D45,E41,E42,E43,E44,E45,
          A51,A52,A53,A54,A55,B51,B52,B53,B54,B55,C51,C52,C53,C54,C55,
       	  D51,D52,D53,D54,D55,E51,E52,E53,E54,E55,
          F11,F12,F13,F14,F15,G11,G12,G13,G14,G15,H11,H12,H13,H14,H15,
          I11,I12,I13,I14,I15,J11,J12,J13,J14,J15,
          F21,F22,F23,F24,F25,G21,G22,G23,G24,G25,H21,H22,H23,H24,H25,
          I21,I22,I23,I24,I25,J21,J22,J23,J24,J25,
          F31,F32,F33,F34,F35,G31,G32,G33,G34,G35,H31,H32,H33,H34,H35,
          I31,I32,I33,I34,I35,J31,J32,J33,J34,J35,
          F41,F42,F43,F44,F45,G41,G42,G43,G44,G45,H41,H42,H43,H44,H45,
          I41,I42,I43,I44,I45,J41,J42,J43,J44,J45,
          F51,F52,F53,F54,F55,G51,G52,G53,G54,G55,H51,H52,H53,H54,H55,
          I51,I52,I53,I54,I55,J51,J52,J53,J54,J55,
          K11,K12,K13,K14,K15,L11,L12,L13,L14,L15,M11,M12,M13,M14,M15,
          N11,N12,N13,N14,N15,O11,O12,O13,O14,O15,
          K21,K22,K23,K24,K25,L21,L22,L23,L24,L25,M21,M22,M23,M24,M25,
          N21,N22,N23,N24,N25,O21,O22,O23,O24,O25,
          K31,K32,K33,K34,K35,L31,L32,L33,L34,L35,M31,M32,M33,M34,M35,
          N31,N32,N33,N34,N35,O31,O32,O33,O34,O35,
          K41,K42,K43,K44,K45,L41,L42,L43,L44,L45,M41,M42,M43,M44,M45,
          N41,N42,N43,N44,N45,O41,O42,O43,O44,O45,
          K51,K52,K53,K54,K55,L51,L52,L53,L54,L55,M51,M52,M53,M54,M55,
          N51,N52,N53,N54,N55,O51,O52,O53,O54,O55,
          P11,P12,P13,P14,P15,Q11,Q12,Q13,Q14,Q15,R11,R12,R13,R14,R15,
          S11,S12,S13,S14,S15,T11,T12,T13,T14,T15,
          P21,P22,P23,P24,P25,Q21,Q22,Q23,Q24,Q25,R21,R22,R23,R24,R25,
          S21,S22,S23,S24,S25,T21,T22,T23,T24,T25,
          P31,P32,P33,P34,P35,Q31,Q32,Q33,Q34,Q35,R31,R32,R33,R34,R35,
          S31,S32,S33,S34,S35,T31,T32,T33,T34,T35,
          P41,P42,P43,P44,P45,Q41,Q42,Q43,Q44,Q45,R41,R42,R43,R44,R45,
          S41,S42,S43,S44,S45,T41,T42,T43,T44,T45,
          P51,P52,P53,P54,P55,Q51,Q52,Q53,Q54,Q55,R51,R52,R53,R54,R55,
          S51,S52,S53,S54,S55,T51,T52,T53,T54,T55,
          U11,U12,U13,U14,U15,V11,V12,V13,V14,V15,W11,W12,W13,W14,W15,
          X11,X12,X13,X14,X15,Y11,Y12,Y13,Y14,Y15,
          U21,U22,U23,U24,U25,V21,V22,V23,V24,V25,W21,W22,W23,W24,W25,
          X21,X22,X23,X24,X25,Y21,Y22,Y23,Y24,Y25,
          U31,U32,U33,U34,U35,V31,V32,V33,V34,V35,W31,W32,W33,W34,W35,
          X31,X32,X33,X34,X35,Y31,Y32,Y33,Y34,Y35,
          U41,U42,U43,U44,U45,V41,V42,V43,V44,V45,W41,W42,W43,W44,W45,
          X41,X42,X43,X44,X45,Y41,Y42,Y43,Y44,Y45,
          U51,U52,U53,U54,U55,V51,V52,V53,V54,V55,W51,W52,W53,W54,W55,
          X51,X52,X53,X54,X55,Y51,Y52,Y53,Y54,Y55];


% solve satisfy;
solve :: int_search(Vars, first_fail, indomain_min, complete) satisfy;

constraint
    A11=8 /\ A22=18 /\ A23=24 /\ A24=22 /\ A32=21 /\ A33=23 /\ A34=13 /\ 
    A42=15 /\ A43=1 /\ A44=20 /\ A55=17 /\
    B11=21 /\ B12=15 /\ B14=6 /\ B21=19 /\ B23=4 /\ B32=10 /\ B35=1 /\ 
    B41=18 /\ B45=13 /\ B53=24 /\ B54=11 /\ B55=12 /\ 
    C11=19 /\ C13=1 /\ C15=11 /\ C21=7 /\ C23=2 /\ C25=8 /\ C32=14 /\ 
    C33=3 /\ C34=22 /\ C42=24 /\ C43=4 /\ C44=16 /\ C53=5 /\
    D12=4 /\ D14=14 /\ D15=3 /\ D23=1 /\ D25=12 /\ D31=5 /\ D34=11 /\ 
    D41=19 /\ D45=2 /\ D51=15 /\ D52=13 /\ D53=18 /\
    E15=22 /\ E22=21 /\ E23=14 /\ E24=9 /\ E32=20 /\ E33=12 /\ E34=25 /\ 
    E42=7 /\ E43=23 /\ E44=17 /\ E51=16 /\
    F11=13 /\ F12=17 /\ F14=3 /\ F21=2 /\ F23=5 /\ F32=6 /\ F35=16 /\ 
    F41=21 /\ F44=4 /\ F45=10 /\ F53=18 /\ F54=1 /\ F55=20 /\
    G12=19 /\ G13=20 /\ G14=5 /\ G21=25 /\ G22=6 /\ G31=13 /\ G34=3 /\ 
    G35=9 /\ G41=8 /\ G43=15 /\ G53=7 /\
    H13=6 /\ H22=19 /\ H23=7 /\ H24=21 /\ H32=1 /\ H33=8 /\ H34=4 /\ 
    H41=12 /\ H43=9 /\ H45=23 /\ H51=3 /\ H53=10 /\ H55=25 /\
    I12=23 /\ I13=16 /\ I14=24 /\ I24=3 /\ I25=11 /\ I31=2 /\ I32=10 /\ 
    I35=7 /\ I43=6 /\ I45=14 /\ I53=15 /\
    J12=25 /\ J14=12 /\ J15=14 /\ J23=17 /\ J25=20 /\ J31=24 /\ J34=15 /\ 
    J41=11 /\ J42=3 /\ J45=7 /\ J51=4 /\ J52=13 /\ J53=6 /\
    K11=14 /\ K12=4 /\ K23=13 /\ K24=8 /\ K31=11 /\ K33=6 /\ K34=12 /\ 
    K35=7 /\ K43=3 /\ K44=9 /\ K51=22 /\ K52=25 /\ 
    L14=8 /\ L15=20 /\ L22=22 /\ L23=14 /\ L31=3 /\ L35=17 /\ L42=12 /\ 
    L43=6 /\ L54=7 /\ L55=4 /\
    M12=3 /\ M13=11 /\ M14=19 /\ M21=24 /\ M22=20 /\ M24=18 /\ M25=17 /\ 
    M31=23 /\ M33=13 /\ M35=4 /\ M41=5 /\ M42=22 /\ M44=10 /\ M45=21 /\ 
    M52=16 /\ M53=15 /\ M54=9 /\
    N11=1 /\ N12=12 /\ N23=11 /\ N24=7 /\ N31=8 /\ N35=15 /\ N43=13 /\ 
    N44=18 /\ N51=17 /\ N52=3 /\
    O14=18 /\ O15=23 /\ O22=19 /\ O23=9 /\ O31=20 /\ O32=14 /\ O33=16 /\ 
    O35=21 /\ O42=8 /\ O43=15 /\ O54=5 /\ O55=13 /\
    P13=21 /\ P14=17 /\ P15=6 /\ P21=23 /\ P24=24 /\ P25=15 /\ P32=16 /\ 
    P35=25 /\ P41=1 /\ P43=9 /\ P51=12 /\ P52=2 /\ P54=11 /\
    Q13=8 /\ Q21=7 /\ Q23=11 /\ Q31=10 /\ Q34=23 /\ Q35=24 /\ Q41=6 /\ 
    Q42=14 /\ Q52=25 /\ Q53=21 /\ Q54=4 /\
    R11=15 /\ R13=16 /\ R15=5 /\ R21=2 /\ R23=17 /\ R25=14 /\ R32=13 /\ 
    R33=18 /\ R34=12 /\ R42=10 /\ R43=19 /\ R44=7 /\ R53=20 /\
    S13=2 /\ S23=10 /\ S25=21 /\ S31=3 /\ S32=20 /\ S35=9 /\ S44=12 /\ 
    S45=8 /\ S52=14 /\ S53=22 /\ S54=1 /\
    T11=13 /\ T12=1 /\ T13=20 /\ T21=25 /\ T22=9 /\ T25=16 /\ T31=14 /\ 
    T34=11 /\ T43=2 /\ T45=15 /\ T52=18 /\ T54=23 /\ T55=5 /\
    U15=5 /\ U22=12 /\ U23=14 /\ U24=6 /\ U32=1 /\ U33=11 /\ U34=7 /\ 
    U42=10 /\ U43=22 /\ U44=19 /\ U51=15 /\
    V13=13 /\ V14=19 /\ V15=14 /\ V21=15 /\ V25=8 /\ V32=4 /\ V35=22 /\ 
    V41=5 /\ V43=18 /\ V51=23 /\ V52=7 /\ V54=17 /\
    W13=21 /\ W22=17 /\ W23=22 /\ W24=20 /\ W32=6 /\ W33=23 /\ W34=5 /\ 
    W41=16 /\ W43=24 /\ W45=15 /\ W51=11 /\ W53=25 /\ W55=18 /\
    X11=23 /\ X12=6 /\ X13=12 /\ X21=16 /\ X25=24 /\ X31=18 /\ X34=17 /\ 
    X43=21 /\ X45=25 /\ X52=19 /\ X54=13 /\ X55=1 /\
    Y11=1 /\ Y22=5 /\ Y23=13 /\ Y24=10 /\ Y32=12 /\ Y33=21 /\ Y34=2 /\ 
    Y42=11 /\ Y43=7 /\ Y44=14 /\ Y55=6 /\


    E14=13 /\ 
    D21=10 /\
    E21=6 /\ E25=3 /\
    C45=12 /\ D42=21 /\
    C52=21 /\ 
    E54=19 /\ E55=1 /\
    H12=11 /\ H14=15 /\
    J11=10 /\ J13=1 /\ 
    H21=13 /\ H25=16 /\
    I33=25 /\ 
    H44=24 /\ I41=13 /\ 
    I55=17 /\ J55=2 /\
    M11=6 /\ M15=7 /\ 
    O11=2 /\ O12=17 /\
    M23=12 /\ N21=21 /\ 
    O24=6 /\ O25=10 /\
    N33=19 /\ 
    M43=14 /\ N41=20 /\ 
    M51=8 /\ M55=1 /\ 
    O52=24 /\ O53=11 /\ 
    R12=23 /\ R14=11 /\
    T14=7 /\ 
    R24=1 /\ S21=6 /\ 
    S33=17 /\ S34=15 /\
    T33=8 /\ T35=19 /\
    R41=25 /\ R45=3 /\
    T42=4 /\ T44=24 /\
    R54=6 /\ R55=24 /\ 
    T51=17 /\ T53=10 /\ 
    W11=4 /\ W12=7 /\ 
    Y14=3 /\ Y15=9 /\
    W25=19 /\ X22=11 /\ 
    X32=15 /\ 
    X35=10 /\ Y31=19 /\ Y35=8 /\
    W44=3 /\ X41=4 /\ X42=9 /\
    W52=9 /\ W54=8 /\ 
    

    % block
    all_different([A11,A12,A13,A14,A15,A21,A22,A23,A24,A25,A31,A32,A33,A34,A35,A41,A42,A43,A44,A45,A51,A52,A53,A54,A55]) /\ 
    all_different([B11,B12,B13,B14,B15,B21,B22,B23,B24,B25,B31,B32,B33,B34,B35,B41,B42,B43,B44,B45,B51,B52,B53,B54,B55]) /\ 
    all_different([C11,C12,C13,C14,C15,C21,C22,C23,C24,C25,C31,C32,C33,C34,C35,C41,C42,C43,C44,C45,C51,C52,C53,C54,C55]) /\ 
    all_different([D11,D12,D13,D14,D15,D21,D22,D23,D24,D25,D31,D32,D33,D34,D35,D41,D42,D43,D44,D45,D51,D52,D53,D54,D55]) /\ 
    all_different([E11,E12,E13,E14,E15,E21,E22,E23,E24,E25,E31,E32,E33,E34,E35,E41,E42,E43,E44,E45,E51,E52,E53,E54,E55]) /\ 
    all_different([F11,F12,F13,F14,F15,F21,F22,F23,F24,F25,F31,F32,F33,F34,F35,F41,F42,F43,F44,F45,F51,F52,F53,F54,F55]) /\ 
    all_different([G11,G12,G13,G14,G15,G21,G22,G23,G24,G25,G31,G32,G33,G34,G35,G41,G42,G43,G44,G45,G51,G52,G53,G54,G55]) /\ 
    all_different([H11,H12,H13,H14,H15,H21,H22,H23,H24,H25,H31,H32,H33,H34,H35,H41,H42,H43,H44,H45,H51,H52,H53,H54,H55]) /\ 
    all_different([I11,I12,I13,I14,I15,I21,I22,I23,I24,I25,I31,I32,I33,I34,I35,I41,I42,I43,I44,I45,I51,I52,I53,I54,I55]) /\ 
    all_different([J11,J12,J13,J14,J15,J21,J22,J23,J24,J25,J31,J32,J33,J34,J35,J41,J42,J43,J44,J45,J51,J52,J53,J54,J55]) /\ 
    all_different([K11,K12,K13,K14,K15,K21,K22,K23,K24,K25,K31,K32,K33,K34,K35,K41,K42,K43,K44,K45,K51,K52,K53,K54,K55]) /\ 
    all_different([L11,L12,L13,L14,L15,L21,L22,L23,L24,L25,L31,L32,L33,L34,L35,L41,L42,L43,L44,L45,L51,L52,L53,L54,L55]) /\ 
    all_different([M11,M12,M13,M14,M15,M21,M22,M23,M24,M25,M31,M32,M33,M34,M35,M41,M42,M43,M44,M45,M51,M52,M53,M54,M55]) /\ 
    all_different([N11,N12,N13,N14,N15,N21,N22,N23,N24,N25,N31,N32,N33,N34,N35,N41,N42,N43,N44,N45,N51,N52,N53,N54,N55]) /\ 
    all_different([O11,O12,O13,O14,O15,O21,O22,O23,O24,O25,O31,O32,O33,O34,O35,O41,O42,O43,O44,O45,O51,O52,O53,O54,O55]) /\ 
    all_different([P11,P12,P13,P14,P15,P21,P22,P23,P24,P25,P31,P32,P33,P34,P35,P41,P42,P43,P44,P45,P51,P52,P53,P54,P55]) /\ 
    all_different([Q11,Q12,Q13,Q14,Q15,Q21,Q22,Q23,Q24,Q25,Q31,Q32,Q33,Q34,Q35,Q41,Q42,Q43,Q44,Q45,Q51,Q52,Q53,Q54,Q55]) /\ 
    all_different([R11,R12,R13,R14,R15,R21,R22,R23,R24,R25,R31,R32,R33,R34,R35,R41,R42,R43,R44,R45,R51,R52,R53,R54,R55]) /\ 
    all_different([S11,S12,S13,S14,S15,S21,S22,S23,S24,S25,S31,S32,S33,S34,S35,S41,S42,S43,S44,S45,S51,S52,S53,S54,S55]) /\ 
    all_different([T11,T12,T13,T14,T15,T21,T22,T23,T24,T25,T31,T32,T33,T34,T35,T41,T42,T43,T44,T45,T51,T52,T53,T54,T55]) /\ 
    all_different([U11,U12,U13,U14,U15,U21,U22,U23,U24,U25,U31,U32,U33,U34,U35,U41,U42,U43,U44,U45,U51,U52,U53,U54,U55]) /\ 
    all_different([V11,V12,V13,V14,V15,V21,V22,V23,V24,V25,V31,V32,V33,V34,V35,V41,V42,V43,V44,V45,V51,V52,V53,V54,V55]) /\ 
    all_different([W11,W12,W13,W14,W15,W21,W22,W23,W24,W25,W31,W32,W33,W34,W35,W41,W42,W43,W44,W45,W51,W52,W53,W54,W55]) /\ 
    all_different([X11,X12,X13,X14,X15,X21,X22,X23,X24,X25,X31,X32,X33,X34,X35,X41,X42,X43,X44,X45,X51,X52,X53,X54,X55]) /\ 
    all_different([Y11,Y12,Y13,Y14,Y15,Y21,Y22,Y23,Y24,Y25,Y31,Y32,Y33,Y34,Y35,Y41,Y42,Y43,Y44,Y45,Y51,Y52,Y53,Y54,Y55]) /\ 

    % horizontal
    all_different([A11,A12,A13,A14,A15,B11,B12,B13,B14,B15,C11,C12,C13,C14,C15,D11,D12,D13,D14,D15,E11,E12,E13,E14,E15]) /\ 
    all_different([A21,A22,A23,A24,A25,B21,B22,B23,B24,B25,C21,C22,C23,C24,C25,D21,D22,D23,D24,D25,E21,E22,E23,E24,E25]) /\ 
    all_different([A31,A32,A33,A34,A35,B31,B32,B33,B34,B35,C31,C32,C33,C34,C35,D31,D32,D33,D34,D35,E31,E32,E33,E34,E35]) /\ 
    all_different([A41,A42,A43,A44,A45,B41,B42,B43,B44,B45,C41,C42,C43,C44,C45,D41,D42,D43,D44,D45,E41,E42,E43,E44,E45]) /\ 
    all_different([A51,A52,A53,A54,A55,B51,B52,B53,B54,B55,C51,C52,C53,C54,C55,D51,D52,D53,D54,D55,E51,E52,E53,E54,E55]) /\ 
    all_different([F11,F12,F13,F14,F15,G11,G12,G13,G14,G15,H11,H12,H13,H14,H15,I11,I12,I13,I14,I15,J11,J12,J13,J14,J15]) /\ 
    all_different([F21,F22,F23,F24,F25,G21,G22,G23,G24,G25,H21,H22,H23,H24,H25,I21,I22,I23,I24,I25,J21,J22,J23,J24,J25]) /\ 
    all_different([F31,F32,F33,F34,F35,G31,G32,G33,G34,G35,H31,H32,H33,H34,H35,I31,I32,I33,I34,I35,J31,J32,J33,J34,J35]) /\ 
    all_different([F41,F42,F43,F44,F45,G41,G42,G43,G44,G45,H41,H42,H43,H44,H45,I41,I42,I43,I44,I45,J41,J42,J43,J44,J45]) /\ 
    all_different([F51,F52,F53,F54,F55,G51,G52,G53,G54,G55,H51,H52,H53,H54,H55,I51,I52,I53,I54,I55,J51,J52,J53,J54,J55]) /\ 
    all_different([K11,K12,K13,K14,K15,L11,L12,L13,L14,L15,M11,M12,M13,M14,M15,N11,N12,N13,N14,N15,O11,O12,O13,O14,O15]) /\ 
    all_different([K21,K22,K23,K24,K25,L21,L22,L23,L24,L25,M21,M22,M23,M24,M25,N21,N22,N23,N24,N25,O21,O22,O23,O24,O25]) /\ 
    all_different([K31,K32,K33,K34,K35,L31,L32,L33,L34,L35,M31,M32,M33,M34,M35,N31,N32,N33,N34,N35,O31,O32,O33,O34,O35]) /\ 
    all_different([K41,K42,K43,K44,K45,L41,L42,L43,L44,L45,M41,M42,M43,M44,M45,N41,N42,N43,N44,N45,O41,O42,O43,O44,O45]) /\ 
    all_different([K51,K52,K53,K54,K55,L51,L52,L53,L54,L55,M51,M52,M53,M54,M55,N51,N52,N53,N54,N55,O51,O52,O53,O54,O55]) /\ 
    all_different([P11,P12,P13,P14,P15,Q11,Q12,Q13,Q14,Q15,R11,R12,R13,R14,R15,S11,S12,S13,S14,S15,T11,T12,T13,T14,T15]) /\ 
    all_different([P21,P22,P23,P24,P25,Q21,Q22,Q23,Q24,Q25,R21,R22,R23,R24,R25,S21,S22,S23,S24,S25,T21,T22,T23,T24,T25]) /\ 
    all_different([P31,P32,P33,P34,P35,Q31,Q32,Q33,Q34,Q35,R31,R32,R33,R34,R35,S31,S32,S33,S34,S35,T31,T32,T33,T34,T35]) /\ 
    all_different([P41,P42,P43,P44,P45,Q41,Q42,Q43,Q44,Q45,R41,R42,R43,R44,R45,S41,S42,S43,S44,S45,T41,T42,T43,T44,T45]) /\ 
    all_different([P51,P52,P53,P54,P55,Q51,Q52,Q53,Q54,Q55,R51,R52,R53,R54,R55,S51,S52,S53,S54,S55,T51,T52,T53,T54,T55]) /\ 
    all_different([U11,U12,U13,U14,U15,V11,V12,V13,V14,V15,W11,W12,W13,W14,W15,X11,X12,X13,X14,X15,Y11,Y12,Y13,Y14,Y15]) /\ 
    all_different([U21,U22,U23,U24,U25,V21,V22,V23,V24,V25,W21,W22,W23,W24,W25,X21,X22,X23,X24,X25,Y21,Y22,Y23,Y24,Y25]) /\ 
    all_different([U31,U32,U33,U34,U35,V31,V32,V33,V34,V35,W31,W32,W33,W34,W35,X31,X32,X33,X34,X35,Y31,Y32,Y33,Y34,Y35]) /\ 
    all_different([U41,U42,U43,U44,U45,V41,V42,V43,V44,V45,W41,W42,W43,W44,W45,X41,X42,X43,X44,X45,Y41,Y42,Y43,Y44,Y45]) /\ 
    all_different([U51,U52,U53,U54,U55,V51,V52,V53,V54,V55,W51,W52,W53,W54,W55,X51,X52,X53,X54,X55,Y51,Y52,Y53,Y54,Y55]) /\ 
    % vertical
    all_different([A11,A21,A31,A41,A51,F11,F21,F31,F41,F51,K11,K21,K31,K41,K51,P11,P21,P31,P41,P51,U11,U21,U31,U41,U51]) /\ 
    all_different([A12,A22,A32,A42,A52,F12,F22,F32,F42,F52,K12,K22,K32,K42,K52,P12,P22,P32,P42,P52,U12,U22,U32,U42,U52]) /\ 
    all_different([A13,A23,A33,A43,A53,F13,F23,F33,F43,F53,K13,K23,K33,K43,K53,P13,P23,P33,P43,P53,U13,U23,U33,U43,U53]) /\ 
    all_different([A14,A24,A34,A44,A54,F14,F24,F34,F44,F54,K14,K24,K34,K44,K54,P14,P24,P34,P44,P54,U14,U24,U34,U44,U54]) /\ 
    all_different([A15,A25,A35,A45,A55,F15,F25,F35,F45,F55,K15,K25,K35,K45,K55,P15,P25,P35,P45,P55,U15,U25,U35,U45,U55]) /\ 
    all_different([B11,B21,B31,B41,B51,G11,G21,G31,G41,G51,L11,L21,L31,L41,L51,Q11,Q21,Q31,Q41,Q51,V11,V21,V31,V41,V51]) /\ 
    all_different([B12,B22,B32,B42,B52,G12,G22,G32,G42,G52,L12,L22,L32,L42,L52,Q12,Q22,Q32,Q42,Q52,V12,V22,V32,V42,V52]) /\ 
    all_different([B13,B23,B33,B43,B53,G13,G23,G33,G43,G53,L13,L23,L33,L43,L53,Q13,Q23,Q33,Q43,Q53,V13,V23,V33,V43,V53]) /\ 
    all_different([B14,B24,B34,B44,B54,G14,G24,G34,G44,G54,L14,L24,L34,L44,L54,Q14,Q24,Q34,Q44,Q54,V14,V24,V34,V44,V54]) /\ 
    all_different([B15,B25,B35,B45,B55,G15,G25,G35,G45,G55,L15,L25,L35,L45,L55,Q15,Q25,Q35,Q45,Q55,V15,V25,V35,V45,V55]) /\ 
    all_different([C11,C21,C31,C41,C51,H11,H21,H31,H41,H51,M11,M21,M31,M41,M51,R11,R21,R31,R41,R51,W11,W21,W31,W41,W51]) /\ 
    all_different([C12,C22,C32,C42,C52,H12,H22,H32,H42,H52,M12,M22,M32,M42,M52,R12,R22,R32,R42,R52,W12,W22,W32,W42,W52]) /\ 
    all_different([C13,C23,C33,C43,C53,H13,H23,H33,H43,H53,M13,M23,M33,M43,M53,R13,R23,R33,R43,R53,W13,W23,W33,W43,W53]) /\ 
    all_different([C14,C24,C34,C44,C54,H14,H24,H34,H44,H54,M14,M24,M34,M44,M54,R14,R24,R34,R44,R54,W14,W24,W34,W44,W54]) /\ 
    all_different([C15,C25,C35,C45,C55,H15,H25,H35,H45,H55,M15,M25,M35,M45,M55,R15,R25,R35,R45,R55,W15,W25,W35,W45,W55]) /\ 
    all_different([D11,D21,D31,D41,D51,I11,I21,I31,I41,I51,N11,N21,N31,N41,N51,S11,S21,S31,S41,S51,X11,X21,X31,X41,X51]) /\ 
    all_different([D12,D22,D32,D42,D52,I12,I22,I32,I42,I52,N12,N22,N32,N42,N52,S12,S22,S32,S42,S52,X12,X22,X32,X42,X52]) /\ 
    all_different([D13,D23,D33,D43,D53,I13,I23,I33,I43,I53,N13,N23,N33,N43,N53,S13,S23,S33,S43,S53,X13,X23,X33,X43,X53]) /\ 
    all_different([D14,D24,D34,D44,D54,I14,I24,I34,I44,I54,N14,N24,N34,N44,N54,S14,S24,S34,S44,S54,X14,X24,X34,X44,X54]) /\ 
    all_different([D15,D25,D35,D45,D55,I15,I25,I35,I45,I55,N15,N25,N35,N45,N55,S15,S25,S35,S45,S55,X15,X25,X35,X45,X55]) /\ 
    all_different([E11,E21,E31,E41,E51,J11,J21,J31,J41,J51,O11,O21,O31,O41,O51,T11,T21,T31,T41,T51,Y11,Y21,Y31,Y41,Y51]) /\ 
    all_different([E12,E22,E32,E42,E52,J12,J22,J32,J42,J52,O12,O22,O32,O42,O52,T12,T22,T32,T42,T52,Y12,Y22,Y32,Y42,Y52]) /\ 
    all_different([E13,E23,E33,E43,E53,J13,J23,J33,J43,J53,O13,O23,O33,O43,O53,T13,T23,T33,T43,T53,Y13,Y23,Y33,Y43,Y53]) /\ 
    all_different([E14,E24,E34,E44,E54,J14,J24,J34,J44,J54,O14,O24,O34,O44,O54,T14,T24,T34,T44,T54,Y14,Y24,Y34,Y44,Y54]) /\ 
    all_different([E15,E25,E35,E45,E55,J15,J25,J35,J45,J55,O15,O25,O35,O45,O55,T15,T25,T35,T45,T55,Y15,Y25,Y35,Y45,Y55])

;


output [
%   show(Vars)
   if j = 1 then "\n" else " " endif ++
     show(Vars[25*(i-1) + j]) ++
   if i mod 5 = 0 /\ j = 25 then "\n" else "" endif ++
   if j mod 5 = 0 then " " else "" endif 
   | i,j in 1..25

];