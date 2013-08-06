/*

Fri Dec 28 00:57:39 2007/hakank@bonetmail.com

From Lingo, samples/box

MODEL:
! Design a box at minimum cost that meets
 area, volume, marketing and aesthetic requirements;
 [COST] min = 2*( .05*(d*w + d*h) +.1*w*h);
 [SURFACE]  2*(h*d + h*w + d*w) >= 888;
 [VOLUME]   h*d*w >= 1512;
! These two enforce aesthetics;
 [NOTNARRO] h/w <= .718;
 [NOTHIGH]  h/w >= .518;
! Marketing requires a small footprint;
 [FOOTPRNT] d*w <= 252;
END

Lingo gives the following:

  Objective value:                              50.96508

                       Variable           Value        Reduced Cost
                              D        23.03096            0.000000
                              W        9.562196            0.000000
                              H        6.865657            0.000000

                            Row    Slack or Surplus      Dual Price
                           COST        50.96508           -1.000000
                        SURFACE        0.000000          -0.2342588E-01
                         VOLUME        0.000000          -0.1329933E-01
                       NOTNARRO        0.000000            2.298546
                        NOTHIGH       0.2000000            0.000000
                       FOOTPRNT        31.77343            0.000000


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

var d >= 0 := 2;
var w >= 0 := 2;
var h >= 0 := 2;

# Design a box at minimum cost that meets
# area, volume, marketing and aesthetic requirements;
 minimize COST: 2*( .05*(d*w + d*h) +.1*w*h);

subject to
 SURFACE:  2*(h*d + h*w + d*w) >= 888;
 VOLUME:   h*d*w >= 1512;
# These two enforce aesthetics;
 NOTNARRO: h/w <= .718;
 NOTHIGH:  h/w >= .518;
# Marketing requires a small footprint;
 FOOTPRNT: d*w <= 252;

# data;

# option solver cplex; # cplex kan inte lösa det (i alla fall utan option)
# option solver bonmin; # OK
# option solver minos; # funkar med initieringar
# option solver donlp2; # OK
# option solver pcx; # nope
#  option solver gjh; # nope

solve;
display _obj, _obj.sstatus, _obj.relax;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack,_var.dual,_var.relax; 

display _conname,_con, _con.body,_con.slack, _con.lb, _con.ub, _con.dual,_con.relax; 

option nl_comments 1;

/*
note: first letter is the code for -o
        -o0 {no files written}
        -obstub {write stub.nl in generic binary format -- like -og,
                but binary after the first 10 lines}
        -ogstub {write stub.nl in generic ASCII format}
        -omstub {write stub.mps in MPS format; nonlinearity disallowed}
        -onstub {write MPS format to stdout, honoring $auxfiles}
        -om {(no stub) write MPS format to stdout, ignoring $auxfiles}

i.e. gbox is
  -gbox 
which gives box.nl
And it's g one shoule use (ASCII)
or perhaps m or m for MPS, but they don't handle
non-linearities.

*/
# write gbox;