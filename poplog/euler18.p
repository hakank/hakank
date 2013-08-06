/*

  Problem 18
  """
  By starting at the top of the triangle below and moving to adjacent 
  numbers on the row below, the maximum total from top to bottom is 23.

  3
  7 4
  2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

  75
  95 64
  17 47 82
  18 35 87 10
  20 04 82 47 65
  19 01 23 75 03 34
  88 02 77 73 07 63 67
  99 65 04 28 06 16 70 92
  41 41 26 56 83 40 80 70 33
  41 48 72 33 47 32 37 16 94 29
  53 71 44 65 25 43 91 52 97 51 14
  70 11 33 28 77 73 17 78 39 68 17 57
  91 71 52 38 17 14 91 43 58 50 27 29 48
  63 66 04 68 89 53 67 30 73 16 69 87 40 31
  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

  NOTE: As there are only 16384 routes, it is possible to solve this problem 
  by trying every route. However, Problem 67, is the same challenge with a 
  triangle containing one-hundred rows; it cannot be solved by brute force, 
  and requires a clever method! ;o)
  """

  Note: This program use the GOSPL library for the split_with function:
  http://www.cs.bham.ac.uk/research/projects/poplog/gospl_1_2_0.tar.gz


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

vars tri = [];
vars ms = 0;

define pp(row, column, sum);
    row+1->row;
    if row == length(tri)+1 then
       if sum > ms then
           sum->ms;
       endif;
       return;
    endif;

    lvars i;    
    for i from 0 to 1 do
        pp(row,column+i, sum+tri(row)(column+i));
    endfor;

enddefine;

define problem18();
    vars tri_lines, tri;
    vars tri_str = 
'75\
95 64\
17 47 82\
18 35 87 10\
20 04 82 47 65\
19 01 23 75 03 34\
88 02 77 73 07 63 67\
99 65 04 28 06 16 70 92\
41 41 26 56 83 40 80 70 33\
41 48 72 33 47 32 37 16 94 29\
53 71 44 65 25 43 91 52 97 51 14\
70 11 33 28 77 73 17 78 39 68 17 57\
91 71 52 38 17 14 91 43 58 50 27 29 48\
63 66 04 68 89 53 67 30 73 16 69 87 40 31\
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23';

    [%split_with(tri_str, '\n')%] -> tri_lines;
    lvars i,x;
    for i from 1 to length(tri_lines) do 
        ;;;lvars p = [%split(tri_lines(i))%];
        ;;; strnumber to convert the strings to number
        ;;;[% for x in p do strnumber(x) endfor%]->p;
        ;;; as a one liner:
        lvars p = [%split(tri_lines(i))%].maplist(%procedure(e); strnumber(e); endprocedure%);
        tri <> [^p]->tri;
    endfor;
    pp(1,1, tri(1)(1));
    ms=>;

enddefine;

'problem18()'=>
problem18();
