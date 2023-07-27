% 
% Global constraint diffn/1 in MiniZinc.
%
% Features different representations of the orthotopes:
%  - [xorig, xend,  yorig, yend, ...]: "end" representation
%  - [xorig, xsize, yorig, ysize, ...]: "size" representation
%  - [xorig, yorig, ..., xsize, ysize, ...]: "chip" representation
% 
% converting between the representations
%  - end2chip
%  - size2chip
%  - end2size
%
% calculations of areas in all representations.
%
%
%
% From Global Constraint catalog;
% https://sofdem.github.io/gccat/gccat/Cdiffn.html
% """
%
% diffn​(ORTHOTOPES)
% Purpose
%
% Generalised multi-dimensional non-overlapping constraint: 
% Holds if, for each pair of orthotopes (O1,O2), O1 and O2 do not overlap. 
% Two orthotopes do not overlap if there exists at least one dimension 
% where their projections do not overlap.
% 
% Example
%  (
%    <
%  orth - ori - 2 siz - 2 end - 4,  ori - 1 siz - 3 end - 4, 
%  orth - ori - 4 siz - 4 end - 8,  ori - 3 siz - 3 end - 6, 
%  orth - ori - 9 siz - 2 end - 11, ori - 4 siz - 3 end - 7
%  >
%  )
%
% Figure 4.89.1 represents the respective position of the three rectangles 
% of the example. The co -ordinates of the leftmost lowest corner of each 
% rectangle are stressed in bold. The diffn constraint holds since the 
% three rectangles do not overlap.
%
%    -----------------------
%   6|               |     |
%    |      -------- | R3  |
%   5|      |      | |     |
%    |      |      | |     |
%   4|      | R2   | |     |
%    | ------      | |-----|
%   3| |R1  |      |       |
%    | |    |------|       |
%   2| |    |              |
%    | |    |              |
%   1| |    |              |
%    | -----               |
%   0|-------------------- |
%    0 1 2 3 4 5 6 7 8 9 10
%
% """

% Note: This model just handle rectangles.

% 
% Also see (about the CHIP system)
% http://www.cosytec.com/production_scheduling/chip/pdf/the_chip_system.pdf
% Note: This is the [xorig, xlen] (CHIP) representation.
% 
% """
% We consider a set of n rectangles with lower left corner (Xi,Yi), 
% with length Li and height Hi. These rectangles should not overlap. 
% This means that for each pair of rectangles Ri and Rj, at least 
% one of these four conditions must be true:
%  * Ri is above Rj
%  * Ri is below Rj
%  * Ri is to the left of Rj
%  * Ri is to the right of Rj
% ...
% 
% diffn([ [X1,Y1,L1,H1], [X2,Y2,L2,H2], ...,[Xm,Ym,Lm,Hm]]).
% 
% Another example: 
%         xO y0 xL yL   x0 y0 xL yL  x0 y0 xL yL 
% diffn([[ 1, 2, 2, 2],[3, 1, 2, 1],[4, 2, 3, 3]]
% 
 

% size of orthotopes, m = 2*d where d is the dimension
% i.e. 2-dim (rectangle) gives m = 2*2 = 4
% int: n = 3; % number of orthotopes. For the above example and the CHIP example
int: n = 20; % number of orthotopes. For the "reverse" problem
int: m = 4; % 2*(dimension of orthotopes)

array[1..n, 1..m] of var 1..n+1: r; % the coordinates
% array[1..n, 1..m] of var 1..11: r2; % the coordinates, for, X2chip
array[1..n] of var 0..1000: areas; % the area of the orthotopes

% sum of all the coordinates (to minimize)
% var int: coord_sum = sum(i in 1..n, j in 1..m div 2) (r[i,j]);

%
% diffn_size: ensure that no orthotope overlaps.
% [xorig, xsize, ...], "size" representation
%
predicate diffn_size(array[int,int] of var int: x) =
    forall(i, j in index_set_1of2(x) where i < j) (
       diff2_size(
              [x[i,k] | k in index_set_2of2(x)] , 
              [x[j,k] | k in index_set_2of2(x)] 
          )
    )
;


%
% diffn_end: ensure that no orthotope overlaps
% [xorig, xend, ...], "end" representation
%
predicate diffn_end(array[int,int] of var int: x) =
    forall(i, j in index_set_1of2(x) where i < j) (
      diff2_end(
              [x[i,k] | k in index_set_2of2(x)] , 
              [x[j,k] | k in index_set_2of2(x)] 
          )
    )
;

%
% diffn_chip: ensure that no orthotope overlaps.
% [xorig, yorig, ..., xsize, ysize, ...], "CHIP" representation
%
predicate diffn_chip(array[int,int] of var int: x) =
    forall(i, j in index_set_1of2(x) where i < j) (
       diff2_chip(
              [x[i,k] | k in index_set_2of2(x)] , 
              [x[j,k] | k in index_set_2of2(x)] 
          )
    )
;


%
% diff2_end: "end" representation
% 
% This assumes a an array of orthotopes with the following elements:
%    [xorig, xend,   yorig, yend, ... ]
%      1      2       3      4 
%
predicate diff2_end(array[int] of var int: R1, array[int] of var int: R2) =
    assert(length(R1) = length(R2), "R1 and R2 must be of the same length")
    /\
    % sanity check: the rectangle must at least be 1 x 1
    forall(i in 0..(length(R1) div 2)-1) (
       (
       R1[2*i+1] < R1[2*i+2] /\  R2[2*i+1] < R2[2*i+2]
       )
    )
    /\ % the diff (overlap) check
    exists(i in 0..(length(R1) div 2)-1) (
       % R1 left, below, ... of R2  
       R1[(2*i)+1] <= R2[(2*i)+2]

       \/
       % R1 right, above, ... of R2
       R1[(2*i)+2]   >= R2[(2*i)+1]
    )
; 


%
% diff2_size: [xorig, xsize, ...], "size" representation
%    
predicate diff2_size(array[int] of var int: R1, array[int] of var int: R2) =
    assert(length(R1) = length(R2), "R1 and R2 must be of the same length")
    /\
     exists(i in 0..(length(R1) div 2)-1) (

        % r1_end <= r2_orig: to the left/below/before
        (R1[(2*i)+1] + R1[(2*i)+2] <= R2[(2*i)+1])

        \/ % r1_orig >= r2_end: to the right/above/after
        (R1[(2*i)+1]               >= R2[(2*i)+1] + R2[(2*i)+2])
     )
;


%
% diff2_chip: for [xorig, yorig, ..., xsize, ysize, ...] (CHIP) representation
%    
predicate diff2_chip(array[int] of var int: R1, array[int] of var int: R2) =
    let {
        int: t = (length(R1) div 2)
    }
    in
    assert(length(R1) = length(R2), "R1 and R2 must be of the same length")
    /\
     exists(i in 1..t) (
        % r1_end <= r2_orig: to the left/below/before
        (R1[i] + R1[i+t] <= R2[i])
        \/ % r1_orig >= r2_end: to the right/above/after
        (R1[i]           >= R2[i] + R2[i+t])
     )
;


%
% end2chip
% converts end representation of an matrix to CHIP representation
%
predicate end2chip(array[int, int] of var int: R_end, 
                   array[int, int] of var int: R_chip) =
   let {
        int: t = (card(index_set_2of2(R_chip)) div 2)-1
   } 
   in
   forall(i in index_set_1of2(R_chip)) (
     forall(j in 0..t) (
       R_chip[i,j+1] = R_end[i, (2*j)+1] % orig
       /\
       R_chip[i,j+1+t+1] = R_end[i, (2*j)+2] - R_end[i, (2*j)+1] % size

     )
   )
;

%
% size2chip
% converts a size representation to CHIP representation
%
predicate size2chip(array[int, int] of var int: R_size, 
                   array[int, int] of var int: R_chip) =
   let {
        int: t = (card(index_set_2of2(R_size)) div 2)-1
   } 
   in
   forall(i in index_set_1of2(R_size)) (
     forall(j in 0..t) (
       R_chip[i,j+1] = R_size[i, (2*j)+1] % orig
       /\
       R_chip[i,j+1+t+1] = R_size[i, (2*j)+2] % size
     )
   )
;

%
% end2size
% converts an end representation to size representation
%
predicate end2size(array[int, int] of var int: R_end, 
                   array[int, int] of var int: R_size) =
   let {
        int: t = (card(index_set_2of2(R_size)) div 2)-1
   } 
   in
   forall(i in index_set_1of2(R_size)) (
     forall(j in 0..t) (
       R_size[i,(2*j)+1] = R_end[i, (2*j)+1] % orig
       /\
       R_size[i,(2*j)+2]  = R_end[i, (2*j)+2] - R_end[i,(2*j)+1] % size
     )
   )
;



%
% calculate the size of an orthotope (rectangle, etc)
% r_area_end version
% 
% (Unfortunately the function product() don't work in MiniZinc <= 8.1)
%
predicate r_area_end(var int: s, array[int] of var int: R) = 
  let {
     int: t = length(R) div 2,
     array[1..t] of var lb_array(R)..ub_array(R): a
  }
  in
  a[1] = R[2] - R[1]
  /\
  forall(i in 1..t-1) ( 
      a[i+1] = a[i]*(R[(2*i)+2] - R[2*i+1])
  )
  /\
  s = a[t]
;

%
% calculate the size of an orthotope (rectangle, etc)
% r_area_size version
% [xorig, xsize, yorig, ysize, ...)
%
predicate r_area_size(var int: s, array[int] of var int: R) = 
  let {
     int: t = length(R) div 2,
     array[1..t] of var lb_array(R)..ub_array(R): a
  }
  in
  a[1] = R[2]
  /\
  forall(i in 1..(length(R) div 2)-1) ( 
      a[i+1] = a[i]*R[(2*i)+2]
  )
  /\
  s = a[t]
;

%
% calculate the size of an orthotope (rectangle, etc)
% r_area_chip version
% [xorig, yorig, ..., xsize, ysize, ...)
%
predicate r_area_chip(var int: s, array[int] of var int: R) = 
  let {
     int: t = length(R) div 2,
     array[1..t] of var lb_array(R)..ub_array(R): a
  }
  in
  a[1] = R[t+1]
  /\
  forall(i in 1..t-1) ( 
      a[i+1] = a[i]*R[t+i+1]
  )
  /\
  s = a[t]
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

predicate cp1d(array[int] of var int: x, array[int] of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 


% solve satisfy;
% solve minimize coord_sum;
% solve :: int_search(areas ++ [r[i,j] | i in 1..n, j in 1..m], occurrence, indomain_split, complete) satisfy;
% solve :: int_search(areas ++ [r[i,j] | i in 1..n, j in 1..m], first_fail, indomain_min, complete) satisfy;
% solve maximize areas[n];
solve :: int_search(areas ++ [r[i,j] | i in 1..n, j in 1..m], input_order, indomain, complete) satisfy;

constraint

    % The example above from Global Constraint catalog
    % in "end" representation: [xorig, xend, yorig, yend]
%     cp2d(r,array2d(1..n, 1..m, [  2,4,  1,4,    4,8, 3,6,     9,11, 4,7 ])) 
%     /\
%     diffn_end(r)
%     /\
%     forall(i in 1..n) (
%        r_area_end(areas[i], [r[i,k] | k in 1..m] )
%     )


      %
      % the CHIP example (above) as end representation: 
      % [xorig, xend, yorig, yend]
      % 
%      cp2d(r, array2d(1..n, 1..m, [
%             1,3, 2,4,     
%             3,5, 1,2,      
%             % _,_,_,_,
%             4,7, 2,5
%             ]))
%       /\
%       diffn_end(r)
%       /\
%       forall(i in 1..n) (
%          r_area_end(areas[i], [r[i,k] | k in 1..m] )
%       )
%          /\ % converting to chip representation
%        end2chip(r,r2)
%        /\
%        diffn_chip(r2)
%        /\
%        forall(i in 1..n) (
%           r_area_chip(areas[i], [r2[i,k] | k in 1..m] )
%        )

%          /\ % converting to size representation
%         end2size(r,r2)
%         /\
%         diffn_size(r2)
%         /\
%         forall(i in 1..n) (
%            r_area_size(areas[i], [r2[i,k] | k in 1..m] )
%         )


      % 
      % the CHIP example as size representation: 
      % [xorig, xsize, yorig, ysize]
      % 
%       cp2d(r,array2d(1..n, 1..m, [1,2, 2,2,      3,2, 1,1,   4,3, 2,3 ]))
%       /\
%       diffn_size(r)
%       /\
%       forall(i in 1..n) (
%          r_area_size(areas[i], [r[i,k] | k in 1..m] )
%       )

%         /\ % converting to chip representation
%        size2chip(r,r2)
%        /\
%        diffn_chip(r2)
%        /\
%        forall(i in 1..n) (
%           r_area_chip(areas[i], [r2[i,k] | k in 1..m] )
%        )



%      % 
%      % the CHIP example as CHIP representation: 
%      % [xorig, yorig, xsize, ysize]
%      % 
%      cp2d(r,array2d(1..n, 1..m, [ 1,2, 2,2,    3,1, 2,1,   4,2, 3,3, ]))
%      /\
%      diffn_chip(r)
%      /\
%      forall(i in 1..n) (
%         r_area_chip(areas[i], [r[i,k] | k in 1..m] )
%      )

     %
     % reverse problem: given the areas calculate r
    %
      cp1d(areas,[1,2,3,4,5,6,7,8,9,10,
              11,12,13,14,15,16,17,18,19,20,
              % 21,22,23,24,25,26,27,28,29,30,
                ])
      /\
      % diffn_end(r)    % [xorig, xend,  yorig, yend]
      % diffn_size(r) % [xorig, xsize, yorig, ysize]
      diffn_chip(r) % [xorigm yorig, xsize, ysize]
      /\
      forall(i in 1..n) (
         % r_area_end(areas[i], [r[i,k] | k in 1..m] )
         % r_area_size(areas[i], [r[i,k] | k in 1..m] )
         r_area_chip(areas[i], [r[i,k] | k in 1..m] )
      )
%      /\
%      coord_sum > 0

;


output [
  if j = 1 then "\n" else " " endif ++
   show(r[i,j])
  | i in 1..n, j in 1..m
]
  ++
[ "\nareas: ", show(areas), "\n",
%   "coord_sum: ", show(coord_sum), "\n",
] ++ ["\n"]
;

