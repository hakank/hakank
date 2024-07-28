/*

  de Bruijn sequences in Pop-11.

  See http://en.wikipedia.org/wiki/De_Bruijn_sequence
  """
  In combinatorial mathematics, a de Bruijn sequence of order n on 
  a size-k alphabet A is a cyclic sequence in which every possible 
  length-n string on A occurs exactly once as a substring (i.e., as 
  a contiguous subsequence). Such a sequence is denoted by B(k, n) 
  and has length kn, which is also the number of distinct strings of 
  length n on A. Each of these distinct strings, when taken as 
  a substring of B(k, n), must start at a different position, because 
  substrings starting at the same position are not distinct. Therefore, 
  B(k, n) must have at least kn symbols. And since B(k, n) has exactly kn 
  symbols, De Bruijn sequences are optimally short with respect to the 
  property of containing every string of length n exactly once. 
  """
  
  This is a port of my Unicode version
  http://www.hakank.org/unicode/debruijn.icn
    
  which is a a port of (a part of) the original C program with
  the following copyright:

    -----------------------------------------------------------------------------
    | C program to generate necklaces, Lyndon words, and De Bruijn              |
    | sequences.  The algorithm is CAT and is described in the book             |
    | "Combinatorial Generation."  This program, was obtained from the          |
    | (Combinatorial) Object Server, COS, at http://www.theory.csc.uvic.ca/~cos |
    | The inputs are n, the length of the string, k, the arity of the           |
    | string, and density, the maximum number of non-0's in the string.         |
    | The De Bruijn option doesn't make sense unless density >= n.              |
    | The program can be modified, translated to other languages, etc.,         |
    | so long as proper acknowledgement is given (author and source).           |
    | Programmer: Frank Ruskey (1994), translated to C by Joe Sawada            |
    -----------------------------------------------------------------------------

  Note: An adjustment has been done to the algorithm cited above 
  - adjusted to 1-based 

  Compare with my web based programs:
  - http://www.hakank.org/comb/debruijn.cgi   
  - http://www.hakank.org/comb/deBruijnApplet.html
  - http://www.hakank.org/javascript_progs/debruijn.html

  Syntax: 
    $ pop11 debruijn.p <k> <n> <p>
      where
       n: the language: 0..n-1
       k: length of each sub sequence
       p: if a flatten version should be printed as well

  Examples:

    $ pop11 debruijn.p 2 3 1
    k: 2  n: 3 , i.e. language: 0..k-1 (0..1) and with string len n (3)
    0 
    0 0 1 
    0 1 1 
    1 

    As a flat sequence: 0 0 0 1 0 1 1 1 (0 0)
    length:  2 ** 3+3-1 =  8 + 2 = 10


    The port code / door lock sequence: digits 0..9 of length 4
    $ pop11 debruijn.p 10 4 1
    k: 10  n: 4 , i.e. language: 0..k-1 (0..9) and with string len n (4)
    0 
    0 0 0 1 
    0 0 0 2 
    0 0 0 3 
    0 0 0 4 
    ...
    As a flat sequence: 0 0 0 0 1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0 5 0 0 0 6 0 0 0 7 0 0 0 8 0 0 0 9 0 0 1 1 0 0 1 2 0 
    ...
    8 9 8 7 8 9 9 7 9 7 9 8 8 7 9 8 9 7 9 9 8 7 9 9 9 8 8 8 8 9 8 8 9 9 8 9 8 9 9 9 9 (0 0 0)

    length: 10**4+4-1 = 10000 + 3 = 10003

    (See http://www.hakank.org/comb/debruijn_k_10_n_4.html )

  
  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

global vars k, n, L, a, print_seq;

define Print(p);
    lvars j t;
    if n mod p = 0 do
        for j from 1 to p do
            a(j+1) -> t;
            [^^L ^t] -> L;
            ppr(t);
        endfor;
        nl(1);
    endif;
enddefine;


define Gen(t, p, ones);
    lvars j;
    if ones <= n  do
        if t > n do
            Print(p);
        else
            a(t-p+1) -> a(t+1);
            if a(t+1) > 0 do
                Gen(t+1,p,ones+1);
            else 
                Gen(t+1,p,ones);
            endif;
            for j from a(t-p+1)+1 to k-1 do 
                j -> a(t+1);
                Gen(t+1,t,ones+1);
            endfor;
        endif;
    endif;
    
enddefine;


define debruijn(k1,n1,print_seq1);
    lvars j after;
    
    ;;; the door lock combination solution
    ;;; is k = 10, n = 4
    ;;; Also see: http://www.hakank.org/comb/debruijn_k_10_n_4.html

    ;;; k: language (0..k-1)
    k1 -> k;

    ;;; n: string length
    n1 -> n;

    ;;; also print the sequence as a flat list?
    print_seq1 -> print_seq;
    
    ['k:' ^k ' n:' ^n ' i.e. language: 0..k-1 (0..'^(k-1) ') and with string len n (' ^n ')')]=>;
    [] -> L;
    [% for j from 1 to 101 do 0 endfor %] -> a;
    0 -> a(1);

    Gen(1,1,0);
    
    ;;; flat sequence
    if print_seq do
        '\nAs a flat sequence:'=>;
        [% for j from 1 to n-1 do L(j) endfor %] -> after;
        [^^L '(' ^^after ')'].flatten.ppr;
    endif;
    nl(1);
    ['length: ' ^k '^' ^n '+' ^n '-1 = ' ^(k**n) ' + ' ^( n-1) ' = ' ^( k**n+n-1) ]=>;

    
enddefine;



;;; debruijn(10,4,true);
lvars k1 = 3, n1 = 4, print_seq1=true;
if poparglist.length = 3 do
    poparglist(1).strnumber->k1;
    poparglist(2).strnumber->n1;
    poparglist(3)->print_seq1;
endif;
debruijn(k1,n1,print_seq1);
