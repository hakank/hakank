/*

  Euler problem 42
  """
  The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
  so the first ten triangle numbers are:
  
  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
  
  By converting each letter in a word to a number corresponding to its 
  alphabetical position and adding these values we form a word value. For example, 
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
  is a triangle number then we shall call the word a triangle word.
  
  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
  containing nearly two-thousand common English words, how many 
  are triangle words?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define triangle_number(n);
    return((n*(n+1))/2);
enddefine;

define get_score(name)->total;
    lvars total=0;
    lvars i;
    for i from 1 to length(name) do
        total+(name(i)-64)->total;
    endfor;
    total->total;
enddefine;

define problem42;

    ;;; Note: when words.txt didn't have a newline it took about a
    ;;; second just to read the file. With the newline added it 
    ;;; took no time.

    lvars all, num;
    ;;; Note: sys_read_lines returns (result, number_of_read_lines)
    sys_read_lines('words.txt',1,1,true) -> (all,num);

    lvars i;
    lvars stripped=[%for i in all.unpackitem do 
                              if i(1) /= 34 then i; endif; 
                          endfor%].packitem><'';
    ;;; lvars words=[%split_with(stripped,',')%];
    lvars words=[%sys_parse_string(stripped,`,`)%];

    ;;; Take the 20 first triangle numbers
    lvars t20 = [% for i from 1 to 20 do triangle_number(i) endfor %];
    lvars w;
    lvars sum = 0;
    for w in words do 
        lvars score = get_score(w);
        if member(score, t20) then
            sum+1->sum;
        endif;
    endfor;
    sum=>
enddefine;


'problem42()'=>
problem42();


