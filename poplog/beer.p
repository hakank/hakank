/*
   Mon Dec  1 18:12:02 2008/hakank@bonetmail.com

   From mailinglist 
Date: Mon Oct 30 12:07:30 1995
Subject: Re: Pop Beer Needed
From: John Williams

*/
compile('/home/hakank/Poplib/init.p');


 true -> pop_longstrings;

 define beer(n);
     until n == 0 do
         format_print(
             '~@(~R~) bottle~:P of beer on the wall.~%~
              ~:*~@(~R~) bottle~:P of beer!~%~
              ~:*Take ~[~;it~:;one~] down, pass it around.~%',
             [^n]);
         n - 1 -> n;
         format_print('~[No~:;~:*~@(~R~)~] more bottle~:P of beer on the wall.~2%', [^n])
     enduntil
 enddefine;

 beer(99);
