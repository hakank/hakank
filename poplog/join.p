/*

   join(string, separator)

   given a string str and a separator sep (as string)
      join(string, sep) 
   creates a string with the characters in str with
   sep interleaved.

   Examples:

   join('hello,world','|')=>
   ** h|e|l|l|o|,|w|o|r|l|d

   join('abc','@.@*')=>
   ** a@.@*b@.@*c

   It could be constructed as follows:
   lvars str, s;
   '' ->str; 
   for s in_string 'hello, world' do 
       str><consstring(s,'@.@*'.explode,5)->str; 
   endfor;
   str=>
   ** h@.@*e@.@*l@.@*l@.@*o@.@*,@.@* @.@*w@.@*o@.@*r@.@*l@.@*d@.@*

   Except that it has the separator at the end.

*/

/*
   This more compact and elegant formulation was suggested
   by Aaron Sloman ( http://www.cs.bham.ac.uk/~axs/ )
*/
define appdata_butlast(n, struct, proc);

      ;;; Apply proc to transform all elements of struct except for
      ;;; last n which are simply left on stack.

      lvars len = datalength(struct);

      checkinteger(n, 0, len);

      len - n -> len;

      appdata(struct,
              procedure(item);
                      if len == 0 then
                              item
                      else
                              proc(item);
                              len - 1 -> len
                      endif
              endprocedure)

enddefine;

define join(str, sep);

    consstring(#|
        appdata_butlast(1, str,
                          procedure(); explode(sep) endprocedure);
                |#);

enddefine;


/*
;;; my original function
define join(str, sep);
    lvars str_list;
    if islist(str) then
        str -> str_list;
    else
        str.datalist -> str_list;
    endif;
    lvars len = sep.length + 1;
    lvars res = '';
    lvars s;
    for s in allbutlast(1,str_list) do
        res >< consstring(s, sep.explode, len) -> res;
    endfor;
    ;;; and finally the last element in list 
    res >< consstring(last(str_list),1) -> res;
    return(res);

enddefine;
*/