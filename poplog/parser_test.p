/* 
  From HELP define_parser 


*/

uses lr_parser;
    define :parser Lambda();
    tokens
        \  .  '('  ')'  VAR:isword
    endtokens
    rules
        exp ::=
                \ VAR:x . exp:e      { [ABS ^x ^e] }
            |   appexp
            ;
        appexp ::=
                appexp:e1 aexp:e2    { [APP ^e1 ^e2] }
            |   aexp
            ;
        aexp ::=
                VAR:x                { [VAR ^x] }
            |   '(' exp:e ')'        { e }
            ;
    endrules
    enddefine;


    define Lambda_input();
        lvars c = cucharin();
        while c == `\s` or c == `\t` do
            ;;; skip leading spaces
            cucharin() -> c;
        endwhile;
        if c == `\n` or c == termin then
            ;;; end of input
            termin;
        elseif strmember(c, '\\.()') or islowercode(c) then
            ;;; legal item
            consword(c, 1);
        else
            ;;; illegal item
            consstring(c, 1);
        endif;
    enddefine;


;;; testing
    Lambda() =>
    x
;;;    ** [VAR x]

    Lambda() =>
    \x.\y.x
;;;    ** [ABS x [ABS y [VAR x]]]

    Lambda() =>
    (\x.xx)(\x.xx)
;;;    ** [APP [ABS x [APP [VAR x] [VAR x]]] [ABS x [APP [VAR x] [VAR x]]]]
