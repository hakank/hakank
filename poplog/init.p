/*
   hakank: This is my ~/Poplog/init.p file
*/

;;; Default user startup file for Poplog
;;; Aaron Sloman 17 Sep 2000
;;;pr('greetings from ~/Poplib/init.p\n');
;;; 'greetings from ~/Poplib/init.p'=>;

;;;'hello from ~/Poplib/init.p' ==>
;;; Possinly increase this default? (number is in words, not bytes)
;;; max(popmemlim, 1500000) -> popmemlim;
max(popmemlim, 50000000) -> popmemlim;

;;; from REF system
;;; unless islist(popunderx) do
;;;   [] -> popunderx;
;;;   sysxsetup();
;;;endunless;
 

;;; Initiating GOSPL
;;; 
;;; (I have to start with poplog.sh pop11)
compile( '/home/hakank/poplog/gospl/init.p' );
uses_project pop11
uses_project ved
uses time;
uses teaching;

/*
;;; From ~/poplog/new/emacs/example-init.p
;;; Emacs interface procedures.
;;; If you set the variable 'inferior-pop-initialisation' to nil in your
;;; .emacs file, then the following two procedures must be loaded for the
;;; Emacs/Poplog communication to work properly.

define emacs_match_wordswith(pattern, file); 
    dlocal cucharout=discout(file), poplinewidth = false; 
    
    '( '.pr; 
    applist(match_wordswith(pattern), printf(% '\"%P\" ' %)); 
    ')'.pr; 
    1.nl; 
    termin.cucharout; 
    enddefine;

define emacs_flatten_searchlists(file); 
    dlocal cucharout=discout(file), poplinewidth = false; 
    lvars t, l; 

    '( '.pr; 
    for t l in [help teach ref doc lib], 
	       [^vedhelplist ^vedteachlist ^vedreflist 
		^veddoclist ^popuseslist] do
        printf(t, '( \"%P\" '); 
	applist(flatten_searchlist(l), printf(%'\"%P\" '%)); 
	')'.pr;
	endfor;
 
    ' )'.pr; 
    1.nl; 
    termin.cucharout; 
    enddefine;

;;; Other things that you may find useful ...

;;; We use an alternative version of prwarning when running under Emacs.
;;; When compiling an Emacs buffer, the value of popfilename is the
;;; temporary file used for inter-process communication.
define emacs_prwarning(word);
    ;;; popfilename is false when the compiler is reading standard input?
    if popfilename then
	if issubstring('/tmp/emacs', popfilename) then
    	    printf(';;; DECLARING VARIABLE %p line %p\n', [^word ^poplinenum]);
    	else
    	    printf(';;; DECLARING VARIABLE %p in file %p line %p\n',
               	   [^word ^popfilename ^poplinenum]);
	    endif;
    else
	sysprwarning(word);
	endif;
    enddefine;
emacs_prwarning -> prwarning;

;;;
;;; end emacs things, from example-init.p
;;;
*/

vars
	popmaxtraceindent = 30,		;;; limits depth of tracing
	pop_mishap_doing_lim = 30,	;;; limits depth of callstack in error messages
;

;;; Uncomment this section if you have a poplib directory for autoloading

;;; add owner's poplib directory to popautolist (and therefore popuseslist)
lvars ownerlib=sysfileok('~/Poplib');

if sys_file_exists(ownerlib) then

	unless member(ownerlib,popautolist) then
		;;; May have been set up in a saved image
		[^ownerlib ^^popautolist] -> popautolist;
	endunless;

endif;

;;; ['/home/hakank/poplog/me' ^^popautolist] -> popautolist;

;;; Added this 2011-12-03
;;;; ['/home/hakank/poplog/current_poplog/pop/current-poplog/pop/packages/teaching/lib' ^^popautolist] -> popautolist;

;;; Added this 2012-01-01
['/home/hakank/poplog/poplog16/poplog_base/pop/packages/teaching/lib' ^^popautolist] -> popautolist;

;;; [adding . to popautolist]=>;
['.' ^^popautolist] -> popautolist;

;;; From 
;;; http://mailman.cs.bham.ac.uk/archives/pop-forum/2007q4/000125.html
;;; makes these characters as a character.
;;;
lvars i;
for i from 1 to 6 do 1->item_chartype('åäöÅÄÖ'(i)) endfor;

vars initfiledone = true;

