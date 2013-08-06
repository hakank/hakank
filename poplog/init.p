/*
   hakank: This is my ~/Poplog/init.p file
*/

;;; Default user startup file for Poplog
;;; Aaron Sloman 17 Sep 2000
;;; pr('greetings from ~/Poplib/init.p\n');

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
compile( '/home/hakank/poplog/gospl/init.p' );
uses_project pop11
uses_project ved
uses time;


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

;;; From 
;;; http://mailman.cs.bham.ac.uk/archives/pop-forum/2007q4/000125.html
;;; makes these national (Swedish) characters as a character.
;;;
lvars i;
for i from 1 to 6 do 1->item_chartype('åäöÅÄÖ'(i)) endfor;

vars initfiledone = true;
