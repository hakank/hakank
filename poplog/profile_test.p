compile('/home/hakank/Poplib/init.p');
define example();
        define procB();
            repeat 1e8 times endrepeat;
        enddefine;
        define procA();
            repeat 1e8 times endrepeat;
            procB();
        enddefine;
        procA();
enddefine;

uses profile;

profile example();

