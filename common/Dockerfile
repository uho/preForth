from debian

run dpkg --add-architecture i386

run apt-get -y update && apt-get -y upgrade
run apt-get -y install fasm gforth make gcc:i386

run mkdir preForth
workdir preForth 

copy Makefile /preForth/Makefile
copy preForth.pre /preForth/preForth.pre
copy preForth-i386-backend.pre /preForth/preForth-i386-backend.pre
copy preForth-i386-rts.pre /preForth/preForth-i386-rts.pre
copy preForth-rts.pre /preForth/preForth-rts.pre

copy borrow.fs /preForth/borrow.fs
copy load-preForth.fs /preForth/load-preForth.fs
copy load-i386-preForth.fs /preForth/load-i386-preForth.fs

copy seed /preForth/seed
copy seedForthDemo.seedsource /preForth/seedForthDemo.seedsource
copy seedForth-i386.pre /preForth/seedForth-i386.pre
copy seedForthInteractive.seedsource /preForth/seedForthInteractive.seedsource 
copy seedForth-tokenizer.fs /preForth/seedForth-tokenizer.fs

copy hi.forth /preForth/hi.forth

run make bootstrap
run make


