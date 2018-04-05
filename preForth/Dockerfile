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

copy simpleForth.pre /preForth/simpleForth.pre
copy simpleForth-i386-backend.pre /preForth/simpleForth-i386-backend.pre
copy simpleForth-i386-rts.simple /preForth/simpleForth-i386-rts.simple
copy simpleForth-rts.simple /preForth/simpleForth-rts.simple

copy simpleForthDemo.simple /preForth/simpleForthDemo.simple

run make bootstrap
run make simpleForthDemo
run ./simpleForthDemo

