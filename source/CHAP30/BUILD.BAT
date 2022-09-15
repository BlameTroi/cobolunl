@echo off
if '%1'=='' goto USAGE-error
if '%1'=='cgipost' set COBINTFN=cobintfn.obj
if '%1'=='CGIPOST' set COBINTFN=cobintfn.obj
if '%1'=='cgimaint' set COBINTFN=cobintfn.obj
if '%1'=='CGIMAINT' set COBINTFN=cobintfn.obj
if '%1'=='cgissi' set COBINTFN=cobintfn.obj
if '%1'=='CGISSI' set COBINTFN=cobintfn.obj
echo.
echo + Running COBOL against %1.cbl...
cobol %1.cbl,%1.obj USE(CGI_NT.dir) ;
if errorlevel 8 goto COBOL-error
echo + Running CBLLINK against %1.obj...
cbllink  -L -R4.0 -S -o%1.exe %1.obj %COBINTFN%
if errorlevel 8 goto LINK-error
echo.
goto exit
:USAGE-error
echo +++
echo +++ Usage:
echo +++    BUILD <fileroot>
echo +++
echo +++   where <fileroot> is the file name without extension
echo +++   eg. BUILD cgiloc   compiles and links cgiloc.cbl
echo +++
:COBOL-error
echo +++ COBOL against %1.cbl terminated abnormally
goto error
:LINK-error
echo +++ CBLLINK against %1.obj terminated abnormally
goto error
:error
set rc=0
if errorlevel  1 set rc=1
if errorlevel  2 set rc=2
if errorlevel  3 set rc=3
if errorlevel  4 set rc=4
if errorlevel  5 set rc=5
if errorlevel  6 set rc=6
if errorlevel  7 set rc=7
if errorlevel  8 set rc=8
if errorlevel  9 set rc=9
if errorlevel 10 set rc=10
if errorlevel 11 set rc=11
if errorlevel 12 set rc=12
if errorlevel 13 set rc=13
if errorlevel 14 set rc=14
if errorlevel 15 set rc=15
if errorlevel 16 set rc=16
if errorlevel 17 set rc=greater than 16
echo +++ Return code is %rc%.
set rc=
:exit
