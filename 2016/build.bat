@echo off
REM When you call a batch file, you can enter data after the command that the batch file refers to as %1, %2, etc. For example, in the batch file hello.bat, the following command
REM 
REM @echo hello %1 boy
REM would output
REM 
REM hello john boy
REM if you called it as
REM 
REM hello john
REM The following table outlines how you can modify the passed parameter.
REM 
REM Parameter	Description
REM %1	The normal parameter.
REM %~f1	Expands %1 to a fully qualified pathname. If you passed only a filename from the current directory, this parameter would also expand to the drive or directory.
REM %~d1	Extracts the drive letter from %1.
REM %~p1	Extracts the path from %1.
REM %~s1	Changes the n and x options’ meanings to reference the short name. You would therefore use %~sn1 for the short filename and %~sx1 for the short extension.
REM %~n1	Extracts the filename from %1, without the extension.
REM %~x1	Extracts the file extension from %1.

REM if variable VSINSTALLDIR isnt empty (it is set by vcvarsall.bat) skip calling vcvarsall (using goto)
REM use double quotes around %varname% to escape quotes in path
IF NOT "%VSINSTALLDIR%" == "" GOTO SKIPVCVARS
call "D:\Programs\Visual Studio 2017 Enterprise\VC\Auxiliary\Build\vcvarsall.bat" x64
call "C:\Program Files (x86)\Visual Studio 2017 Enterprise\VC\Auxiliary\Build\vcvarsall.bat" x64

:SKIPVCVARS
if not exist "build" mkdir build
REM use %* to pass on all args that the batch file was called with to cl instead of just the first one with %1
REM example: build.bat lib\MD5.cpp -c to seperately build a object file without linking
REM example: build.bat d5.cpp build\MD5.obj -> to build d5.cpp and link with prev built obj file
cl /EHsc -Z7 -nologo %* -Fo:build\ -Fe:build\
