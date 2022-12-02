@echo off
if not exist "build" mkdir build
REM use %* to pass on all args that the batch file was called with to cl instead of just the first one with %1
REM example: build.bat lib\MD5.cpp -c to seperately build a object file without linking
REM example: build.bat d5.cpp build\MD5.obj -> to build d5.cpp and link with prev built obj file
cl /EHsc -Z7 -nologo %* -Fo:build\ -Fe:build\