@echo off

IF NOT EXIST ..\build mkdir ..\build
pushd ..\build
cl -MT -Oi -Od -W4 -WX -wd4201 -wd4100 -wd4189 -FC -Z7 -Fm -DHANDMADE_INTERNAL=1 -DHANDMADE_SLOW=1 -DHANDMADE_WIN32=1 ..\..\me_handmadehero\code\win32_handmade.cpp /link -subsystem:windows user32.lib gdi32.lib
popd

REM Compiler options:
REM cl, is cl.exe, is the call for MSVC compiler on Windows
REM -MT, is to compiles to create a multithreaded executable file, by using LIBCMTD.lib (static)
REM -MD, is to compiles to create a multithreaded DLL, by using MSVCRT.lib (dynamic)
REM -Oi, is to generate intrinsic functions
REM -Od, is to disable optimization 
REM -w4, is a warning level 4
REM -wx, is to treat warnings as errors thus failure in compiling unless warning got resolved
REM -wdxxxx, is to ignore a specific warning
REM -FC, is to display the full path of source code files passed to cl.exe in diagnostic text
REM -Z7, is to generate complete debugging information/files except (vc10.pdb) 
REM -D<name>, is to define constants and macros
REM -Fm, is to generate a map file 

REM Link options:
REM /link, is the call for the linker
REM -subsystem:windows, is to tell the operating system to run the executable file .exe in a GUI environment
REM -subsystem:console, is to tell the operating system to run the executable file .exe in a console environment
REM user32.lib gdi32.lib, are libraries to link with

