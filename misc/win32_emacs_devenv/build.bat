@echo off

IF NOT EXIST ..\build mkdir ..\build
pushd ..\build
cl -FC -Zi ..\..\me_handmadehero\code\win32_handmade.cpp user32.lib gdi32.lib
popd
