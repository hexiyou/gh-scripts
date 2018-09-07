@echo off
REM @set PATH=%CD%;%CD%\lib;%CD%\bin;%PATH%
set pwd=%~dp0
set pwd=%pwd:~0,-1%
@set PATH=%pwd%;%~dp0lib;%~dp0bin;%PATH%