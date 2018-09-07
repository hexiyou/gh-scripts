@echo off
call "%~dp0varsset.bat"
set "javapath="
if "x%javapath%" == "x" (
if exist "%ProgramFiles%\Java" (
		set javapath="%ProgramFiles%\Java"
) else (
	if exist "%ProgramFiles(x86)%\Java" (
		set javapath="%ProgramFiles(x86)%\Java"
	) else (
		echo\&cecho    {0C}没有找到Java路径......  Exit{\n#}
		Timeout /t 3||ping -n 3 localhost>nul
		Exit /b 0
	)
)
) else (
	echo\&cecho    {0B}!!!使用手工指定的Java路径 {\n#}
)
echo\
cecho    Already Find JavaPath ====^> %javapath%{\n}


set JDKCMD=dir %%javapath%% /b /A:D^|grep jdk
for /f "usebackq" %%i in (`"%JDKCMD%"`) Do (
	set jdkFolder=%%i
)

set javapath=%javapath:"=%
set fulljdk=%javapath%\%jdkFolder%

cecho    Full JDK Path is:{0A} %fulljdk% {\n#}

::转义反斜杠\
REM set fulljdk=%fulljdk:\=\\%

echo %fulljdk%>%~dp0jdk.txt
if not "x%1"=="xcontinue" pause