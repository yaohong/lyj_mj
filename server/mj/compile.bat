echo off

pushd "%~dp0"

::去掉只读
RemoveReadonly.exe ..\lib

::删除ebin的输出目录
rd /s /q .\ebin

set "errorlevel="
escript.exe emake clean > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel

set "errorlevel="
escript.exe emake get-deps > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel

::删除BOM头
RemoveBom.exe .\ *.*

set "errorlevel="
if exist __build.log (
  del __build.log /F /Q
)
escript.exe emake build > __build.log
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel

::成功
goto Success

::失败
:ErrorLabel
echo -----------------------------------------------------------------
echo !!!!!!Error!!!!!!!!!
echo -----------------------------------------------------------------
notepad.exe __build.log
pause

:Success

if exist __build.log (
  del __build.log /F /Q
)

popd


