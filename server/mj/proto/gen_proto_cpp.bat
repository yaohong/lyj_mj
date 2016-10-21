echo off

pushd "%~dp0"

set Src=..\..\..\client_lib\client_lib\

if exist %Src%mj.pb.cc (
  del %Src%mj.pb.cc /F /Q
)

if exist %Src%mj.pb.h (
  del %Src%mj.pb.h /F /Q
)


set "errorlevel="
protoc --cpp_out=%Src% mj.proto > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel


goto SuccLabel

:ErrorLabel
pause

:SuccLabel
popd
