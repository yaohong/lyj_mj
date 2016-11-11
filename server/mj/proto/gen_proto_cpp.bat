echo off

pushd "%~dp0"

set Src=..\..\..\client_lib\client_lib\
set NifSrc=..\nif\
if exist %Src%mj.pb.cc (
  del %Src%mj.pb.cc /F /Q
)

if exist %Src%mj.pb.h (
  del %Src%mj.pb.h /F /Q
)

if exist %Src%common.pb.cc (
  del %Src%common.pb.cc /F /Q
)

if exist %Src%common.pb.h (
  del %Src%common.pb.h /F /Q
)



set "errorlevel="
protoc --cpp_out=%Src% mj.proto > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel
protoc --cpp_out=%Src% common.proto > nul
IF %ERRORLEVEL% NEQ 0 goto ErrorLabel

goto SuccLabel

:ErrorLabel
pause

:SuccLabel
popd
