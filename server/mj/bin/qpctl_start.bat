@echo off

set CURRENT_PATH=%cd%\..\
::echo %CURRENT_PATH%

set MYAPP=qp
set APPNAME=qp
set HOST=127.0.0.1
set NODE= %MYAPP%@%HOST%

set MNESIA_PATH=%CURRENT_PATH%\var\mnesia\%MYAPP%
set PID_DIR_PATH=%CURRENT_PATH%\var\pid
set PID_PATH=%PID_DIR_PATH%\qp.pid
set EBIN_PATH=%CURRENT_PATH%\ebin

set PRIVATE_CFG_PATH=%CURRENT_PATH%\var\config\qp.cfg

set PUBLIC_CFG_PATH=%CURRENT_PATH%\var\config\public.cfg

set LOG_ROTATE_INTERVAL=21600

set LOG_PATH=%CURRENT_PATH%\var\log\%MYAPP%\file_log.log

set LOG_LEVEL=debug

::日志目录
set SASL_LOG_PATH=%CURRENT_PATH%\var\log\%MYAPP%
set OLD_LOG_PATH=%CURRENT_PATH%\old_log\%MYAPP%

if not exist %SASL_LOG_PATH% md %SASL_LOG_PATH%

if not exist %OLD_LOG_PATH% md %OLD_LOG_PATH%

set SASL_LOG_VALUE=%CURRENT_PATH%\var\log\qp\sasl.log

echo %SASL_LOG_VALUE%

::db目录
if not exist %MNESIA_PATH% md %OLD_LOG_PATH%
::pid目录
if not exist %PID_DIR_PATH% md %PID_DIR_PATH%

set SETCOOKIE="qpcookie"
set ERL_PROCESSES=1025000

set ERLANG_OPTS= +K true -smp auto +P %ERL_PROCESSES%

echo "------------------------------"

::echo -name %NODE% %ERLANG_OPTS%  -setcookie %SETCOOKIE% -pa %EBIN_PATH% -s %APPNAME% start -sasl sasl_error_logger {file,%SASL_LOG_VALUE%}

echo "------------------------------"

::echo   -name %MYAPP%_ctl@%HOST%  -setcookie %SETCOOKIE% -pa %EBIN_PATH% -s qp_ctl -extra %NODE% wait_start %APPNAME%

echo "------------------------------"

erl   -name %NODE% %ERLANG_OPTS%  -setcookie %SETCOOKIE% -pa %EBIN_PATH% -s %APPNAME% start -sasl sasl_error_logger false

::erl  -noinput -name %MYAPP%_ctl@%HOST% -setcookie %SETCOOKIE% -pa %EBIN_PATH% -s qp_ctl -extra %NODE% "wait_start" %APPNAME%

pause