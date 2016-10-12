@echo off
python EncoderParser.py --target=utf8nb --dir=".\src"
python EncoderParser.py --target=utf8nb --dir=".\include"
::去除\r
::python EndlineParser.py --target=linux --dir=%server_code_dir%e_dispatch_server\proto\ --filter=proto
pause