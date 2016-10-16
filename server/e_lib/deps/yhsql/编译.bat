@echo off
escript.exe emake clean
escript.exe emake get-deps
escript.exe emake build
pause