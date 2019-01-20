@echo off
for %%* in (.) do set CurrDirName=%%~nx*
echo %CurrDirName%

"C:\Program Files\WinRAR\WinRar.exe" a -afzip -x"pack_with_rar.bat" ../%CurrDirName%.zip *.*
