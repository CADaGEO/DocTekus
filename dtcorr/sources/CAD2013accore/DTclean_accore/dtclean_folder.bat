REM @ECHO Off
ECHO DTclean_accore : Debut de nettoyage
FOR /r %%f IN (*.dwg) DO "C:\Program Files\Autodesk\AutoCAD Map 3D 2013\accoreconsole.exe" /i "%%f" /s "D:\cglocal\sync\cg01\SI\Dev\DTclean_accore\dtclean.scr"

pause
