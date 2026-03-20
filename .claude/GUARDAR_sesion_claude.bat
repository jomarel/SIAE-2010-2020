@echo off
echo Guardando sesion de Claude en Google Drive...
xcopy "%USERPROFILE%\.claude\projects\g--Mi-unidad-SIAE-2010-2020" "G:\Mi unidad\SIAE 2010-2020\.claude-session\g--Mi-unidad-SIAE-2010-2020" /E /I /Y
echo.
echo Listo! Sesion guardada correctamente.
pause
