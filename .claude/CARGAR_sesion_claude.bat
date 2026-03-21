@echo off
echo Cargando sesion de Claude desde Google Drive...
xcopy "G:\Mi unidad\SIAE 2010-2020\.claude-session\g--Mi-unidad-SIAE-2010-2020" "%USERPROFILE%\.claude\projects\g--Mi-unidad-SIAE-2010-2020" /E /I /Y
echo.
echo Listo! Sesion cargada. Ya puedes abrir VS Code.
pause

