lcc -O 3d.c
lcclnk -s -O -dll qrksoftg.def 3d.obj
copy qrksoftg.dll ..\runtime\dlls