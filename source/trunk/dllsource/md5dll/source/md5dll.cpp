// MD5 DLL for Game Maker 0.6

#include <windows.h>
#include "MD5.h"

#define gm_export extern "C" __declspec (dllexport)

gm_export LPSTR GetFileMd5(LPSTR szFileName) {
	return MD5File(szFileName);
}

gm_export double GetBenchmarkMd5(LPSTR szFileName) {
	double before;
	before = GetTickCount();
	MD5File(szFileName);
	return GetTickCount() - before;
}

gm_export LPSTR GetStringMd5(LPSTR szString) {
	return MD5String(szString);
}

// TickCount rules. Theres no way you to get two same MD5 straight
gm_export LPSTR GetRandomMd5() {
	char lTickCount[1024];
	wsprintf(lTickCount, "%d", GetTickCount());
	return MD5String(lTickCount);
}
