
typedef unsigned char  FxU8;
typedef   signed char  FxI8;
typedef unsigned short FxU16;
typedef   signed short FxI16;
typedef unsigned long  FxU32;
typedef   signed long  FxI32;

typedef struct {
    int smallLod, largeLod;
    int aspectRatio;
    int format;
    char *data;
} grTexInfo_t;

#define GR_TEXFMT_RGB_565  10
#define GR_TEXFMT_RGB_443  222    // custom internal format

typedef struct {
    float sow, tow, oow;
} grTmuVertex_t;

typedef struct {
    float x, y, z;
    float r, g, b;
    float ooz;
    float a;
    float oow;
    grTmuVertex_t tmuvtx[2];
} grVertex_t;


#define EPSILON          (1.0/64)
#define MINWBITS         6
#define MINW             (1<<MINWBITS)
#define MAXW             (65535.0-128.0)
#define OOWTABLEBITS     16
#define OOWTABLESIZE     (1<<OOWTABLEBITS)
#define OOWTABLEBASE     (OOWTABLESIZE<<MINWBITS)
#define MAXOOWBIAS       (int)(OOWTABLEBASE/MAXW)


#ifndef GCC
#include <windows.h>
#ifdef _MSC_VER
#define __attribute__(x)           __declspec(dllexport) WINAPI
#else
#define __attribute__(x)           WINAPI __declspec(dllexport)
#endif
#endif


void __attribute__((__stdcall__)) grTexSource(int tmu, int startAddress, int evenOdd, grTexInfo_t *info);
void __attribute__((__stdcall__)) softgLoadFrameBuffer(int *buffer, int format);
void __attribute__((__stdcall__)) grDrawTriangle(grVertex_t *a, grVertex_t *b, grVertex_t *c);
void __attribute__((__stdcall__)) grBufferClear(int reserved1, int reserved2, int reserved3);
void __attribute__((__stdcall__)) grTexDownloadTable(int reserved1, int reserved2, void *table);
void __attribute__((__stdcall__)) grGlideInit();
void __attribute__((__stdcall__)) grClipWindow(int left, int top, int right, int bottom);
int  __attribute__((__stdcall__)) grSstWinOpen(int reserved1, int reserved2, int reserved3, int reserved4, int reserved5, int reserved6, int reserved7);
void __attribute__((__stdcall__)) grSstWinClose();
int  __attribute__((__stdcall__)) softgQuArK();
void __attribute__((__stdcall__)) grConstantColorValue(FxU32 color);
//void __attribute__((__stdcall__)) grFogColorValue(FxU32 color);
void __attribute__((__stdcall__)) guColorCombineFunction(int mode);
void __attribute__((__stdcall__)) grHints(int mode, int value);
