
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


__attribute__((__stdcall__)) void grTexSource(int tmu, int startAddress, int evenOdd, grTexInfo_t *info);
__attribute__((__stdcall__)) void softgLoadFrameBuffer(int *buffer, int format);
__attribute__((__stdcall__)) void grDrawTriangle(grVertex_t *a, grVertex_t *b, grVertex_t *c);
__attribute__((__stdcall__)) void grBufferClear(int reserved1, int reserved2, int reserved3);
__attribute__((__stdcall__)) void grTexDownloadTable(int reserved1, int reserved2, void *table);
__attribute__((__stdcall__)) void grGlideInit();
__attribute__((__stdcall__)) void grClipWindow(int left, int top, int right, int bottom);
__attribute__((__stdcall__)) int grSstWinOpen(int reserved1, int reserved2, int reserved3, int reserved4, int reserved5, int reserved6, int reserved7);
__attribute__((__stdcall__)) void grSstWinClose();
__attribute__((__stdcall__)) int softgQuArK();
__attribute__((__stdcall__)) void grConstantColorValue(FxU32 color);
//__attribute__((__stdcall__)) void grFogColorValue(FxU32 color);
__attribute__((__stdcall__)) void guColorCombineFunction(int mode);
__attribute__((__stdcall__)) void grHints(int mode, int value);
