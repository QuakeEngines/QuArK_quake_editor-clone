#include <stdlib.h>
#include <stdarg.h>

class Logger
{
private:
  FILE* fh;
  int level;
public:
  Logger(const char* name)
  {
    level=40;
    fh=fopen(name,"wb");
    msg(0,"(Start logging)\r\n");
  }
  ~Logger()
  {
    msg(0,"(Stop logging)\r\n");
    fclose(fh);
  }

  void setlevel(int _level)
  {
    level=_level;
    if (level < 0) level=0;
    if (level > 40) level=40;
  }

  void msg( int _level,char const *pMsg, ... )
  {
    if (level < _level) return;
    va_list marker;
    va_start( marker, pMsg );
    vfprintf(fh, pMsg, marker );
    fflush(fh);
    va_end( marker );
  }

};