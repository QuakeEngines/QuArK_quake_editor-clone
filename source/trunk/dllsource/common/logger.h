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
    level=20;
    fh=fopen(name,"wb");
    msg(20,"\nStart logging\n");
  }
  ~Logger()
  {
    msg(20,"Stop logging\n");
    fclose(fh);
  }

  void setlevel(int _level)
  {
    level=_level;
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

  void scope( char const *pMsg, ... )
  {
    if (level < 40) return;

    va_list marker;
  	va_start( marker, pMsg );
	  vfprintf(fh, pMsg, marker );
    fflush(fh);
	  va_end( marker );
  }

};