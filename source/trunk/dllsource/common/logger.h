#include <stdlib.h>
#include <stdarg.h>

class Logger
{
private:
  FILE* fh;
public:
  Logger(const char* name)
  {
    fh=fopen(name,"wb");
    msg("\nStart logging\n");
  }
  ~Logger()
  {
    msg("Stop logging\n");
    fclose(fh);
  }

  void msg( char const *pMsg, ... )
  {
	  va_list marker;
  	va_start( marker, pMsg );
	  vfprintf(fh, pMsg, marker );
    fflush(fh);
	  va_end( marker );
  }

  void scope( char const *pMsg, ... )
  {
    va_list marker;
  	va_start( marker, pMsg );
	  vfprintf(fh, pMsg, marker );
    fflush(fh);
	  va_end( marker );
    }

};