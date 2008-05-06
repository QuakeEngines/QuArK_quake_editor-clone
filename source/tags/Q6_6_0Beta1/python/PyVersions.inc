
(* Pre-2.0 versions of Python are no longer supported*)
{$DEFINE PYTHON24}

(* Version cascade: *)
{$IFDEF PYTHON24}
{$DEFINE PYTHON25}
{$ENDIF}
{$IFDEF PYTHON23}
{$DEFINE PYTHON22}
{$ENDIF}
{$IFDEF PYTHON22}
{$DEFINE PYTHON21}
{$ENDIF}
{$IFDEF PYTHON21}
{$DEFINE PYTHON20}
{$ENDIF}