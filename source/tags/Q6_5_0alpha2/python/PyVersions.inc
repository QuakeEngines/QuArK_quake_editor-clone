
(* Pre-2.0 versions of Python are no longer supported*)
{$DEFINE PYTHON20}

(* If PYTHON_SDK is not set, sets PYTHON_BUNDLED and PYTHON22_OR_HIGHER *)
{$IFNDEF PYTHON_SDK}
{$DEFINE PYTHON_BUNDLED}
{$DEFINE PYTHON22}
{$ENDIF}

(* Version cascade: *)
{$IFDEF PYTHON23}
{$DEFINE PYTHON22}
{$ENDIF}
{$IFDEF PYTHON22}
{$DEFINE PYTHON21}
{$ENDIF}
