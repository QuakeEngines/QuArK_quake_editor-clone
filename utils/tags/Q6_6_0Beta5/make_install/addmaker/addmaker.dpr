program addmaker;

uses
  Classes, Sysutils, Windows;

procedure main;
var
  out_file,cap,mess,defe: String;
  fl,mess_s,pyver,pyfl: integer;
  local: integer;
  fs: TFilestream;
  a: string;
  i: integer;
begin
  i:=1;
  if paramcount=0 then
  begin
    writeln;
    writeln('addmaker (c) 2001 Andy Vincent');
    writeln('------------------------------');
    writeln('valid command line options:');
    writeln('   -caption [caption]');
    writeln('   -message [message]');
    writeln('   -defaultdir [dir]');
    writeln('   -flags [flags]');
    writeln('       Bit 0 (val  1) : if set, user can disable running the command line after extraction (if any)');
    writeln('       Bit 1 (val  2) : if set, user can choose what files to extract');
    writeln('       Bit 2 (val  4) : if set, user cannot change the overwrite-mode (confirm, overwrite, skip)');
    writeln('       Bit3-4(val 8,16) : default-overwrite mode');
    writeln('            0 : confirm overwriting existing files ');
    writeln('            8 : overwrite existing files');
    writeln('            16 : skip existing files');
    writeln('       Bit5 (val  32) : internally used, if set, then do not check file size');
    writeln('       Bit6 (val  64) : if set, then automatically extract all files');
    writeln('       Bit7 (val 128) : if set, don''t show success message ("all files have been extracted")');
    writeln('   -pyver [version]');
    writeln('        ie 151 = 1.5.1');
    writeln('   -pyflags [flags]');
    writeln('        0  = don''t check');
    writeln('        1  = Allow pyver1 = pyver2');
    writeln('        2  = Allow pyver1 > pyver2');
    writeln('        4  = Allow pyver1 < pyver2');
    writeln('   -messagestyle [style]');
    writeln('        1 : messagebox style = MB_ICONINFORMATION ,');
    writeln('            buttons : (ok,cancel ; if cancel is pressed, stop sfx )');
    writeln('        2 : messagebox style = MB_ICONCONFIRMATION ,');
    writeln('            buttons : (yes,no ; if no is pressed, stop sfx )');
    writeln('   -output [filename]');
    exit;
  end;
  while (ParamCount > i) do
  begin
    if paramstr(i)[1]='-' then
    begin
      writeln('param: '+copy(paramstr(i),2,length(paramstr(i))-1)+'='+paramstr(i+1));
      if paramstr(i)='-caption' then
      begin
        cap:=paramstr(i+1);
        inc(i);
      end
      else if paramstr(i)='-message' then
      begin
        mess:=paramstr(i+1);
        inc(i);
      end
      else if paramstr(i)='-defaultdir' then
      begin
        defe:=paramstr(i+1);
        inc(i);
      end
      else if paramstr(i)='-flags' then
      begin
        fl:=strtoint(paramstr(i+1));
        inc(i);
      end
      else if paramstr(i)='-pyver' then
      begin
        pyver:=strtoint(paramstr(i+1));
        inc(i);
      end
      else if paramstr(i)='-pyflags' then
      begin
        pyfl:=strtoint(paramstr(i+1));
        inc(i);
      end
      else if paramstr(i)='-messagestyle' then
      begin
        mess_s:=strtoint(paramstr(i+1));
        inc(i);
      end
      else if paramstr(i)='-output' then
      begin
        out_file:=paramstr(i+1);
        inc(i);
      end
    end;
    inc(i);
  end;
  if out_file='' then exit;
  fs:=TFileStream.Create(out_file, fmCreate);
  a:='MPV';
  fs.WriteBuffer(a[1],3);
  fs.WriteBuffer(fl,1);
  fl:=0; fs.WriteBuffer(fl,1);
  fl:=1; fs.WriteBuffer(fl,1);
  local:=fs.position;
  fl:=0; fs.WriteBuffer(fl,2);
  fl:=length(cap); fs.WriteBuffer(fl,1);
  if fl>0 then
    fs.writebuffer(cap[1], length(cap));
    fl:=length(defe); fs.WriteBuffer(fl,1);
  if fl>0 then
    fs.writebuffer(defe[1], length(defe));
  fl:=0; fs.WriteBuffer(fl,1);
  fl:=length(mess)+1; fs.WriteBuffer(fl,1);
  if fl-1>0 then begin
    fs.WriteBuffer(mess_s,1);
    fs.writebuffer(mess[1], length(mess));
  end;
  fs.WriteBuffer(pyver,1);
  fs.WriteBuffer(pyfl,1);
  fl:=fs.position;
  fs.position:=local;
  fs.writebuffer(fl,2);
  fs.free;
end;

begin
  main;
end.
