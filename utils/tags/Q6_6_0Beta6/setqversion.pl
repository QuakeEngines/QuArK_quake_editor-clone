#!/usr/local/bin/perl
# set version string in quark source
# 
# GNU PUBLIC SOFTWARE LICENSE
#
# $Header$
#

if ($#ARGV != 0) {die "USAGE: setqversion.pl <versionstring> \n\n";}

$verstring=$ARGV[0];

## read the file given by argument
sub getfile()
{
  local($fname)=@_;
  local @lines;
  open(FILE,$fname) or die "doesnt open $!";
  @lines=<FILE>;
  close(FILE);
  return \@lines;
}

## write the file given by argument
sub putfile()
{
  local($r_lines,$fname)=@_;
  open(FILE,">$fname") or die "doesnt open $!";
  print FILE @$r_lines;
  close(FILE);
}

## replace string
sub replace()
{
  local($r_lin,$vstring)=@_;

  foreach $li (@$r_lin)
  {
    $li =~ s/^(\s*QuArKVersion\s*=\s*\')([^\']+)\'/$1$vstring\'/;
  }
}

## replace string
sub replace1()
{
  local($r_lin,$vstring)=@_;

  foreach $li (@$r_lin)
  {
    $li =~ s/^(\s*Version\s*=\s*\")([^\"]+)\"/$1$vstring\"/;
  }
}

## replace string
sub replace2()
{
  local($r_lin,$vstring)=@_;

  foreach $li (@$r_lin)
  {
    $li =~ s/^(\s*0:\s*\")([^\"]+)\"/$1$vstring\"/;
  }
}
$r_lin=&getfile("../source/prog/QkObjects.pas");
&replace($r_lin,$verstring);
&putfile($r_lin,"../source/prog/QkObjects.pas");

$r_lin=&getfile("../runtime/addons/Defaults.qrk");
&replace1($r_lin,$verstring);
&putfile($r_lin,"../runtime/addons/Defaults.qrk");

$r_lin=&getfile("../runtime/quarkpy/qdictionnary.py");
&replace2($r_lin,$verstring);
&putfile($r_lin,"../runtime/quarkpy/qdictionnary.py");


################# history ##################################################
#
#
#$Log$
#
#
#
############################################################################