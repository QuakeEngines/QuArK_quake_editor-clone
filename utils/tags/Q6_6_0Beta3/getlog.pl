#!/usr/local/bin/perl
# rip log messages after given ate from given file
# 
# GNU PUBLIC SOFTWARE LICENSE
#
# $Header$
#

if ($#ARGV != 1) {die "USAGE: getlog.pl <filename> <yyyy/mm/dd>\n       to show log messages after that date\n\n";}

## get date argument
($yy,$mm,$dd)=split(/\//,$ARGV[1]);
#print " $yy $mm $dd \n";

## to terminate sometimes at all :)
$maxremaininglines=5;

## return 0 if date 1 is earlier, else 1
sub cmpdates()
{
  local ($y1,$m1,$d1,$y2,$m2,$d2)=@_;
  local $rc;

  ## megastupid date compare
  if ($y1 * 365 + $m1 * 30 + $d1  > $y2 * 365 + $m2 * 30 + $d2)
  {
    $rc= 1;
  }
  else
  {
    $rc= 0;
  }
#  print "compare @_ = $rc\n";
  return $rc;
}

## open the file given by argument
open(FILE,$ARGV[0]) or die "dont opens $!";
while(<FILE>)
{
  ## get current rev from header tag
  ## 
  if(m/^[\#\s\/]*\$(Header:.+)$/)
  {
    ($hea,$fileident,$rev,$date,$time,$usr,$bla)=split(/\s+/,$1);
#    print "Headerrev=$rev\n";
  }
$logentrycount=0;
  ## start of log entries is detected
  if(m/^[\#\s\/]*\$(Log)/)
  {
#    print "log start $1\n";
    while(<FILE>)
    {
      ## a log entry is etecte
      if(m/^[\#\s\/]*Revision\s+(.+)\s+([\d\/]+) /)
      { 
        $logrev=$1;
        ($cyy,$cmm,$cdd)=split(/\//,$2);

#        print "log entry $2\n";
        #if log entry is earlier > stop it
        if(&cmpdates($cyy,$cmm,$cdd,$yy,$mm,$dd) == 0 )
        {
#          print "################  no more log entries since $yy/$mm/$ ############## \n\n";
          exit(0);
        }
        else ## a newer log entry was found
        {
          ## have we printed the header yet ? 
          if ($logentrycount==0)
          {
            print "################ Log for File $fileident $rev ###############\n";
            
          }
          ## count log entries
          $logentrycount++;
          print ">>>>   Log entry for rev $logrev at $cyy $cmm $cdd <<<<\n";

          ## safety that we print only max 5 lines of last log entry
          $maxremaininglines=5;
        }
      }
      else ## print out text lines
      {
        $maxremaininglines--;
        print $_;
      }
      ## bail out if run out of max lines or comment bracket of delphi
      if (m/^\}/ or $maxremaininglines < 0 )
      {
        exit(0);
      }
    }
  }
}

################# history ##################################################
#
#
#$Log$
#Revision 1.2  2000/06/03 10:53:05  alexander
#protected from cvs using $(Log) and alike
#
#Revision 1.1  2000/06/02 17:07:12  alexander
#initial commit
#
#
#
############################################################################