#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  global $newsperiod;

  if ($newsperiod) {
    $newsyear = (int)substr($newsperiod, 0, 4);
    $newsmonth = (int)substr($newsperiod, 4, 2);
  } else {
    $newsperiod = mktime(0,0,0,date("m") - 1,1,date("Y"));
    $newsyear = (int)date("Y", $newsperiod);
    $newsmonth = (int)date("m", $newsperiod);
  }

  if ($newsyear && $newsmonth)
  {
    $month = date("F", mktime(0,0,0,$newsmonth,1,$newsyear));
    pageName("Archived News of " . $month . " " . $newsyear);
  }
  else
    pageName("Archived News");

  #
  # The "<font...><p> </p></font>" is for Netscape to _minimize_ the size of a Form's Input buttons!
  #

  $filterpanel = "<form><font size=2><p>Select year and month of archived news: <select name=\"newsperiod\">";

  $year = date("Y");
  $month = date("m");
  do {
    $month--;
    $somedate = mktime(0,0,0,$month,1,$year);
    if (date("Y", $somedate) == $newsyear && date("m", $somedate) == $newsmonth)
      $selected = "selected ";
    else
      $selected = "";
    $filterpanel .= "<option " . $selected . "value=\"" . date("Ym", $somedate) . "\">" . date("Y F", $somedate);
  } while ($somedate > mktime(0,0,0,1,1,1999));

  $filterpanel .= "</select>&nbsp;&nbsp;<input type=\"submit\" value=\"Show me\"></p></font></form>";

  $prevbutton = "";
  if (($newsyear > 1999) || ($newsyear == 1999 && $newsmonth > 1)) {
    $somedate = mktime(0,0,0,$newsmonth - 1,1,$newsyear);
    $prevbutton = "<form><font size=2><p><input type=\"hidden\" name=\"newsperiod\" value=\"" . date("Ym", $somedate) . "\"><input type=\"submit\" value=\"&lt;--- Prev month\"></p></font></form>";
  }
  $nextbutton = "";
  if (($newsyear < date("Y")) || ($newsyear == date("Y") && $newsmonth < date("m")-1)) {
    $somedate = mktime(0,0,0,$newsmonth + 1,1,$newsyear);
    $nextbutton = "<form><font size=2><p><input type=\"hidden\" name=\"newsperiod\" value=\"" . date("Ym", $somedate) . "\"><input type=\"submit\" value=\"Next month ---&gt;\"></p></font></form>";
  }
  $bottombuttons = "<table align=center border=0 cellspacing=0 cellpadding=0><tr><td>".$prevbutton."</td><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td><td>".$nextbutton."</td></tr></table>";

  if (1) {
    $topbuttons = "<table align=center border=0 cellspacing=0 cellpadding=0><tr><td>".$prevbutton."</td><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td><td>".$filterpanel."</td><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td><td>".$nextbutton."</td></tr></table>";
  } else {
    $topbuttons = $filterpanel;
  }

  pagePanel(" ", "Select...", "", $topbuttons);

  if ($newsyear && $newsmonth) {
    displayNews(mktime(0,0,0, $newsmonth+1, 0, $newsyear), mktime(0,0,0, $newsmonth, 1, $newsyear));
    pageSidePanel("", "", $bottombuttons);
  }
}

pageDisplay("Archived News", 'pageLocalDisplay');
?>
