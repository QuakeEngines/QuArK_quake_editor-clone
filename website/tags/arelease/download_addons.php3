#!/usr/bin/php
<?php
include("_functions.php3");
include("_download_functions.php3");
include("_download-database.php3"); # Contains '$GamesQRKAddons' and '$Games'


function pageLocalDisplay()
{
  pageName("QuArK Addons");

  global $Games, $GamesQRKAddons; # See '_download-database.php3'


$IntroText = "
This page tries to list all the known addons for QuArK, and their download links.
If you ever find an error or some outdated addon, please <a href=\"javascript:mail_decode('qrpxre@cynargdhnxr.pbz');\">tell us</a>,
and we will try to correct it.
<br><br>
Also, if you have made an addon, please <a href=\"javascript:mail_decode('qrpxre@cynargdhnxr.pbz');\">tell us about it</a>, and we will create
a link to your addon from this page. If you need help on creating an addon, please post your question(s) to the
<a href=\"forums.php3\">QuArK forum/mailing-list</a>.
<br><br>
<b>Notice!</b> Those files that have extension '.QRK' or '.qrk', are actually taken directly from the <u>development</u>
source-library, from our project at <a href=\"http://sourceforge.net/project/?group_id=1181\">SourceForge</a>.<br>
<br>";


  $bodytext = downloadaddonBuildPanel($Games, $GamesQRKAddons);

  pagePanel("download", "QuArK Addons", "", $IntroText.$bodytext);
}

pageDisplay("QuArK Addons", 'pageLocalDisplay');
?>
