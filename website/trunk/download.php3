#!/usr/bin/php
<?php
include("_functions.php3");
include("_download_functions.php3");
include("_download-database.php3"); # Contains '$Applications' and '$Games'


function pageLocalDisplay()
{
  pageName("Download");

  global $Applications, $Games; # See '_download-database.php3'


## -- OnlineHelp --
$bodytext = "
<center>
This file contains all the online documentation found on the
<a href=\"http://www.planetquake.com/quark/infobase/index.html\">InfoBase</a> pages.<br>
<br>
<!--<a href=\"http://www.fileplanet.com/dl.aspx?/planetquake/quark/Releases/help_package/quark_help_20030525.zip\">QuArK_Help_20030525.zip</a>-->
<a href=\"http://quark.sourceforge.net/download/quark-help-20030817.zip\">QuArK-Help-20030819.zip</a>
</center>
";

  pagePanel("download", "Download Online-Help", "", $bodytext);


## -- Applications --
$IntroMessage = "
This matrix tries to list which QuArK versions that can support which games (or may in some distant future). Also the needed
build-tools for the specific games are listed, as they are not part of the QuArK-download.
";

$PythonMessage = "
Mini-Python package: <a href=\"http://dl.fileplanet.com/dl/dl.asp?quark/minipy15b.exe\">minipy15b.exe</a>.
Download and install prior to installing QuArK 5.10 or QuArK 6.3.0.<br>
You can also get Python with SDK from <a href=\"http://www.python.org\">www.python.org</a>.
BEWARE though, as only Python v1.5.1 and v1.5.2 works with QuArK 5.10 or QuArK 6.3.0.
";

$OutroMessage = "
<img src=\"pics/Alert.gif\" width=15 height=15 valign=absmiddle>&nbsp;<b>Notice</b>&nbsp;<img src=\"pics/Alert.gif\" width=15 height=15 valign=absmiddle><br>
Medal of Honor:Allied Assault (MOHAA) map-editing is only partially supported in QuArK v6.3.0.<br>
<br>
What this means, is that some of the features the MOHAA game-engine uses, like advanced Level-Of-Detail terrain-facility,
is not provided (as of yet) in the QuArK map-editor. Though you are still able to make basic MOHAA maps and prefabs using the QuArK map-editor.<br>
<!--<br>
To get the MOHAA partial support files for QuArK, go check out the <a href=\"http://www.planetquake.com/quark/latest\">latest-page</a>.
-->
";

  $bodytext = $IntroMessage . "<br><br>" . downloadBuildMatrix($Applications, $Games, $PythonMessage) . "<br>" . $OutroMessage;


  pagePanel("download", "Download Matrix", "", $bodytext);
}

pageDisplay("Download", 'pageLocalDisplay');
?>
