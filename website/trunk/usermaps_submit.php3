#!/usr/bin/php
<?php
include("_functions.php3");

$submitform = "
<form method=\"POST\" action=\"usermaps_submit.php3\">
  <table border=0 cellpadding=0 cellspacing=1 align=center>
    <tr><td class=\"text1\">Map&nbsp;name:</td><td><input name=\"map_name\" size=50></td></tr>
    <tr><td class=\"text1\">Download&nbsp;URL:</td><td><input name=\"map_download\" size=50 value=\"ftp://\"></td></tr>
    <tr><td class=\"text1\">Screenshot&nbsp;URL:</td><td><input name=\"map_screenshot\" size=50 value=\"http://\"></td></tr>
    <tr><td class=\"text1\">Website&nbsp;URL:</td><td><input name=\"map_website\" size=50 value=\"http://\"></td></tr>
    <tr><td class=\"text1\">E-mail:</td><td><input name=\"map_email\" size=50></td></tr>
    <tr><td class=\"text1\">Map&nbsp;Author:</td><td><input name=\"map_author\" size=50></td></tr>
    <tr><td class=\"text1\">Filesize:</td><td><input name=\"map_filesize\" size=10 value=\"0.0 MB\"></td></tr>
    <tr><td class=\"text1\">Map&nbsp;type:</td><td>
  <select name=\"map_type\">
    <option>Single player
    <option>Cooperative
    <option>Deathmatch
    <option>Team Deathmatch
    <option>Team Play
    <option>Capture The Flag
    <option>Rocket Arena
    <option>Team Fortress (Classic)
    <option>Action (Q2/HL)
    <option>Counter-Strike
    <option>AirQuake (Q1/Q2)
    <option>GLOOM
    <option>(other)
  </select>
  <tr><td class=\"text1\">FPS&nbsp;Game:</td><td>
  <select name=\"map_game\">
    <option>Quake-1
    <option>Quake-2
    <option>Hexen-2
    <option>Heretic-2
    <option>Half-Life
    <option>Sin
    <option>KingPin
    <option>Quake-3:Arena
    <option>(other)
  </select></td></tr>
  <tr><td class=\"text1\" valign=top>Description:</td><td><textarea name=\"map_description\" rows=10 cols=48></textarea></td></tr>
  <tr><td class=\"text1\" valign=top>Comment to webmaster:<br>(if any)</td><td><textarea name=\"map_comment\" rows=2 cols=48></textarea></td></tr>
  </table>
<br>
<center><input type=SUBMIT value=\"Submit\"></center>
</form>
";


function pageLocalDisplay() {
  global $submitform;
  pageName("Submit Usermap");
  pagePanel("community", "Got a new QuArK-made map?", "", $submitform);
}

function pageLocalVerify() {
  global $submitform, $map_name, $map_download, $map_screenshot, $map_website, $map_email, $map_author, $map_filesize, $map_type, $map_game, $map_description, $map_comment;
  pageName("Please verify...");

  $mappaneltext = mapCreateText($map_name, $map_download, $map_screenshot, $map_website, $map_email, $map_author, $map_filesize, $map_type, $map_game, $map_description, $map_comment);

  pagePanel("community", $map_name, "", $mappaneltext);
}

if ($map_name!="")
  pageDisplay("Submit Usermap", 'pageLocalVerify');
else
  pageDisplay("Submit Usermap...", 'pageLocalDisplay');
?>

