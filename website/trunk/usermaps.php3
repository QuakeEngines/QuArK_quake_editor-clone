#!/usr/bin/php
<?php
include("_functions.php3");
include("_usermaps-database.php3");

$checkboxon = array("on" => "checked "
                   ,"1"  => "checked ");

$gameslist = array(
 "Q1"   => "Quake"
,"Q2"   => "Quake-2"
,"Q3"   => "Quake-3:Arena"
,"HL"   => "Half-Life"
,"HX2"  => "Hexen-II"
,"HR2"  => "Heretic-2"
,"SIN"  => "SiN"
,"KP"   => "Kingpin"
,"SOF"  => "Soldier of Fortune"
,"-"    => "Unknown"
);

$maptypeslist = array(
 "SP"   => "Single Player"
,"DM"   => "Deathmatch"
,"COOP" => "Cooperative"
,"TD"   => "Team Deathmatch"
,"TP"   => "Team Play"
,"CTF"  => "Capture The Flag"
,"RA"   => "Rocket Arena"
,"TFC"  => "Team Fortress (Classic)"
,"ACT"  => "Action (Q2/HL)"
,"CS"   => "Counter-Strike"
,"AIR"  => "AirQuake (Q1/Q2)"
,"GLM"  => "Gloom"
,"OP4"  => "Opposing Force"
,"-"    => "Unknown"
);

$usermapsform = "
<center>
<form>
<table border=0 cellspacing=2 cellpadding=0>
  <tr><td valign=top>
    <table border=0 cellspacing=0 cellpadding=0>
      <tr><td colspan=3 bgcolor=#CFCFCF><font face=\"MS Sans Serif\" size=2><b>&nbsp;Games:</b></font></td></tr>
      <tr><td bgcolor=#C9C9C9 valign=top>
        <font face=\"MS Sans Serif\" size=2>
        <input ".$checkboxon[$Q1] ."type=\"checkbox\" value=\"1\" name=\"Q1\" >Quake-1<br>
        <input ".$checkboxon[$Q2] ."type=\"checkbox\" value=\"1\" name=\"Q2\" >Quake-2<br>
        <input ".$checkboxon[$Q3A]."type=\"checkbox\" value=\"1\" name=\"Q3A\">Quake-3:Arena<br>
        <input ".$checkboxon[$HL] ."type=\"checkbox\" value=\"1\" name=\"HL\" >Half-Life<br>
        <input ".$checkboxon[$HX2]."type=\"checkbox\" value=\"1\" name=\"HX2\">Hexen-II<br>
        <input ".$checkboxon[$HR2]."type=\"checkbox\" value=\"1\" name=\"HR2\">Heretic-2<br>
        <input ".$checkboxon[$SIN]."type=\"checkbox\" value=\"1\" name=\"SIN\">SiN
        </font>
      </td>
      <td bgcolor=#C9C9C9>&nbsp;&nbsp;</td>
      <td bgcolor=#C9C9C9 valign=top>
        <font face=\"MS Sans Serif\" size=2>
        <input ".$checkboxon[$KP] ."type=\"checkbox\" value=\"1\" name=\"KP\" >Kingpin<br>
        <input ".$checkboxon[$SOF]."type=\"checkbox\" value=\"1\" name=\"SOF\">Soldier&nbsp;of&nbsp;Fortune&nbsp;<br>
        <input ".$checkboxon[$ZZZ]."type=\"checkbox\" value=\"1\" name=\"ZZZ\">Unknown
        </font>
      </td></tr>
      <tr><td colspan=3 bgcolor=#C9C9C9 align=center>
        <font face=\"MS Sans Serif\" size=2>
        <input ".$checkboxon[$ZZX]."type=\"checkbox\" value=\"1\" name=\"ZZX\">Just show me for all games!
        </font>
      </td></tr>
    </table>
  </td>
  <td valign=top><font face=\"MS Sans Serif\" size=2>&nbsp;&nbsp;<b>and</b>&nbsp;&nbsp;</font></td>
  <td valign=top>
    <table border=0 cellspacing=0 cellpadding=0>
      <tr><td colspan=3 bgcolor=#CFCFCF><font face=\"MS Sans Serif\" size=2><b>&nbsp;Maptypes:</b></font></td></tr>
      <tr><td bgcolor=#C9C9C9 valign=top>
        <font face=\"MS Sans Serif\" size=2>
        <input ".$checkboxon[$M_SP]  ."type=\"checkbox\" value=\"1\" name=\"M_SP\"  >Single&nbsp;player<br>
        <input ".$checkboxon[$M_COOP]."type=\"checkbox\" value=\"1\" name=\"M_COOP\">Cooperative<br>
        <input ".$checkboxon[$M_DM]  ."type=\"checkbox\" value=\"1\" name=\"M_DM\"  >Deathmatch<br>
        <input ".$checkboxon[$M_TP]  ."type=\"checkbox\" value=\"1\" name=\"M_TP\"  >Teamplay<br>
        <input ".$checkboxon[$M_TD]  ."type=\"checkbox\" value=\"1\" name=\"M_TD\"  >Team&nbsp;deathmatch&nbsp;<br>
        <input ".$checkboxon[$M_CTF] ."type=\"checkbox\" value=\"1\" name=\"M_CTF\" >Capture&nbsp;the&nbsp;Flag<br>
        <input ".$checkboxon[$M_RA]  ."type=\"checkbox\" value=\"1\" name=\"M_RA\"  >Rocket&nbsp;Arena&nbsp;'Q2'
        </font>
      </td>
      <td bgcolor=#C9C9C9>&nbsp;&nbsp;</td>
      <td bgcolor=#C9C9C9 valign=top>
        <font face=\"MS Sans Serif\" size=2>
        <input ".$checkboxon[$M_TFC] ."type=\"checkbox\" value=\"1\" name=\"M_TFC\" >Team&nbsp;Fortress&nbsp;'Q1/HL'&nbsp;<br>
        <input ".$checkboxon[$M_ACT] ."type=\"checkbox\" value=\"1\" name=\"M_ACT\" >Action&nbsp;'Q2/HL'<br>
        <input ".$checkboxon[$M_CS]  ."type=\"checkbox\" value=\"1\" name=\"M_CS\"  >Counter-Strike&nbsp;'HL'<br>
        <input ".$checkboxon[$M_AIR] ."type=\"checkbox\" value=\"1\" name=\"M_AIR\" >AirQuake&nbsp;'Q1/Q2'<br>
        <input ".$checkboxon[$M_GLM] ."type=\"checkbox\" value=\"1\" name=\"M_GLM\" >Gloom&nbsp;'Q2'<br>
        <input ".$checkboxon[$M_OP4] ."type=\"checkbox\" value=\"1\" name=\"M_OP4\" >Opposing&nbsp;Forces&nbsp;'HL'<br>
        <input ".$checkboxon[$M_ZZZ] ."type=\"checkbox\" value=\"1\" name=\"M_ZZZ\" >Unknown
        </font>
      </td></tr>
      <tr><td colspan=3 bgcolor=#C9C9C9 align=center>
        <font face=\"MS Sans Serif\" size=2>
          <input ".$checkboxon[$M_ZZX] ."type=\"checkbox\" value=\"1\" name=\"M_ZZX\">Just show me for all maptypes!
        </font>
      </td></tr>
    </table>
  </td></tr>
  <tr><td colspan=3 align=center>
  <input type=\"submit\" value=\"Show me\">
  </td></tr>
</table>
</form>
Select minimum a game checkbox <u>and</u> a maptype checkbox, to view the list of maps.
</center>
";

# Create a regular-search-exp for game-ids
$games = "";
if ($Q1  || $ZZX) $games .= "|(Q1)";
if ($Q2  || $ZZX) $games .= "|(Q2)";
if ($Q3A || $ZZX) $games .= "|(Q3)";
if ($HL  || $ZZX) $games .= "|(HL)";
if ($HX2 || $ZZX) $games .= "|(HX2)";
if ($HR2 || $ZZX) $games .= "|(HR2)";
if ($SIN || $ZZX) $games .= "|(SIN)";
if ($KP  || $ZZX) $games .= "|(KP)";
if ($SOF || $ZZX) $games .= "|(SOF)";
if ($ZZZ || $ZZX) $games .= "|(\\-)";
if ($games != "") {
  $games = "(" . substr($games, 1) . ")";
}

# Create a regular-search-exp for maptype-ids
$maptypes = "";
if ($M_SP   || $M_ZZX) $maptypes .= "|(SP)";
if ($M_COOP || $M_ZZX) $maptypes .= "|(COOP)";
if ($M_DM   || $M_ZZX) $maptypes .= "|(DM)";
if ($M_TP   || $M_ZZX) $maptypes .= "|(TP)";
if ($M_TD   || $M_ZZX) $maptypes .= "|(TD)";
if ($M_CTF  || $M_ZZX) $maptypes .= "|(CTF)";
if ($M_RA   || $M_ZZX) $maptypes .= "|(RA)";
if ($M_TFC  || $M_ZZX) $maptypes .= "|(TFC)";
if ($M_ACT  || $M_ZZX) $maptypes .= "|(ACT)";
if ($M_CS   || $M_ZZX) $maptypes .= "|(CS)";
if ($M_AIR  || $M_ZZX) $maptypes .= "|(AIR)";
if ($M_GLM  || $M_ZZX) $maptypes .= "|(GLM)";
if ($M_OP4  || $M_ZZX) $maptypes .= "|(OP4)";
if ($M_ZZZ  || $M_ZZX) $maptypes .= "|(\\-)";
if ($maptypes != "") {
  $maptypes = "(" . substr($maptypes, 1) . ")";
}


function pageLocalDisplay() {
  global $usermapsform, $games, $maptypes;
  global $gameslist, $maptypeslist;

  global $userlevelsdatabase;
  global $mapname_arraycol;
  global $download_arraycol;
  global $screenshot_arraycol;
  global $website_arraycol;
  global $emailauthor_arraycol;
  global $size_arraycol;
  global $maptype_arraycol;
  global $game_arraycol;
  global $description_arraycol;

  pageName("User maps");
  echo "<br>";

  pagePanel("", "Filter...", "", $usermapsform);

  if ($games && $maptypes) {
    $nomaps = count($userlevelsdatabase);
    for ($mapno = 1; $mapno < $nomaps; $mapno++) {
      if (ereg($games, $userlevelsdatabase[$mapno][$game_arraycol]) && ereg($maptypes, $userlevelsdatabase[$mapno][$maptype_arraycol])) {
        $bodytext = "";

        {
          $bodytext .= "<table border=0 width=100%><tr><td valign=top>";

          if ($userlevelsdatabase[$mapno][$download_arraycol] != "-")
            $bodytext .= "<a href=\"" . $userlevelsdatabase[$mapno][$download_arraycol] . "\">Download</a><br>";
          else
            $bodytext .= "No download URL given<br>";

          $bodytext .= "Size: " . $userlevelsdatabase[$mapno][$size_arraycol] . "<br>";

          if ($userlevelsdatabase[$mapno][$screenshot_arraycol] != "-")
            $bodytext .= "<a href=\"" . $userlevelsdatabase[$mapno][$screenshot_arraycol] . "\">Screenshot</a><br>";

          $bodytext .= "</td><td valign=top>";

          $tok = strtok($userlevelsdatabase[$mapno][$maptype_arraycol], " ");
          while ($tok) {
            $bodytext .= $maptypeslist[$tok] . "<br>";
            $tok = strtok(" ");
          }

          $bodytext .= "</td><tr></table>";

          $bodytext .= $userlevelsdatabase[$mapno][$description_arraycol];

          $bodytext .= "<br><br><center>";

          $email = substr($userlevelsdatabase[$mapno][$emailauthor_arraycol], 0, strpos($userlevelsdatabase[$mapno][$emailauthor_arraycol], "?"));
          $author = substr($userlevelsdatabase[$mapno][$emailauthor_arraycol], 1 + strpos($userlevelsdatabase[$mapno][$emailauthor_arraycol], "?"));

          $bodytext .= "<a href=\"mailto:***PLEASE@NO*SPAM***" . $email . "\">" . $author . "</a>";

          if ($userlevelsdatabase[$mapno][$website_arraycol] != "-")
            $bodytext .= " - <a href=\"" . $userlevelsdatabase[$mapno][$website_arraycol] . "\">Website</a>";

          $bodytext .= "</center>";
        }

        pagePanel("community", $userlevelsdatabase[$mapno][$mapname_arraycol], $gameslist[$userlevelsdatabase[$mapno][$game_arraycol]], $bodytext);
      }
    }
  }
}

pageDisplay("User maps", 'pageLocalDisplay');
?>
