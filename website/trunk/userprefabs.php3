#!/usr/bin/php
<?php
include("_functions.php3");
include("_userprefabs-database.php3");

function pageLocalDisplay() {
  global $prefabs_imageroot;
  global $prefabs_fileroot;
  global $prefabs_headers;
  global $prefabs_database;

  global $added_arraycol;
  global $updated_arraycol;
  global $image_arraycol;
  global $file_arraycol;
  global $author_arraycol;
  global $email_arraycol;

  pageName("User Prefabs");
  pagePanelFile("community", "Notice!", "", "userprefabs.html");

  $panel = "";

  $games = count($prefabs_database);

  # Quick links...
  {
    $panel .= "<table border=0 cellpadding=2 cellspacing=1 width=95% align=center>";
    $panel .= "<tr><td class=\"f\" valign=top width=20%>Quick links to page-sections:</td>";

    $g = 0;
    $gsplit = ($games / 2);
    while ($g < $games)
    {
      $panel .= "<td class=\"f\" valign=top width=40%>";

      while ($g < $gsplit)
      {
        $game     = $prefabs_database[$g][0];
        $files    = count($prefabs_database[$g]) - 1;
        $gamename = $prefabs_headers[$game][1];

        $panel .= "<a href=\"#".$game."\">".$gamename."</a> <span class=\"sml\">(".$files." files)</span><br>";

        $g++;
      }

      $panel .= "</td>";

      $gsplit += ($games / 2);
    }

    $panel .= "</tr></table><br>";
  }

  for ($g = 0; $g < $games; $g++)
  {
    $game     = $prefabs_database[$g][0];

    $folder   = $prefabs_headers[$game][0];
    $gamename = $prefabs_headers[$game][1];
    $gameicon = $prefabs_headers[$game][2];

    $panel .= "\n<a name=\"".$game."\"></a>";
    $panel .= "<table border=0 cellpadding=2 cellspacing=1 width=95% align=center>";
    $panel .= "<tr><td class=\"fhh\" width=100% colspan=3>";
    $panel .= "&nbsp;<b>".$gamename."</b>";
    $panel .= "</td></tr>";

    $panel .= "<tr><td class=\"fh\" width=100% colspan=3>";

    $panel .= "<table border=0 cellpadding=6><tr><td class=\"fh\">";
    $panel .= "<img border=0 src=\"".$gameicon."\" width=32 height=32 align=absmiddle>";
    $panel .= "</td><td class=\"fh\">";
    $panel .= "Click <a href=\"mailto:***PLEASE@NO*SPAM***decker_dk%40hotmail.com?subject=QuArK User Prefab ".$gamename."\">here to submit</a> a user prefab. <span class=\"sml\">(E-mail, including image in 160x120 pixels, max 100KB zipped.)</span>";
    $panel .= "</td></tr></table>";

    $panel .= "</td></tr>";

    $column = 0;
    $prefabs = count($prefabs_database[$g]);
    for ($p = 1; $p < $prefabs; $p++)
    {
      if ($column % 3 == 0)
      {
        $panel .= "<tr>";
      }
      $column++;

      $prefab = $prefabs_database[$g][$p];

      if ($p == $prefabs)
      {
      }
      else
      {
        $panel .= "<td class=\"f\" width=33% valign=top>";
        $panel .= "<a href=\"".$prefabs_fileroot.$folder.$prefab[$file_arraycol]."\">";
        $panel .= "<img border=0 src=\"".$prefabs_imageroot.$folder.$prefab[$image_arraycol]."\" width=160 height=120 alt=\"".$prefab[$file_arraycol]."\">";
        $panel .= "</a>";
        $panel .= "<br>By: <a href=\"mailto:***PLEASE@NO*SPAM***".$prefab[$email_arraycol]."\">".$prefab[$author_arraycol]."</a>";
        if ($prefab[$updated_arraycol] != "")
        {
          $panel .= "<br>Updated: ".$prefab[$updated_arraycol];
        }
        $panel .= "<br>Added: ".$prefab[$added_arraycol];
        $panel .= "<br>Download: <a href=\"".$prefabs_fileroot.$folder.$prefab[$file_arraycol]."\">".$prefab[$file_arraycol]."</a>";
        $panel .= "</td>";
      }

      if ($column % 3 == 0)
      {
        $panel .= "</tr>";
      }
    }
    if ($column > 0 && ($column % 3 != 0))
    {
      while ($column % 3 != 0)
      {
        $panel .= "<td class=\"f\" width=33% valign=top>&nbsp;</td>";
        $column++;
      }
      $panel .= "</tr>";
    }

    $panel .= "</table><br>";
  }

  pagePanel("community", "User Prefabs", "", $panel);
}

pageDisplay("User Prefabs", 'pageLocalDisplay');
?>
