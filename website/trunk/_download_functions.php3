<?php
class cApplication
{
  var $VersionID;   # Version-ID
  var $ApplTitle;   # Title (gets linked)
  var $Comment;     # Additional small comment (after title-link)
  var $DownloadURL; # download-link
  var $Color;       #

  function cApplication($aColor="?", $aVersionID=0, $aApplTitle="?", $aComment="?", $aDownloadURL="?")
  {
    $this->VersionID   = $aVersionID;
    $this->ApplTitle   = $aApplTitle;
    $this->Comment     = $aComment;
    $this->DownloadURL = $aDownloadURL;
    $this->Color       = $aColor;
  }
}

class cGame
{
  var $FromVersion;  # From which version did QuArK support this game
  var $GameID;       # Game-ID
  var $GameTitle;    # Full game title
  var $GameIcon;     #
  var $GameURL;      #
  var $BuildToolURL; #

  function cGame($aFromVersion="?", $aGameID="", $aGameTitle="?", $aGameIcon="?", $aGameUrl="?", $aBuildToolURL="?")
  {
    $this->FromVersion  = $aFromVersion;
    $this->GameID       = $aGameID;
    $this->GameTitle    = $aGameTitle;
    $this->GameIcon     = "pics/fileicons/".$aGameIcon;
    $this->GameURL      = $aGameUrl;
    $this->BuildToolURL = $aBuildToolURL;
    if ($this->GameID == "")
    {
      $this->GameID = "&nbsp;";
    }
  }
}

function downloadBuildMatrix($Applications, $Games, $PythonMessage)
{
  # Build Table
  $bodytext = "<table border=0 cellpadding=2 cellspacing=0 width=100%>";

  $CntApplications = count($Applications);
  $CntGames = count($Games);

  # Some constants
  $colsgameidwidth = "10%";
  $colsgametitlewidth = "30%";
  $colsappltitlewidth = "40%"; # Sum of gameid+gametitle widths
  $colscolorwidth = "".(15 / $CntApplications)."%"; # Shared 15% - Calculated based on the number of applications.
  $colsbuildtoolswidth = "45%";
  $blankcolor = "#A0A0A0";
  $style = "class=\"text1\"";

  $CurrentRow = 0;

    $bodytext .= "<tr>";
    $bodytext .= "<td ".$style." bgcolor=".$blankcolor." colspan=".($CntApplications+2)." width=".$colsappltitlewidth." align=middle><b>QuArK versions</b></td>";
    $bodytext .= "<td ".$style." bgcolor=".$blankcolor." width=".$colsbuildtoolswidth." align=middle><b>Python</b></td>";
    $bodytext .= "</tr>";

    # Build ApplicationVersions rows
    for ($Appl = 0; $Appl < $CntApplications; $Appl++)
    {
      $bodytext .= "<tr> ";

      {
        for ($ApplCol = 0; $ApplCol < $Appl; $ApplCol++)
        {
          $bodytext .= "<td width=".$colscolorwidth." bgcolor=".$Applications[$ApplCol]->Color.">&nbsp;</td>";
        }

        $bodytext .= "<td ".$style." colspan=".(($CntApplications - $Appl)+2)." bgcolor=".$Applications[$Appl]->Color.">";
        $bodytext .= "<a href=\"".$Applications[$Appl]->DownloadURL."\">";
        $bodytext .= $Applications[$Appl]->ApplTitle;
        $bodytext .= "</a>";
        if ($Applications[$Appl]->Comment != "") {
          $bodytext .= "&nbsp;&nbsp;".$Applications[$Appl]->Comment;
        }
        $bodytext .= "</td>";
      }

      if ($Appl == 0) {
        $bodytext .= "<td ".$style." valign=top bgcolor=#EEEEEE rowspan=".$CntApplications." width=".$colsbuildtoolswidth.">";
        $bodytext .= $PythonMessage;
        $bodytext .= "</td>";
      }

      $bodytext .= "</tr>"; $CurrentRow++;
      $bodytext .= "\n";
    }

    # Blank row
    $bodytext .= "<tr>";
    for ($Appl = 0; $Appl < $CntApplications; $Appl++)
    {
      $bodytext .= "<td width=".$colscolorwidth." bgcolor=".$Applications[$Appl]->Color.">&nbsp;</td>";
    }
    $bodytext .= "<td ".$style." bgcolor=".$blankcolor." align=middle colspan=2><b>Games</b></td>";
    $bodytext .= "<td ".$style." bgcolor=".$blankcolor." width=".$colsbuildtoolswidth." align=middle><b>Build/Compile-tools</b></td>";
    $bodytext .= "</tr>"; $CurrentRow++;


    # Build SupportedGames rows
    $GamesColorArray = array("#C0C0C0", "#D0D0D0");

    for ($Game = 0; $Game < $CntGames; $Game++)
    {
      $bodytext .= "<tr>";

      {
        for ($Appl = 0; $Appl < $CntApplications; $Appl++)
        {
          if ($Games[$Game]->FromVersion <= $Applications[$Appl]->VersionID)
            $bodytext .= "<td width=".$colscolorwidth." bgcolor=".$Applications[$Appl]->Color." align=middle><img src=\"".$Games[$Game]->GameIcon."\" width=16 height=16 border=0></td>";
          else
            $bodytext .= "<td width=".$colscolorwidth.">&nbsp;</td>";
        }
      }

      {
        $bodytext .= "<td ".$style." bgcolor=".$GamesColorArray[$CurrentRow & 1]." width=".$colsgameidwidth." align=middle>";
        $bodytext .= $Games[$Game]->GameID;
        $bodytext .= "</td>";

        $bodytext .= "<td ".$style." bgcolor=".$GamesColorArray[$CurrentRow & 1]." width=".$colsgametitlewidth.">";
        if ($Games[$Game]->GameURL != "") {
          $bodytext .= "<a href=\"".$Games[$Game]->GameURL."\">";
        }
        $bodytext .= $Games[$Game]->GameTitle;
        if ($Games[$Game]->GameURL != "") {
          $bodytext .= "</a>";
        }
        $bodytext .= "</td>";
      }

      {
        $bodytext .= "<td ".$style." bgcolor=".$GamesColorArray[$CurrentRow & 1].">";
        if ($Games[$Game]->BuildToolURL != "") {
          $aPrefix = "";
          $aString = $Games[$Game]->BuildToolURL;
          $aFilename = strtok($aString, "|");
          while ($aFilename) {
            $aURL = strtok("|");
            $bodytext .= $aPrefix."<a href=\"".$aURL."\">".$aFilename."</a>";
            $aFilename = strtok("|");
            $aPrefix = ", ";
          }
        }
        else {
          $bodytext .= "&nbsp;";
        }
        $bodytext .= "</td>";
      }

      $bodytext .= "</tr>"; $CurrentRow++;
      $bodytext .= "\n";
    }

  $bodytext .= "</table>";

  return $bodytext;
}

# ---

class cQRKAddon {
  var $Title;           # Readable title of QRK addon
  var $TitleURL;        # Optional URL for more info of the title
  var $TitleComment;    # Optional more comment-text for title
  var $Filename;        # Filename of the QRK addon
  var $FileDownloadURL; # DownloadURL of the QRK addon
  var $FileComment;     # Optional more comment-text for file

  function cQRKAddon($aTitle="?", $aTitleURL="?", $aTitleComment="?", $aFilename="?", $aFileDownloadURL="?", $aFileComment="?") {
    $this->Title           = $aTitle;
    $this->TitleURL        = $aTitleURL;
    $this->TitleComment    = $aTitleComment;
    $this->Filename        = $aFilename;
    $this->FileDownloadURL = $aFileDownloadURL;
    $this->FileComment     = $aFileComment;
  }
}

function downloadaddonBuildPanel($Games, $GamesQRKAddons)
{
  $bodytext = "";

  $CntGames = count($Games);

  for ($Game = 0; $Game < $CntGames; $Game++)
  {
    if ($Games[$Game]->FromVersion >= 999)
    {
      # No support for this game, yet!
      continue;
    }

    # Setup table header'n'stuff
    $bodytext .= "<a name=\"".$Games[$Game]->GameID."\"></a>";

    $bodytext .= "<table border=0 cellpadding=2 cellspacing=1 width=95% align=center>";

    $bodytext .= "<tr><td class=\"fhh\" width=100% colspan=2>";
    $bodytext .= "&nbsp;<b>".$Games[$Game]->GameTitle."</b>";
    $bodytext .= "</td></tr>";

    $bodytext .= "<tr><td class=\"fh\" width=100% colspan=2><table border=0 cellpadding=6><tr><td class=\"fh\">";
    if ($Games[$Game]->GameURL != "")
    {
      $bodytext .= "<a href=\"".$Games[$Game]->GameURL."\">";
      $bodytext .= "<img border=0 src=\"".$Games[$Game]->GameIcon."\" width=32 height=32>";
      $bodytext .= "</a>";
    }
    else
    {
      $bodytext .= "<img border=0 src=\"".$Games[$Game]->GameIcon."\" width=32 height=32>";
    }
    $bodytext .= "</td><td class=\"fh\">";
    $bodytext .= ""; # optional more header-text
    $bodytext .= "If you find an error or an addon which is outdated, please <a href=\"javascript:mail_decode('qrpxre@cynargdhnxr.pbz?fhowrpg=Nqqba reebe/bhgqngrq');\">tell us</a>.";
    $bodytext .= "</td></tr></table></td></tr>";

    # Loop though addons for this game
    $GameID = $Games[$Game]->GameID;

    if ("array" == gettype($GamesQRKAddons[$GameID])) # Make sure there's an array below the entry...
    {
      $CntAddons = count($GamesQRKAddons[$GameID]);

      for ($Addon = 0; $Addon < $CntAddons; $Addon++)
      {
        $bodytext .= "<tr><td class=\"f\" width=35%>";
        if ($GamesQRKAddons[$GameID][$Addon]->TitleURL != "")
        {
          $bodytext .= "<a href=\"".$GamesQRKAddons[$GameID][$Addon]->TitleURL."\">";
          $bodytext .= $GamesQRKAddons[$GameID][$Addon]->Title;
          $bodytext .= "</a>";
        }
        else
        {
          $bodytext .= $GamesQRKAddons[$GameID][$Addon]->Title;
        }
        if ($GamesQRKAddons[$GameID][$Addon]->TitleComment != "")
        {
          $bodytext .= "<br> - <span class=\"sml\">";
          $bodytext .= $GamesQRKAddons[$GameID][$Addon]->TitleComment;
          $bodytext .= "</span>";
        }
        $bodytext .= "</td>";

        $bodytext .= "<td class=\"f\" width=65%>";
        $bodytext .= "<a href=\"".$GamesQRKAddons[$GameID][$Addon]->FileDownloadURL."\">";
        $bodytext .= $GamesQRKAddons[$GameID][$Addon]->Filename;
        $bodytext .= "</a>";
        if ($GamesQRKAddons[$GameID][$Addon]->FileComment != "")
        {
          $bodytext .= " <span class=\"sml\">";
          $bodytext .= $GamesQRKAddons[$GameID][$Addon]->FileComment;
          $bodytext .= "</span>";
        }
        $bodytext .= "</td></tr>";
      }
    }
    else
    {
      $bodytext .= "<tr><td class=\"f\" width=100% colspan=2 align=middle><span class=\"sml\">";
      $bodytext .= "No add-on(s) registered yet.";
      $bodytext .= "</span></td></tr>";
    }

    # End the table
    $bodytext .= "</table><br><br>";
  }

  return $bodytext;
}
?>
