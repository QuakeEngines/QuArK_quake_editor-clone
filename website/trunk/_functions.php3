<?php

$local = file_exists("local.txt");

if ($local)
  $picsroot = "http://192.168.1.31/";
else
  $picsroot = "http://dynamic.gamespy.com/~quark/";

$keepinframe = ""; # " target=\"bodyframe\" ";
$sidebarwidth = 150;

$iconimgs = array("news"      => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/news.gif\" align=absmiddle>"
                 ,"community" => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/community.gif\" align=absmiddle>"
                 ,"download"  => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/download.gif\" align=absmiddle>"
                 ,"beta"      => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/beta.gif\" align=absmiddle>"
                 ,"documents" => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/documents.gif\" align=absmiddle>"
                 ,"features"  => "<img border=0 witdh=16 height=16 src=\"".$picsroot."pics/icons/features.gif\" align=absmiddle>"

                 ,"downright" => "<img border=0 width=12 height=16 src=\"".$picsroot."pics/icons/downright.gif\">"
                 ,"down"      => "<img border=0 width=12 height=16 src=\"".$picsroot."pics/icons/down.gif\">"
                 ,"right"     => "<img border=0 width=12 height=16 src=\"".$picsroot."pics/icons/right.gif\">"
                 ,"none"      => "<img border=0 width=12 height=16 src=\"".$picsroot."pics/icons/none.gif\">"

                 ,"smiley_happy"  => "<img border=0 witdh=15 height=15 src=\"".$picsroot."pics/icons/smiley_happy.gif\" align=absmiddle>"
                 ,"smiley_medium" => "<img border=0 witdh=15 height=15 src=\"".$picsroot."pics/icons/smiley_medium.gif\" align=absmiddle>"
                 ,"smiley_sad"    => "<img border=0 witdh=15 height=15 src=\"".$picsroot."pics/icons/smiley_sad.gif\" align=absmiddle>"

                 ,"background"    => $picsroot."pics/back.gif"
                 ,"quarklogo"     => $picsroot."pics/quark_logo2.gif"
                 ,"black"         => $picsroot."pics/black.gif"
            );
$countsidepanels = 0;

function initSidePanel() {
  global $countsidepanels;
  $countsidepanels = 0;
}

function pageBegin($title="") {
  global $iconimgs;
  global $local;
  global $NOAD;

#  echo "<!--INSERTADTHISPAGE -->\n";

  echo "<html>";
  if ($title != "")
    $title = " - " . $title;
  echo "<head><title>The Official QuArK website" . $title . "</title>";

  echo "\n<STYLE TYPE=\"text/css\">";
  echo "\nFORM { margin:0% 0% 0% 0%; }";
  echo "\nINPUT { font-family:MS Sans Serif; font-size:8pt; }";
  echo "\nSELECT { font-family:MS Sans Serif; font-size:8pt; }";
  echo "\n.sml { font-size:6pt; }";
  echo "\n.xsml { font-size:3pt; }";
  echo "\n.large { font-size:14pt; }";
  echo "\n.n  { font-family:MS Sans Serif; font-size:10pt; }";
  echo "\nTD.fn  { font-family:MS Sans Serif; font-size:10pt; background:#AAAAAA; }";
  echo "\nTD.f   { font-family:MS Sans Serif; font-size:10pt; background:#CCCCCC; }";
  echo "\nTD.fh  { font-family:MS Sans Serif; font-size:10pt; background:#EEEEEE; }";
  echo "\nTD.fhh { font-family:MS Sans Serif; font-size:10pt; background:#333366; color:FFFFFF; }";
  echo "\nTD.font8 { font-family:MS Sans Serif; font-size:8pt; }";
  echo "\nTR.edge1 { background:#000000; }";
  echo "\nTR.back1 { background:#DDDDDD; }";
  echo "\nTD.text1 { font-family:MS Sans Serif; font-size:10pt; color:#000000; }";
  echo "\nTR.edge2 { background:#114411; }";
  echo "\nTR.head2 { background:#2222AA; }";
  echo "\nTD.text2 { color:#FFFFFF; }";
  echo "\nTR.edge3 { background:#000000; }";
  echo "\nTR.head3 { background:#000077; }";
  echo "\nTD.text3 { font-family:MS Sans Serif; font-size:10pt; color:#FFFFFF; }";
  echo "\n</STYLE>";

  # -- Javascript to decode ROT13 mailto:-addresses. A way to reduce spam (hopefully) --
  # -- Use like this in HTML: <a href="javascript:mail_decode('<a ROT13 mail-address>');">write me</a>
  echo "<script language='javascript'>function mail_decode(codedmailadr){decodedmailadr='to'+':';for(i=0;i<codedmailadr.length;i++){chr=codedmailadr.substr(i,1);idx='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'.indexOf(chr);if(idx>-1){chr='nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM'.substr(idx,1);}decodedmailadr=decodedmailadr+chr;}window.open('mail'+decodedmailadr,'email_protection_decoder','resizable=1,width=100,height=100');}</script>";

  echo "</head>";

  echo "\n<body link=#990000 vlink=#550000 leftmargin=\"0\" topmargin=\"0\" marginwidth=\"0\" marginheight=\"0\" background=\"" . $iconimgs["background"] . "\">";
  echo "<base target=\"_top\">";

  # -- begin: visible page header. QuArK "logo"-image and required PlanetQuake AdBanner code --
  echo "<table border=0 cellspacing=0 cellpadding=0 width=100%>";
  echo "<tr height=65><td width=320>";
  echo "<a href=\"http://www.planetquake.com/quark\" title=\"The official QuArK web-site: www.planetquake.com/quark\">";
  echo "<img src=\"" . $iconimgs["quarklogo"] . "\" border=0 width=320 height=65>";
  echo "</a>";
  echo "</td><td width=100% align=right valign=middle>";

  if (!$local && !$NOAD)
    include("planetquake_adbanner_code.html");

  echo "</td><td width=8>&nbsp;</td></tr>";
  echo "</table>";

  echo "<img src=\"" . $iconimgs["black"] . "\" height=2 width=100%><br>";
  # -- end: visible page header --
}

function pageEnd() {
  global $iconimgs;

  # -- visible page footer --
  echo "<img src=\"" . $iconimgs["black"] . "\" height=2 width=100%><br>";

  echo "</body>";
  echo "</html>";
}

function pageName($name) {
  $pagenametra = " bgcolor=#114411";
  $pagenametrb = " bgcolor=#2222AA";
  $pagenamefontb = "<font face=\"Arial Black, Arial\" size=+3 color=#FFFFFF>";
  $pagenamefonte = "</font>";

  echo "\n<table border=0 align=center cellspacing=0 cellpadding=2 width=100%>";
  echo "<tr" . $pagenametra . "><td width=100%>";
  echo "<table border=0 cellspacing=0 cellpadding=0 width=100% align=center>";
  echo "<tr" . $pagenametrb . "><td>";
  echo $pagenamefontb;

  $name = str_replace(" ", "&nbsp;", $name);

  echo "&nbsp;" . $name . "...&nbsp;";

  echo $pagenamefonte;
  echo "<td></tr>";
  echo "</table>";
  echo "</td></tr>";
  echo "</table>";
}

function pageFixLinkDirection($text) {
  global $picsroot, $keepinframe;

  # Replace all relative references in 'href="help/' to absolute
  $replacer = "href=\"http://www.planetquake.com/quark/\\2";
  $text = eregi_replace("(href=\")(help/)", $replacer, $text);

  # Replace all relative references in 'src=' to absolute
  $replacer = "src=\"".$picsroot."\\2";
  $text = eregi_replace("(src=\")([^h][^t][^t][^p])", $replacer, $text);

  # Replace all local '.shtml' references to '.php3'
  $searcher = "(href=\"[a-zA-Z0-9_/\\]*)\.shtml";
  $replacer = $keepinframe . "\\1.php3";
  $text = eregi_replace($searcher, $replacer, $text);

  return $text;
}

function pagePanel($icon, $headline1, $headline2, $bodytext) {
  global $iconimgs, $picsroot;
  $headtr = " bgcolor=#000077";
  $headfontb = "<font face=\"MS Sans Serif\" size=2 color=#FFFFFF><b>";
  $headfonte = "</b></font>";
  $bodytr = " bgcolor=#DDDDDD";
  $bodyfontb = "<font face=\"MS Sans Serif\" size=2 color=#000000>";
  $bodyfonte = "</font>";

  if ($icon != "")
    echo "<br>";
  echo "\n<table border=0 align=center cellspacing=0 cellpadding=0 width=100%>";
  echo "<tr><td bgcolor=#000000 width=100%>";
  echo "<table border=0 cellspacing=0 cellpadding=1 width=100%>";

  if ($headline2 != "")
    echo "<tr" . $headtr . "><td width=70%>";
  else
    echo "<tr" . $headtr . "><td width=100%>";
  echo $headfontb;

  if ($icon != "")
    echo "&nbsp;", $iconimgs[$icon];
  echo "&nbsp;", $headline1;

  if ($headline2 != "") {
    echo $headfonte;
    echo "</td><td width=30% align=right>";
    echo $headfontb;

    echo $headline2 . "&nbsp;";
  }

  echo $headfonte;
  echo "</td></tr>";

  echo "<tr bgcolor=#000000>";
  if ($headline2 != "")
    echo "<td colspan=2>";
  else
    echo "<td>";
  echo "<table border=0 cellpadding=4 cellspacing=0 width=100%>";
  echo "<tr" . $bodytr . "><td width=4>&nbsp;</td><td width=100%>";
  echo $bodyfontb;

  $bodytext = pageFixLinkDirection($bodytext);

  echo $bodytext;

  echo $bodyfonte;
  echo "</td><td width=4>&nbsp;</td></tr></table>";

  echo "</td></tr></table>";
  echo "</td></tr></table>";
}

function pageRawFile($filename) {
  global $iconimgs, $picsroot;

  $thefile = file($filename);
  $bodytext = "";
  while (list($key, $line) = each($thefile)) {
    $bodytext .= $line;
  }

  $bodytext = pageFixLinkDirection($bodytext);

  echo $bodytext;
}

function pagePanelFile($icon, $headline1, $headline2, $filename) {
  $thefile = file($filename);
  $bodytext = "";
  while (list($key, $line) = each($thefile)) {
    $bodytext .= $line;
  }
  #$bodytext = pageFixLinkDirection($bodytext);

  pagePanel($icon, $headline1, $headline2, $bodytext);
}

function pageSidePanel($icon, $headline1, $bodytext) {
  global $iconimgs;
  global $countsidepanels;

  $headtr = " bgcolor=#003355";
  $headfontb = "<font face=\"MS Sans Serif\" size=1 color=#FFFFFF><b>";
  $headfonte = "</b></font>";
  $bodytr = " bgcolor=#BBBBBB";
  $bodyfontb = "<font face=\"MS Sans Serif\" size=1 color=#000000>";
  $bodyfonte = "</font>";

  if ($countsidepanels > 0)
    echo "<font size=-3><br></font>";
  $countsidepanels++;

  echo "\n<table border=0 align=center width=130 cellspacing=0 cellpadding=0>";
  echo "<tr><td bgcolor=#000000 width=100%>";
  echo "<table border=0 cellspacing=0 cellpadding=1 width=100%>";

  if ($headline1 != "") {
    echo "<tr" . $headtr . "><td width=100%>";
    echo $headfontb;

    if ($icon != "")
      echo "&nbsp;" . $iconimgs[$icon];

    echo "&nbsp;" . $headline1;

    echo $headfonte;
    echo "</td></tr>";
  }

  echo "<tr bgcolor=#000000><td>";
  echo "<table border=0 cellpadding=4 cellspacing=0 width=100%>";
  echo "<tr" . $bodytr ."><td width=120 n_owrap>";
  echo $bodyfontb;

  $bodytext = pageFixLinkDirection($bodytext);

  echo $bodytext;

  echo $bodyfonte;
  echo "</td></tr></table>";

  echo "</td></tr></table>";
  echo "</td></tr></table>";
}

function stringNavbar($navbararray) {
  global $iconimgs, $keepinframe;

  $indent_arraycol = 0;
  $title_arraycol  = 1;
  $icon_arraycol   = 2;
  $url_arraycol    = 3;

  $navbarlines = count($navbararray);
  $navbarindenticons = array("none","none","none","none","none","none","none","none","none");

  # Find the maximum indent level
  $maxidentlevels = 0;
  for ($navbarline=0; $navbarline < $navbarlines; $navbarline++) {
    if ($navbararray[$navbarline][$indent_arraycol] >= $maxidentlevels)
      $maxidentlevels = $navbararray[$navbarline][$indent_arraycol] + 1;
  }

  $navbartext = "<table border=0 cellpadding=0 cellspacing=0 width=100%>";
  for ($navbarline=0; $navbarline < $navbarlines; $navbarline++) {
    $navbartext .= "<tr>";

    if ($navbararray[$navbarline][$indent_arraycol] > 0) {
      # Figure out which indent-icons to use
      for ($indent=0; $indent < $navbararray[$navbarline][$indent_arraycol] - 1; $indent++) {
        $navbartext .= "<td>" . $iconimgs[$navbarindenticons[$indent]]  . "</td>";
      }

      $indenticon = "right";
      $nextindenticon = "none";
      $tmpline = $navbarline + 1;
      while ($tmpline < $navbarlines) {
        if ($navbararray[$tmpline][$indent_arraycol] == $navbararray[$navbarline][$indent_arraycol]) {
          $indenticon = "downright";
          $nextindenticon = "down";
          break;
        }
        if ($navbararray[$tmpline][$indent_arraycol] < $navbararray[$navbarline][$indent_arraycol])
          break;
        $tmpline++;
      }
      $navbartext .= "<td>" . $iconimgs[$indenticon]  . "</td>";

      $navbarindenticons[$navbararray[$navbarline][$indent_arraycol] - 1] = $nextindenticon;
    }

    $colspan = $maxidentlevels - $navbararray[$navbarline][$indent_arraycol];
    $navbartext .= "<td colspan=" . $colspan . ">";

    $prefix = "<font face=\"MS Sans Serif\" size=1>";
    $postfix = "</font>";

    if ($navbararray[$navbarline][$icon_arraycol] != "")
      $prefix = $prefix . "&nbsp;" . $iconimgs[$navbararray[$navbarline][$icon_arraycol]] . "&nbsp;";
    if ($navbararray[$navbarline][$url_arraycol] != "") {
      $prefix = $prefix . "<a " . $keepinframe . "href=\"" . $navbararray[$navbarline][$url_arraycol] . "\">";
      $postfix = "</a>" . $postfix;
    }
    if ($navbararray[$navbarline][$indent_arraycol] < 2) {
      $prefix = $prefix . "<b>";
      $postfix = "</b>" . $postfix;
    }
    $navbartext .= $prefix . $navbararray[$navbarline][$title_arraycol] . $postfix;

    $navbartext .= "</td></tr>";
  }
  $navbartext .= "</table>";
  return $navbartext;
}

function displayNews($fromdate=0, $todate=0) {
  if ($fromdate == 0) {
    $year = date("Y");
    $month = date("m");
    $day = date("d");
    $todate = 0;
  } else {
    $year = date("Y", $fromdate);
    $month = date("m", $fromdate);
    $day = date("d", $fromdate);
    if ($todate > $fromdate)
      $todate = mktime(0,0,0, $month, $day - 1, $year);
    elseif ($todate == 0)
      $todate = mktime(0,0,0, $month - 1, $day, $year);
  }
  $backdays = 0;
  $countarticles = 0;
  $headlinecount = 0;

  $date = mktime(0,0,0, $month, $day, $year);
  while ($backdays < 365 && (($todate == 0 && ($countarticles < 7 || $backdays < 32)) || ($todate > 0 && $date > $todate))) {
    $date = mktime(0,0,0, $month, $day - $backdays, $year);
    $datenewsfile = "news/news" . date("Ymd", $date) . ".txt";

    $backdays++;

    # Read the news from files like "news/newsYYYYMMDD.txt"
    if (file_exists($datenewsfile)) {
      $countarticles++;
      $thefile = file($datenewsfile);

      # Do some formatting, to present the news
      $newsdate = date("Y.m.d", $date);

# [Decker 2001.03.26] Seems there's a problem calculating the correct number-of-days-ago today??!
      $newsago = "";
#     $daysago = (int)((mktime(0,0,0,date("m"),date("d"),date("Y")) - $date) / 86400);
#     $newsago = "<font face=\"ms sans serif\" size=-2 color=#AAAAAA>(";
#     if ($daysago > 365)
#     {
#       $yearsago = (int)($daysago / 365);
#       if ($yearsago != 1)
#         $newsago .= $yearsago . "&nbsp;years&nbsp;ago";
#       else
#         $newsago .= $yearsago . "&nbsp;year&nbsp;ago";
#     }
#     elseif ($daysago > 60)
#       $newsago .= (int)($daysago / 30.5) . "&nbsp;months&nbsp;ago";
#     elseif ($daysago > 1)
#       $newsago .= $daysago . "&nbsp;days&nbsp;ago";
#     elseif ($daysago > 0)
#       $newsago .= $daysago . "&nbsp;day&nbsp;ago";
#     elseif ($daysago == 0)
#       $newsago .= "today";
#     else
#       $newsago .= "timewarp";
#     $newsago .= ")</font>";

      # Figure out, if there is a "<author>xxxx</author>" in the first line.
      list($key, $author) = each($thefile);
      $lowauthor = strtolower($author);
      $start = strpos($lowauthor, "<author>");
      $stop = strpos($lowauthor, "</author>");
      if ($start || $stop)
        $newsauthor = substr($author, $start + 8, $stop - $start + 8);
      else {
        # No author?
        $newsauthor = "(Unknown author)";
        reset($thefile);
      }

      $headtxt = $newsdate . "&nbsp;&nbsp;&nbsp;" . $newsago;
      $bodytxt = "";
      $headlinecount = 0;
      while (list($key, $line) = each($thefile)) {
        $lowline = strtolower($line);
        $start = strpos($lowline, "<headline>");
        $stop = strpos($lowline, "</headline>");
        if ($start || $stop) {
          $headline = substr($line, $start + 10, $stop - $start + 10);
          if ($headlinecount > 0)
            $bodytxt .= "<br>";
          $bodytxt .= "<b>" . $headline . "</b><br>";
          $headlinecount++;
        } else {
# these two lines makes PHP3 crash?!?
#          $line = str_replace("<blockquote>", "<center><table width=90% border=1 cellspacing=0 cellpadding=4><tr><td bgcolor=#CCCCCC><font face=\"MS Sans Serif\" size=2>", $line);
#          $line = str_replace("</blockquote>", "</font></td></tr></table></center>", $line);

          $line = eregi_replace("<blockquote>", "<center><table width=90% border=1 cellspacing=0 cellpadding=4><tr><td bgcolor=#CCCCCC><font face=\"MS Sans Serif\" size=2>", $line);
          $line = eregi_replace("</blockquote>", "</font></td></tr></table></center>", $line);

          $bodytxt .= $line;
        }
      }

      pagePanel("news", $headtxt, $newsauthor, $bodytxt);
    }
  }
}

function getFilesArray($directory, $eregfilemask="*") {
  $filelist = array();
  $handle = opendir($directory);
  while ($file = readdir($handle)) {
      if (ereg($eregfilemask, $file))
        $filelist[] = $file;
  }
  closedir($handle);

  sort($filelist);
  return $filelist;
}

function pageDisplay($title, $pageFunction) {
  global $sidebarwidth;

  pageBegin($title);

  echo "\n<table border=0 cellspacing=0 cellpadding=0 align=center>";
  echo "<tr><td colspan=7 height=8><td></tr>";
  echo "<tr><td width=4>&nbsp;</td><td width=$sidebarwidth valign=top>";

  initSidePanel();
  include("_navbar.php3");

  $powerphp3text = "<center><a href=\"http://www.php.net\"><img src=\"pics/smlbanners/php3.gif\" width=88 height=31 border=0></a></center>";
  pageSidePanel("", "This page is...", $powerphp3text);
  include("_servertime.php3");

  echo "</td><td width=8>&nbsp;</td>";
  echo "\n<td width=100% valign=top>";


  $pageFunction();


  echo "</td><td width=8>&nbsp;</td>";
  echo "\n<td width=$sidebarwidth valign=top>";

  initSidePanel();
  if ($title=="News" || $title=="InstaPolls") {
    include("_instapoll.php3");
  }
  include("_useful.php3");

  echo "</td><td width=8>&nbsp;</td></tr>";
  echo "<tr><td colspan=7 height=8>";
  #include("footer.html");
  echo "</td></tr></table>";

  pageEnd();
}

function mapCreateText($mapName, $mapDownload, $mapScreenshot, $mapWebsite, $mapEmail, $mapAuthor, $mapFilesize, $mapType, $mapGame, $mapDescription, $mapComment) {
  $text = "";
  $text .= $mapName . "<br>";
  $text .= $mapDownload . "<br>";
  $text .= $mapScreenshot . "<br>";
  $text .= $mapWebsite . "<br>";
  $text .= $mapEmail . "<br>";
  $text .= $mapAuthor . "<br>";
  $text .= $mapFilesize . "<br>";
  $text .= $mapType . "<br>";
  $text .= $mapGame . "<br>";
  $text .= nl2br($mapDescription) . "<br>";
  $text .= nl2br($mapComment) . "<br>";

  return $text;
}

?>
