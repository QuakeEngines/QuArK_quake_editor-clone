#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Features & Screenshots");
# pagePanelFile("features", "Features of QuArK 5.x",    "", "features_quark5.html");
  pagePanelFile("features", "Features of QuArK 6.x",    "", "features_quark6.html");
  echo "<a name=\"screenshots\"></a>";
  pagePanelFile("features", "Screenshots of QuArK 5.x", "", "features_quark5_screenshots.html");
}

pageDisplay("Features", 'pageLocalDisplay');
?>
