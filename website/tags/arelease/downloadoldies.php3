#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Download Oldies");
  echo "<br>";
#  include("downloadoldies.html");
  pageRawFile("downloadoldies.html");
}

pageDisplay("Download oldies", 'pageLocalDisplay');
?>
