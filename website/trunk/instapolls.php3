#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("InstaPolls");
  pagePanelFile("community", "Archived InstaPolls", "", "instapolls_archive.html");
}

pageDisplay("InstaPolls", 'pageLocalDisplay');
?>
