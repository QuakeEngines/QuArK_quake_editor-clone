#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Ad Buttons");
  pagePanelFile("community", "Ad-buttons by the community",  "", "adbuttons_list.html");
}

pageDisplay("Ad Buttons", 'pageLocalDisplay');
?>
