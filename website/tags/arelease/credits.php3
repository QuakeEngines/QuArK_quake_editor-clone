#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Credits");
  pagePanelFile("community", "Thanks goes out to...",  "", "credits_list.html");
}

pageDisplay("Credits", 'pageLocalDisplay');
?>
