#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("IRC / Chat");

  pagePanelFile("community", "IRC",     "",    "irc.html");
}

pageDisplay("IRC/Chat", 'pageLocalDisplay');
?>
