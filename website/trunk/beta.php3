#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Beta-1");
  pagePanelFile("beta", "QuArK 6 Beta-1&nbsp;&nbsp;&nbsp;(2000.04.26)",  "", "beta_1.html");
}

pageDisplay("Beta-test", 'pageLocalDisplay');
?>
