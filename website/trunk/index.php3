#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("News");
  displayNews();
}

pageDisplay("News", 'pageLocalDisplay');
?>
