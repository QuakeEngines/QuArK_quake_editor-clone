#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Ad Banners");
  pagePanelFile("community", "Ad-banners by the community",  "", "adbanners_list.html");
}

pageDisplay("Ad Banners", 'pageLocalDisplay');
?>
