#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Forums / Mailing-lists / IRC");

  pagePanelFile("community", "Main QuArK map-editing forum",     "Moderator: Daniel Stolz",      "forums_quark.html");
  pagePanelFile("community", "QuArK News Wire",      "Writer: The development team", "forums_quarknews.html");
  pagePanelFile("community", "Other map-editing web-based messageboards", "",        "forums_other.html");
# pagePanelFile("community", "Serpent Lord's Editing Messageboard", "Moderator: Serpent Lord", "forums_serpentlord.html");
  pagePanelFile("community", "IRC / Chat",           "",                             "irc.html");
  pagePanelFile("community", "Coding plug-ins for QuArK using Delphi and Python", "Moderators: Decker, Tiglari",  "forums_quarkpython.html");
}

pageDisplay("IRC/Forums/Mailing-lists", 'pageLocalDisplay');
?>
