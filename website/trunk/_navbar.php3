<?php

$navbar = array(
#    Indent, Title                  ,Icon          ,URL
   array( 0,"worldspawn"            ,""            ,""                                                                )

  ,array( 1,  "Main"                ,"news"        ,""                                                                )
  ,array( 2,    "News"              ,""            ,"index.php3"                                                      )
  ,array( 3,      "Archive"         ,""            ,"archivednews.php3"                                               )
  ,array( 3,      "Submit news"     ,""            ,"javascript:mail_decode('qrpxre@cynargdhnxr.pbz?fhowrpg=DhNeX Arjf');"                )
# ,array( 2,    "Armin's..."        ,""            ,""                                                                )
##,array( 3,      "E-mail"          ,""            ,"javascript:mail_decode('nevtb@cynargdhnxr.pbz');"                                    )
# ,array( 3,      "News"            ,""            ,"http://www.planetquake.com/quark/armin/index.shtm"               )
# ,array( 3,      "Advanced"        ,""            ,"http://www.planetquake.com/quark/armin/advanced.html"            )
  ,array( 2,    "InstaPolls"        ,""            ,"instapolls.php3"                                                 )
  ,array( 2,    "Credits"           ,""            ,"credits.php3"                                                    )

  ,array( 1,  "QuArK"               ,"features"    ,""                                                                )
  ,array( 2,    "Features"          ,""            ,"features.php3"                                                   )
  ,array( 2,    "Screenshots"       ,""            ,"features.php3#screenshots"                                       )
  ,array( 2,    "Questions?"        ,""            ,"forums.php3"                                                     )
  ,array( 2,    "Problems?"         ,""            ,"forums.php3"                                                     )
  ,array( 2,    "Download"          ,""            ,"download.php3"                                                   )

  ,array( 1,  "Help &amp; Docs."    ,"documents"   ,""                                                                )
  ,array( 2,    "Infobase"          ,""            ,"http://www.planetquake.com/quark/infobase/index.html"            )
  ,array( 3,      "Map tutorial"    ,""            ,"http://www.planetquake.com/quark/infobase/maped.tutorial.html"   )
  ,array( 3,      "Create Addons"   ,""            ,"http://www.planetquake.com/quark/infobase/adv.html"              )
# ,array( 2,    "Online doc."       ,""            ,"http://www.planetquake.com/quark/help/index.html"                )
# ,array( 3,      "Basic knowledg." ,""            ,"http://www.planetquake.com/quark/help/basicknowledge.html"       )
# ,array( 3,      "Map tutorial"    ,""            ,"http://www.planetquake.com/quark/help/maptutorial/index.html"    )
# ,array( 3,      "FAQ"             ,""            ,"http://www.planetquake.com/quark/help/faq/index.html"            )
# ,array( 3,      "Glossary"        ,""            ,"http://www.planetquake.com/quark/help/glossary/index.html"       )
  ,array( 2,    "Download"          ,""            ,"download.php3#help"                                              )

  ,array( 1,  "Community"           ,"community"   ,""                                                                )
  ,array( 2,    "Communication"     ,""            ,""                                                                )
  ,array( 3,      "Forums"          ,""            ,"forums.php3"                                                     )
  ,array( 3,      "Mailing-lists"   ,""            ,"forums.php3"                                                     )
  ,array( 3,      "IRC / Chat"      ,""            ,"forums.php3"                                                     )
  ,array( 2,    "Misc. files"       ,""            ,""                                                                )
  ,array( 3,      "QuArK Addons"    ,""            ,"download_addons.php3"                                            )
  ,array( 3,      "User Prefabs"    ,""            ,"userprefabs.php3"                                                )
  ,array( 3,      "User Maps"       ,""            ,"usermaps.php3"                                                   )
# ,array( 3,      "Submit"          ,""            ,"usermaps_submit.php3"                                            )
  ,array( 2,    "Help needed"       ,""            ,""                                                                )
  ,array( 3,      "Sugg. plug-ins"  ,""            ,"suggestedplugins.php3"                                           )
# ,array( 3,      "Ad-Banners"      ,""            ,"adbanners.php3"                                                  )
  ,array( 3,      "Ad-Buttons"      ,""            ,"adbuttons.php3"                                                  )

  ,array( 1,  "Download"            ,"download"    ,""                                                                )
  ,array( 2,    "QuArK Addons"      ,""            ,"download_addons.php3"                                            )
  ,array( 2,    "Python"            ,""            ,"download.php3"                                                   )
  ,array( 2,    "QuArK"             ,""            ,"download.php3"                                                   )
  ,array( 2,    "Online-help"       ,""            ,"download.php3"                                                   )
# ,array( 2,    "Build-packs"       ,""            ,"download.php3#buildpacks"                                        )
# ,array( 2,    "Add-ons v6.1"      ,""            ,"download.php3#addons6"                                           )
# ,array( 2,    "Add-ons v5.10"     ,""            ,"download.php3#addons"                                            )
# ,array( 2,    "Plug-ins"          ,""            ,"download.php3#plugins"                                           )
# ,array( 2,    "Misc. Utils."      ,""            ,"download.php3#misc"                                              )
  ,array( 2,    "Source Code"       ,""            ,"http://sourceforge.net/project/?group_id=1181"                   )
# ,array( 2,    "Source Code"       ,""            ,"http://www.planetquake.com/quark/armin/index.shtm#Documentation" )
# ,array( 2,    "Oldies"            ,""            ,"downloadoldies.php3"                                             )

  ,array( 1,  "Beta test"           ,"beta"        ,""                                                                )
  ,array( 2,    "Latest"            ,""            ,"http://www.planetquake.com/quark/latest"                         )
);

$navbartext = stringNavbar($navbar);
pageSidePanel("", "Site Navigation...", $navbartext);

?>
