<?php

$prefabs_imageroot = "http://www.planetquake.com/quark/userprefabs/";
#$prefabs_imageroot = "http://millennium/userprefabs/";
$prefabs_fileroot  = "http://www.fileplanet.com/dl.aspx?/planetquake/quark/userprefabs/";

$prefabs_headers = array(
#                     sub-folder    Full-Name                           32x32-game-icon
 "quake1"    => array("quake1/",    "Quake 1",                          "pics/fileicons/game_quake1.gif")
,"hexen2"    => array("hexen2/",    "Hexen 2",                          "pics/fileicons/game_hexen2.gif")
,"quake2"    => array("quake2/",    "Quake 2",                          "pics/fileicons/game_quake2.gif")
,"heretic2"  => array("heretic2/",  "Heretic 2",                        "pics/fileicons/game_heretic2.gif")
,"halflife"  => array("half-life/", "Half-Life",                        "pics/fileicons/game_halflife.gif")
,"sin"       => array("sin/",       "Sin",                              "pics/fileicons/game_sin.gif")
,"kingpin"   => array("kingpin/",   "Kingpin",                          "pics/fileicons/game_kingpin.gif")
,"sof"       => array("sof/",       "Soldier of Fortune",               "pics/fileicons/game_sof.gif")
,"quake3"    => array("quake3/",    "Quake 3:Arena",                    "pics/fileicons/game_quake3.gif")
,"stvef"     => array("stvef/",     "Star Trek:Voyager - Elite Force",  "pics/fileicons/game_stvef.gif")
,"rtcw"      => array("rtcw/",      "Return to Castle Wolfenstein",     "pics/fileicons/game_rtcw.gif")
,"mohaa"     => array("mohaa/",     "Medal of Honor:Allied Assault",    "pics/fileicons/game_mohaa.gif")
);

$added_arraycol = 0;
$updated_arraycol = 1;
$image_arraycol = 2;
$file_arraycol = 3;
$author_arraycol = 4;
$email_arraycol = 5;

$prefabs_database = array(
#array("gamename"
#  ,array("0000-00-00", "" # date-added, date-updated
#    ,"prefabimage"
#    ,"prefabzip"
#    ,"author" ("*;author" = collected by)
#    ,"email"
#  )
#  [...]
#)
# ---
 array("quake1"
  ,array("2000-10-21", "2000-11-04"
    ,"prefabspack1.gif"
    ,"prefabspack1Updatebeta1.zip"
    ,"Joe"
    ,"joe@quakemasters.inbox.as"
  )
  ,array("2000-10-27", ""
    ,"balloon.jpg"
    ,"balloon.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
  ,array("2000-10-27", ""
    ,"chandelier.jpg"
    ,"chandelier.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
  ,array("2000-10-27", ""
    ,"spikeme.jpg"
    ,"spikeme.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
  ,array("2000-10-27", ""
    ,"amazing.jpg"
    ,"amazing.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
  ,array("2000-10-21", ""
    ,"octapipes.gif"
    ,"octapipes.zip"
    ,"Joe"
    ,"joe@quakemasters.inbox.as"
  )
)
# ---
,array("hexen2"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("quake2"
  ,array("2003-05-11", ""
    ,"Door1-ByDataKiller.jpg"
    ,"Door1-ByDataKiller.zip"
    ,"DataKiller"
    ,"Door1-ByDataKiller"
  )
  ,array("2001-11-14", ""
    ,"alphabet.gif"
    ,"alphabet.zip"
    ,"Max Charleson"
    ,"Max.Charleson@btinternet.com"
  )
  ,array("2001-09-17", ""
    ,"alienship.jpg"
    ,"alienship.zip"
    ,"&lt;uSm&gt;Dune"
    ,"janthonyb1@hotmail.com"
  )
  ,array("2001-04-13", ""
    ,"droptube.gif"
    ,"droptube.zip"
    ,"R. Terrett"
    ,"Snarfevs@hotmail.com"
  )
  ,array("2001-01-03", ""
    ,"pre-spindeath.jpg"
    ,"pre-spindeath.zip"
    ,"Blade"
    ,"person78@home.com"
  )
  ,array("2000-11-23", ""
    ,"stonetower.gif"
    ,"stonetower.zip"
    ,"Capcom"
    ,"iamcapcom@hotmail.com"
  )
  ,array("2000-11-08", ""
    ,"dog_house_pf.jpg"
    ,"dog_house_pf.zip"
    ,"Gary Oaks"
    ,"iamzelda0@yahoo.com"
  )
  ,array("2000-10-27", ""
    ,"rollers.jpg"
    ,"rollers.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
  ,array("2000-10-27", ""
    ,"barracks.jpg"
    ,"barracks.zip"
    ,"Wayne Peeples"
    ,"wayne@pcbest.com"
  )
)
# ---
,array("heretic2"
  ,array("2000-12-12", ""
    ,"mstairs.gif"
    ,"mstairs.zip"
    ,"astouffer86"
    ,"astouffer86@yahoo.com"
  )
)
# ---
,array("halflife"
  ,array("2002-06-19", ""
    ,"pilot.jpg"
    ,"pilot.zip"
    ,"David van der Tuyn"
    ,"david@vandertuyn.demon.nl"
  )
  ,array("2002-05-28", ""
    ,"tiger.jpg"
    ,"tiger.zip"
    ,"Ed Agoff"
    ,"edward@agoff.net"
  )
  ,array("2002-04-19", ""
    ,"builder.jpg"
    ,"builder.zip"
    ,"RoulioZ"
    ,"roulioa@hotmail.com"
  )
  ,array("2001-03-25", ""
    ,"ellis-lamppost.gif"
    ,"ellis-lamppost.zip"
    ,"Ellis"
    ,"ellisd@newmail.net"
  )
  ,array("2000-11-23", ""
    ,"t-62.gif"
    ,"t-62.zip"
    ,"Esa Repo"
    ,"esa.repo@pp.nic.fi"
  )
  ,array("2000-10-27", ""
    ,"prefab-HL.gif"
    ,"prefab-HL.ace"
    ,"Decker"
    ,"decker@***PLEASE@NO*SPAM***planetquake.com"
  )
)
# ---
,array("sin"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("kingpin"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("sof"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("quake3"
#array("gamename"
#  ,array("0000-00-00", "" # date-added, date-updated
#    ,"prefabimage"
#    ,"prefabzip"
#    ,"author" ("*;author" = collected by)
#    ,"email"
#  )
  ,array("2001-05-06", ""
    ,"Q3ArchesLibrary.gif"
    ,"Q3ArchesLibrary.zip"
    ,"Rolf Hulsbergen"
    ,"R.P.Hulsbergen@Student.TUDelft.NL"
  )
)
# ---
,array("stvef"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("rtcw"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
,array("mohaa"
# ,array("", ""
#   ,""
#   ,""
#   ,""
#   ,""
# )
)
# ---
);

?>
