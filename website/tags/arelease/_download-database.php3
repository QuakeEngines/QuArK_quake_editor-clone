<?php
##
## -- Applications --
##
$Applications = array();

#                                   Color (keep shading)
#                                   |         QuArK version
#                                   |         |     QuArK title
#                                   |         |     |                   Extra info after QuArK title
#                                   |         |     |                   |                                URL for download
#                                   |         |     |                   |                                |
$Applications[] = new cApplication("#40C040" ,407 ,"QuArK v4.7"       ,"very old"                        ,"http://dl.fileplanet.com/dl/dl.asp?quark/releases/quark407.zip");
$Applications[] = new cApplication("#40D040" ,510 ,"QuArK v5.10"      ,"requires Python -&gt;"           ,"http://dl.fileplanet.com/dl/dl.asp?quark/releases/quark510.exe");
$Applications[] = new cApplication("#40E040" ,602 ,"QuArK v6.2c"      ,"requires Python -&gt;"           ,"http://dl.fileplanet.com/dl/dl.asp?quark/releases/quark_v6.2c.exe");
$Applications[] = new cApplication("#40F040" ,603 ,"QuArK v6.3.0"     ,"requires Python -&gt;"           ,"http://www.fileplanet.com/dl/dl.asp?/planetquake/quark/releases/quark_v6.3.0.EXE");
#$Applications[] = new cApplication("#9090FF" ,604 ,"QuArK v6.4 beta"  ,"snapshot, requires Python -&gt;" ,"http://www.planetquake.com/quark/latest");



##
## -- Games --
##
$Games = array();

#                    Supported since QuArK version
#                    |     Short abbrivation of game-name (MANDATORY and must be UNIQUE, as its used in the download_addons.php3)
#                    |     |        Game-name or Engine-name
#                    |     |        |                                     Icon-file
#                    |     |        |                                     |                        Game/Engine-information URL (if any)
#                    |     |        |                                     |                        |                                 Build tools (delimiter is '|', and syntax is "<text>|<DownloadURL>|<text>|<DownloadURL>")
#                    |     |        |                                     |                        |                                 |
$Games[] = new cGame(407 ,"Q1"    ,"Quake"                              ,"game_quake1.gif"       ,""                               ,"Enhanced buildtools|http://user.tninet.se/~xir870k/|buildq1-06.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/quake1/buildq1-06.zip");
$Games[] = new cGame(407 ,"Hx2"   ,"Hexen 2"                            ,"game_hexen2.gif"       ,""                               ,"hexutils.zip|http://www.fileplanet.com/dl/dl.asp?/quark/buildtools/hexen2/hexutils.zip|buildh2-06.zip|http://www.fileplanet.com/dl/dl.asp?/quark/buildtools/hexen2/buildh2-06.zip");
$Games[] = new cGame(407 ,"Q2"    ,"Quake 2"                            ,"game_quake2.gif"       ,""                               ,"buildq2-39.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/quake2/buildq2-39.zip|txqbsp310.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/quake2/txqbsp310.zip");
$Games[] = new cGame(510 ,"Hr2"   ,"Heretic 2"                          ,"game_heretic2.gif"     ,""                               ,"buildq2-39.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/quake2/buildq2-39.zip");
$Games[] = new cGame(510 ,"HL"    ,"Half-Life (+ Counter-Strike)"       ,"game_halflife.gif"     ,""                               ,"zhlt253.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/halflife/zhlt253.zip|zhlt253-custombuild161.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/halflife/zhlt253-custombuild161.zip");
$Games[] = new cGame(510 ,"SIN"   ,"Sin"                                ,"game_sin.gif"          ,""                               ,"sinbuild.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/sin/sinbuild.zip");
$Games[] = new cGame(602 ,"KP"    ,"Kingpin"                            ,"game_kingpin.gif"      ,""                               ,"buildq2-39.zip|http://dl.fileplanet.com/dl/dl.asp?quark/buildtools/quake2/buildq2-39.zip|Kingpin_SDK_121.zip|http://www.fileplanet.com/index.asp?file=39676");
$Games[] = new cGame(602 ,"SOF"   ,"Soldier of Fortune"                 ,"game_sof.gif"          ,""                               ,"SOFSDK11.zip|http://www.fileplanet.com/index.asp?file=45356");
$Games[] = new cGame(602 ,"Q3A"   ,"Quake 3:Arena (+ Team Arena)"       ,"game_quake3.gif"       ,""                               ,"GTKRadiant's tools|http://www.qeradiant.com/?data=editors/gtk|Q3ToolSetup_Mar172000.exe|http://www.fileplanet.com/index.asp?file=39158");
$Games[] = new cGame(602 ,"STVEF" ,"Star Trek:Voyager-Elite Force"      ,"game_stvef.gif"        ,""                               ,"GTKRadiant's tools|http://www.qeradiant.com/?data=editors/gtk|eliteforceGDK.zip|http://www.fileplanet.com/index.asp?file=50614");
$Games[] = new cGame(602 ,"CS"    ,"Crystal Space engine"               ,"game_crystalspace.gif" ,"http://crystal.sourceforge.net" ,"");
$Games[] = new cGame(603 ,"RTCW"  ,"Return to Castle Wolfenstein"       ,"game_rtcw.gif"         ,""                               ,"WolfToolsSDK.zip|http://www.fileplanet.com/index.asp?file=83872|GTKRadiant's tools|http://www.qeradiant.com/?data=editors/gtk");
$Games[] = new cGame(603 ,"6DX"   ,"6DX engine"                         ,"game_6dx.gif"          ,"http://www.aztica.com"          ,"");
$Games[] = new cGame(603 ,"T"     ,"Torque engine"                      ,"game_torque.gif"       ,"http://www.garagegames.com"     ,"");
$Games[] = new cGame(603 ,"JK2"   ,"Jedi Knight 2:Jedi Outcast"         ,"game_jk2.gif"          ,""                               ,"JK2EditingTools.zip|http://www.fileplanet.com/index.asp?file=87277");
$Games[] = new cGame(603 ,"MOHAA" ,"Medal of Honor:Allied Assault"      ,"game_mohaa.gif"        ,""                               ,"MOHAATools.zip|http://www.fileplanet.com/index.asp?file=86998");
$Games[] = new cGame(603 ,"SOF2"  ,"Soldier of Fortune 2:Double Helix"  ,"game_sof2.gif"         ,""                               ,"");
# In the works to be supported.
# ---
# Unsupported, but based on similar Quake-engines.
$Games[] = new cGame(999 ,"DK"    ,"Daikatana"                          ,"game_daikatana.gif"    ,"http://www.daikatana.com"       ,"dkmapedit.zip|http://www.fileplanet.com/index.asp?file=48509");
$Games[] = new cGame(999 ,"FAKK"  ,"Heavy Metal:F.A.K.K.2"              ,""                      ,"http://www.ritual.com/FAKK2"    ,"FAKKTools.exe|http://www.fileplanet.com/index.asp?file=50050");



##
## -- QuArK Addons --
##
$OfficialQRKAddon = "(Directly from our <a href=\"http://sourceforge.net/project/?group_id=1181\">SourceForge CVS-library</a>)";

$GamesQRKAddons = array();

#               Short abbrivation of game-name (Look at the $Games array above)
#               |                          Add-on title
#               |                          |                          Add-on info-URL (optional)
#               |                          |                          |                                              Add-on extra comments (optional)
#               |                          |                          |                                              |   Add-on filename
#               |                          |                          |                                              |   |                   Add-on download URL
#               |                          |                          |                                              |   |                   |                                                                                                           Add-on filename extra comments
#               |                          |                          |                                              |   |                   |                                                                                                           |

# Quake
$GamesQRKAddons["Q1"   ] = array();
$GamesQRKAddons["Q1"   ][] = new cQRKAddon("Quake 1"                  ,""                                           ,"" ,"DataQ1.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_1/DataQ1.qrk"        ,$OfficialQRKAddon);
$GamesQRKAddons["Q1"   ][] = new cQRKAddon("Nehahra"                  ,"http://www.planetquake.com/nehahra"         ,"" ,"NehahraQ1.qrk"    ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_1/NehahraQ1.qrk"     ,$OfficialQRKAddon);
$GamesQRKAddons["Q1"   ][] = new cQRKAddon("CTF"                      ,""                                           ,"" ,"CTFq1.qrk"        ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_1/CTFq1.qrk"         ,$OfficialQRKAddon);
$GamesQRKAddons["Q1"   ][] = new cQRKAddon("Team Fortress"            ,"http://www.planetfortress.com/teamfortress" ,"" ,"TFq1.qrk"         ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_1/TFq1.qrk"          ,$OfficialQRKAddon);
$GamesQRKAddons["Q1"   ][] = new cQRKAddon("Dissolution of Eternity"  ,""                                           ,"" ,"RogueQ1.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_1/RogueQ1.qrk"       ,$OfficialQRKAddon);

# Hexen-2
$GamesQRKAddons["Hx2"  ] = array();
$GamesQRKAddons["Hx2"  ][] = new cQRKAddon("Hexen-2"                  ,""                                           ,"" ,"DataH2.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Hexen_II/DataH2.qrk"       ,$OfficialQRKAddon);
$GamesQRKAddons["Hx2"  ][] = new cQRKAddon("Portal of Praevus"        ,""                                           ,"" ,"Praevus.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Hexen_II/Praevus.qrk"      ,$OfficialQRKAddon);

# Quake 2
$GamesQRKAddons["Q2"   ] = array();
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("Quake 2"                  ,""                                           ,"" ,"DataQ2.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/DataQ2.qrk"        ,$OfficialQRKAddon);
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("CTF"                      ,""                                           ,"" ,"CTFq2.qrk"        ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/CTFq2.qrk"         ,$OfficialQRKAddon);
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("Future Wars"              ,"http://www.planetquake.com/futurewar"       ,"" ,"FutureWarsq2.qrk" ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/FutureWarsq2.qrk"  ,$OfficialQRKAddon);
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("Ground Zero"              ,""                                           ,"" ,"Rogueq2.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/Rogueq2.qrk"       ,$OfficialQRKAddon);
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("The Reckoning"            ,""                                           ,"" ,"Xatrixq2.qrk"     ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/Xatrixq2.qrk"      ,$OfficialQRKAddon);
$GamesQRKAddons["Q2"   ][] = new cQRKAddon("Lazarus"                  ,"http://www.planetquake.com/lazarus"         ,"" ,"Q2_Lazarus.qrk"   ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_2/Q2_Lazarus.qrk"    ,$OfficialQRKAddon);

# Heretic-2
$GamesQRKAddons["Hr2"  ] = array();
$GamesQRKAddons["Hr2"  ][] = new cQRKAddon("Heretic-2"                ,""                                           ,"" ,"DataHr2.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Heretic_II/DataHr2.qrk"    ,$OfficialQRKAddon);

# Half-Life
$GamesQRKAddons["HL"   ] = array();
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Half-Life"                ,""                                           ,"Counter-Strike, Blue Shift and many more..."
                                                                                                                        ,"DataHL.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Half-Life/DataHL.qrk"    ,$OfficialQRKAddon."<br><a href=\"http://www.fileplanet.com/dl/dl.asp?/planetquake/quark/addons/half-life/DataHL_20020714.zip\">DataHL_20020714.zip</a> (Dated 2002-07-14)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Gunman Chronicles"        ,""                                           ,"" ,"DataGC.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Half-Life/DataGC.qrk"    ,$OfficialQRKAddon."<br>NOTE: QuArK supports 'Gunman Chronicles' map-editing, but it is a bit difficult to setup correctly, so please ask in the <a href=\"forums.php3\">QuArK-forum</a> if you need to know how, or look at <a href=\"http://www.planetquake.com/quark/infobase/maped.games.half-life.html#cstrike_retail_setup\">this info</a> in the <a href=\"http://www.planetquake.com/quark/infobase/\">InfoBase</a>.");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Blue Shift"               ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)<br>NOTE: QuArK supports 'Blue Shift' map-editing, but it is a bit difficult to setup correctly, so please ask in the <a href=\"forums.php3\">QuArK-forum</a> if you need to know how, or look at <a href=\"http://www.planetquake.com/quark/infobase/maped.games.half-life.html#cstrike_retail_setup\">this info</a> in the <a href=\"http://www.planetquake.com/quark/infobase/\">InfoBase</a>.");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Counter-Strike"           ,"http://www.counter-strike.net"              ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)<br>NOTE: QuArK supports 'Counter-Strike Retail' map-editing, but it is a bit difficult to setup correctly, so please ask in the <a href=\"forums.php3\">QuArK-forum</a> if you need to know how, or look at <a href=\"http://www.planetquake.com/quark/infobase/maped.games.half-life.html#cstrike_retail_setup\">this info</a> in the <a href=\"http://www.planetquake.com/quark/infobase/\">InfoBase</a>.");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Opposing Force"           ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Team Fortress Classic"    ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Deathmatch Classic"       ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("BuzzyBots"                ,"http://www.buzzybots.dk"                    ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("HL Rally"                 ,"http://www.hlrally.net"                     ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Action Half-Life"         ,"http://ahl.action-web.net"                  ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Science & Industry"       ,"http://www.planethalflife.com/si"           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Classic CTF"              ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("The Assignment"           ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Kanonball"                ,"http://www.planethalflife.com/kanonball"    ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("FireArms"                 ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Desert Crisis"            ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("The Sherman Project"      ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Arg! The Pirates Strike Back"
                                                                      ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Sven-Coop"                ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Front Line Force"         ,"http://www.planethalflife.com/frontline"    ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Day of Defeat"            ,"http://www.dayofdefeatmod.com"              ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Turbo"                    ,""                                           ,"" ,""                 ,""                                                                                                       ,"(In the same DataHL.QRK file as above.)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Pirates and Vikings and Knights"
                                                                      ,""                                           ,"" ,"pvkHL.qrk"        ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Half-Life/pvkHL.qrk"     ,$OfficialQRKAddon);
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Swarm"                    ,"http://swarm.edgegaming.com"                ,"" ,"SwarmHL.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Half-Life/SwarmHL.qrk"   ,$OfficialQRKAddon);
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Wizard Wars"              ,"http://www.planethalflife.com/wizardwars"   ,"" ,"wwqrk.zip"        ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/half-life/wwqrk.zip"                                    ,"(Dated 2001-01-16)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("War in Europe"            ,""                                           ,"" ,"wieHL.zip"        ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/half-life/wieHL.zip"                                    ,"(Dated 2001-01-12)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Prefabs"                  ,""                                           ,"Selected prefabs from <a href=\"http://prefabs.gamedesign.net\">prefabs.gamedesign.net</a>"
                                                                                                                        ,"prefabsHL.zip"    ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/half-life/prefabsHL.zip"                                ,"(Dated 2001-03-17)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("The Opera"                ,"http://opera.redeemedsoft.com"              ,"" ,"OperaHL.zip"      ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/half-life/OperaHL.zip"                                  ,"(Dated 2001-07-22)");
$GamesQRKAddons["HL"   ][] = new cQRKAddon("Natural Selection"        ,"http://www.natural-selection.org"           ,"Thanks to \"<a href=\"javascript:mail_decode('fso@ubzr.ay');\">nugigerulus</a>\" for sorting the textures"
                                                                                                                        ,"nstrHL_20020125.zip"
                                                                                                                                            ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/half-life/nstrHL_20020125.zip"                          ,"(Technology Release. Textures updated 2002-01-25)");

# Sin
$GamesQRKAddons["SIN"  ] = array();
$GamesQRKAddons["SIN"  ][] = new cQRKAddon("Sin"                      ,""                                           ,"" ,"DataSin.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Sin/DataSin.qrk"         ,$OfficialQRKAddon);

# Kingpin
$GamesQRKAddons["KP"   ] = array();
$GamesQRKAddons["KP"   ][] = new cQRKAddon("Kingpin"                  ,""                                           ,"" ,"DataKP.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Kinpin/DataKP.qrk"       ,$OfficialQRKAddon);

# Soldier of Fortune
$GamesQRKAddons["SOF"  ] = array();
$GamesQRKAddons["SOF"  ][] = new cQRKAddon("Soldier of Fortune"       ,""                                           ,"" ,"DataSOF.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/SOF/DataSOF.qrk"         ,$OfficialQRKAddon);

# Quake-3:Arena
$GamesQRKAddons["Q3A"  ] = array();
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Quake-3:Arena"            ,""                                           ,"" ,"DataQ3.qrk"       ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_3/DataQ3.qrk"      ,$OfficialQRKAddon);
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Quake-3 Team Arena"       ,""                                           ,"" ,"DataQ3TA.qrk"     ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_3/DataQ3TA.qrk"    ,$OfficialQRKAddon);
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Arch library"             ,""                                           ,"" ,"Q3Archlib.qrk"    ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Q3Archlib.qrk"           ,$OfficialQRKAddon);
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Q3Fortress"               ,"http://www.q3f.com"                         ,"" ,"Q3Fq3.qrk"        ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_3/Q3Fq3.qrk"       ,$OfficialQRKAddon);
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Q3 map model prefabs"     ,""                                           ,"By <a href=\"javascript:mail_decode('E.C.Uhyforetra@Fghqrag.GHQrysg.AY');\">Rolf Hulsbergen</a>. (original <a href=\"http://www.student.citg.tudelft.nl/c9375215/idmodels.zip\">file</a>)"
                                                                                                                        ,"Q3models.qrk"     ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Quake_3/Q3models.qrk"    ,$OfficialQRKAddon);
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Q3 sample DM maps"        ,""                                           ,"The three Q3Radiant sample maps, fixed and converted into QuArK format, using tree-view groups. By <a href=\"javascript:mail_decode('E.C.Uhyforetra@Fghqrag.GHQrysg.AY');\">Rolf Hulsbergen</a>. (original <a href=\"http://www.student.citg.tudelft.nl/c9375215/samples.zip\">file</a>)"
                                                                                                                        ,"Q3A_Map_Samples.zip"
                                                                                                                                            ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/quake3/Q3A_Map_Samples.zip"                             ,"(Dated 2001-03-17)");
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Q3 Pong"                  ,""                                           ,"By <a href=\"javascript:mail_decode('klcure@zdz.vjnec.pbz');\">\"Xypher\"</a>."
                                                                                                                        ,"Q3Pong.zip"       ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/quake3/Q3Pong.zip"                                      ,"(Dated 2001-07-18)");
$GamesQRKAddons["Q3A"  ][] = new cQRKAddon("Q3 Rally"                 ,""                                           ,"By <a href=\"javascript:mail_decode('klcure@zdz.vjnec.pbz');\">\"Xypher\"</a>."
                                                                                                                        ,"Q3Rally.zip"      ,"http://dl.fileplanet.com/dl/dl.asp?quark/addons/quake3/Q3Rally.zip"                                     ,"(Dated 2001-07-18)");

# Star Trek:Voyager-Elite Force
$GamesQRKAddons["STVEF"] = array();
$GamesQRKAddons["STVEF"][] = new cQRKAddon("ST:V - Elite Force"       ,""                                           ,"" ,"DataSTVEF.qrk"    ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/STVEF/DataSTVEF.qrk"     ,$OfficialQRKAddon);

# Return To Castle Wolfenstein
$GamesQRKAddons["RTCW" ] = array();
$GamesQRKAddons["RTCW" ][] = new cQRKAddon("Return to Castle Wolfenstein" ,""                                       ,"" ,"DataRTCW.qrk"     ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/RTCW/DataRTCW.qrk"       ,$OfficialQRKAddon);

# Medal of Honor:Allied Assault
$GamesQRKAddons["MOHAA"] = array();
$GamesQRKAddons["MOHAA"][] = new cQRKAddon("Medal of Honor:Allied Assault" ,""                                      ,"" ,"DataMOHAA.qrk"    ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/MOHAA/DataMOHAA.qrk"     ,$OfficialQRKAddon);

# Torque
$GamesQRKAddons["T"    ] = array();
$GamesQRKAddons["T"    ][] = new cQRKAddon("Torque"                   ,""                                           ,"" ,"DataTorque.qrk"   ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/Torque/DataTorque.qrk"   ,$OfficialQRKAddon);

# Jedi Knight 2:Jedi Outcast
$GamesQRKAddons["JK2"  ] = array();
$GamesQRKAddons["JK2"  ][] = new cQRKAddon("Jedi Knight 2:Jedi Outcast" ,""                                         ,"" ,"DataJK2.qrk"      ,"http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/quark/runtime/addons/JK2/DataJK2.qrk"         ,$OfficialQRKAddon);
?>
