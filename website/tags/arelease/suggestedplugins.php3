#!/usr/bin/php
<?php
include("_functions.php3");

function pageLocalDisplay() {
  pageName("Suggested plug-ins");

  pagePanelFile("community", "Intro",    "", "suggestedplugins/999_intro.html");

  echo "<a name=\"suggest14\"></a>";
  pagePanelFile("community", "Import DXF Files",                            "", "suggestedplugins/014_import_dxf_files.html");
  echo "<a name=\"suggest13\"></a>";
  pagePanelFile("community", "Torus Creator",                               "", "suggestedplugins/013_torus_creator.html");
  echo "<a name=\"suggest12\"></a>";
  pagePanelFile("community", "Arches",                                      "", "suggestedplugins/012_arches.html");
  echo "<a name=\"suggest11\"></a>";
  pagePanelFile("community", "Light emitting textures in OpenGL-views",     "", "suggestedplugins/011_light_emitting_textures_in_opengl_views.html");
  echo "<a name=\"suggest10\"></a>";
  pagePanelFile("community", "Polygon",                                     "", "suggestedplugins/010_polygon.html");
  echo "<a name=\"suggest9\"></a>";
  pagePanelFile("community", "Import/Export of games special file-formats", "", "suggestedplugins/009_import_export_of_games_special_file_formats.html");
  echo "<a name=\"suggest8\"></a>";
  pagePanelFile("community", "Extract used textures from selected .BSPs",   "", "suggestedplugins/008_extract_used_textures_from_selected__bsps.html");
  echo "<a name=\"suggest7\"></a>";
  pagePanelFile("community", "Merge brushes",                               "", "suggestedplugins/007_merge_brushes.html");
  echo "<a name=\"suggest6\"></a>";
  pagePanelFile("community", "Show width/height/depth of selected poly(s)", "", "suggestedplugins/006_show_width_height_depth_of_selected_poly_s_.html");
  echo "<a name=\"suggest5\"></a>";
  pagePanelFile("community", "Enlarge/Shrink a single face",                "", "suggestedplugins/005_enlarge_shrink_a_single_face.html");
  echo "<a name=\"suggest4\"></a>";
  pagePanelFile("community", "Finishing the QuArK model-editor",            "", "suggestedplugins/004_finishing_the_quark_model_editor.html");
  echo "<a name=\"suggest3\"></a>";
  pagePanelFile("community", "Entity prefab 'editor'",                      "", "suggestedplugins/003_entity_prefab__editor_.html");
  echo "<a name=\"suggest2\"></a>";
  pagePanelFile("community", "User-defined pivot point",                    "", "suggestedplugins/002_user_defined_pivot_point.html");
  echo "<a name=\"suggest1\"></a>";
  pagePanelFile("community", "Three-point cut plane",                       "", "suggestedplugins/001_three_point_cut_plane.html");
}

pageDisplay("Suggested plug-ins", 'pageLocalDisplay');
?>
