<?php

$instapollno="3435";

$instapolltxt="
<form name=\"form$instapollno\">
<font face=\"MS Sans Serif\" size=\"-2\">
<center>
<!-- 2001.02.26 -->
What do you think of QuArK's colorful captions:<br><br>
<img width=74 height=44 border=0 src=\"pics/instapoll/instapoll20010226.jpg\">
</center>
<!--
<br>
<table border=0 cellspacing=2 cellpadding=0>
<tr><td class=\"font8\" valign=top><input type=radio name=\"question\" value=\"16245\"></td><td class=\"font8\">I don't use them and don't care if they disappear.<br><br></tr>
<tr><td class=\"font8\" valign=top><input type=radio name=\"question\" value=\"16244\"></td><td class=\"font8\">I somewhat use them, but would not miss them if they disappeared.<br><br></tr>
<tr><td class=\"font8\" valign=top><input type=radio name=\"question\" value=\"16243\"></td><td class=\"font8\">I use them and would miss them if they disappeared.<br><br></tr>
<tr><td class=\"font8\" valign=top><input type=radio name=\"question\" value=\"16246\"></td><td class=\"font8\">What colorful captions?</tr>
</table>
-->
<p>
<!--  <input type=button name=\"bVote\" value=\" Vote \" onclick=\"instapoll$instapollno()\">-->
&nbsp;<input type=button name=\"bView\" value=\"Results\" onclick=\"view$instapollno()\">
</p>
</font></form>
";

$instapollscript="
<script language=\"Javascript\">
<!--
function instapoll$instapollno() {
  url = \"http://asp.planetquake.com/instapoll/poll.asp?poll_id=$instapollno&maxwidth=300\";
  count = 0;
  while (document.form$instapollno.question[count].checked != true) { count++; }
  url += \"&choice=\" + document.form$instapollno.question[count].value;
  ipwindow = open(\"\",\"prewindow\",\"scrollbars=yes,toolbar=no,height=340,width=500\");
  ipwindow.location.href = url;
}
function view$instapollno() {
  url = \"http://asp.planetquake.com/instapoll/poll.asp?poll_id=$instapollno&maxwidth=300&dontvote=true\";
  ipwindow = open(\"\",\"prewindow\",\"scrollbars=yes,toolbar=no,height=340,width=500\");
  ipwindow.location.href = url;
}
// -->
</script>
";

#pageSidePanel("", "InstaPoll...", $instapollscript . $instapolltxt);

?>
