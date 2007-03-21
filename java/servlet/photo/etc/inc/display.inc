<html><head><title>Info for %IMAGE%</title>

<script language="javascript">

<!-- hide me from non-JavaScript-enabled browsers

function openWindow(URL,name) {

	popupWin = window.open(URL, name, 'resizable=yes,scrollbars=yes');
        
	if ( (navigator.appName != "Microsoft Internet Explorer")
	      && (navigator.appVersion.substring(0,1) == "3") )
        popupWin.focus();

}
// done hiding -->
</script>
</head>
<body bgcolor="#fFfFfF">

<table width="100%"> <!-- % -->
<tr>
	<td align="left">%PREV%</td>
	<td align="right">%NEXT%</td>
</tr>
</table>

<center>
<img src="%SELF_URI%?func=getimage&photo_id=%IMAGE%">
</center>
<p>

Category: %CAT%
<p>

Size: %SIZE% bytes
<p>

Taken: %TAKEN%
<p>

Added: %TIMESTAMP% by %ADDEDBY%
<p>

Keywords:<br>
%KEYWORDS%
<p>


Info:<br>
%INFO%

<p>

<a href="javascript:openWindow('%SELF_URI%?func=display&id=%IMAGE%')">Open
in new window</a><br>
<a href="javascript:openWindow('%SELF_URI%?func=getimage&photo_id=%IMAGE%')">
Open image only in new window</a><br>
<a href="%SELF_URI%?func=logview&view=viewers&which=%IMAGE%">Who's seen
this?</a><br>
<a href="%SELF_URI%?func=display&id=%IMAGE%">Linkable image</a><br>

<hr>
</body></html>
