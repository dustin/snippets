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
%STYLESHEET%
</head>
<body bgcolor="#fFfFfF">

<center>
<img src="%CGIDIR%/img.cgi/%IMAGE%">
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
<a href="javascript:openWindow('%CGIDIR%/img.cgi/%IMAGE%')">Open image only
in new window</a><br>

<hr>
</body></html>
