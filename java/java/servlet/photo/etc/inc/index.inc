<html><head><title>Photo Album</title>
%STYLESHEET%
</head>
<body bgcolor="fFfFfF">

<h2>Dustin's Photo Album</h2>

<ul>
    <li><a href="%SELF_URI%?func=addform">Add an image</a></li>
    <li><a href="%SELF_URI%?func=findform">Find an image</a></li>
    <li><a href="%SELF_URI%?func=catview">
	Category view</a></li>
	<li><a href="%SELF_URI%?func=styleform">Set viewing style</a></li>

</ul>

<h2>Canned searches</h2>

<ul>
  %SAVED%
</ul>

<form method="POST" action="%SELF_URI%">
<input type="hidden" name="func" value="search">
<input type="hidden" name="maxret" value="5">
<input type="hidden" name="fieldjoin" value="and">
<input type="hidden" name="keyjoin" value="and">
<input type="hidden" name="order" value="a.ts">
<input type="hidden" name="sdirection" value="desc">
<input type="hidden" name="field" value="keywords">
Quick Search:  <input name="what">
<input type="submit" value="Find">
</form>

<p>
<h2>Credits:</h2>
All pages herein were created using vi.  For more information on the vi
web page publishing system, type <i>man vi</i> at your prompt.

<!--
    Logged in as:  %REMOTE_USER%
-->

</body>
</html>
