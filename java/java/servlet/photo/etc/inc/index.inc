<html><head><title>Photo Album</title>
%STYLESHEET%
</head>
<body bgcolor="fFfFfF">

<h2>Dustin's Photo Album</h2>

<ul>
    <li><a href="add.shtml">Add an image</a></li>
    <li><a href="find.shtml">Find an image</a></li>
    <li><a href="/perl/dustin/photo/photo.cgi?func=catview">
	Category view</a></li>

</ul>

<h2>Canned searches</h2>

<ul>
  %SAVED%
</ul>

<form method="POST" action="/perl/dustin/photo/photo.cgi">
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
web page publishing system, type <tt>man vi</tt> at your prompt.

<!--
    Logged in as:  %REMOTE_USER%
-->

</body>
</html>
