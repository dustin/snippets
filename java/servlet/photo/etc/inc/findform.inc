<html><head><title>Find an Image</title>
%STYLESHEET%
</head>
<body bgcolor="fFfFfF">

<h2>Simple Search</h2>

<form method="POST" action="%SELF_URI%">
<input type="hidden" name="func" value="search">
<input type="hidden" name="maxret" value="5">
<input type="hidden" name="fieldjoin" value="and">
<input type="hidden" name="keyjoin" value="and">
<input type="hidden" name="order" value="a.ts">
<input type="hidden" name="sdirection" value="desc">

Find all images whose

<select name="field">
    <option value="keywords">Keyword
    <option value="descr">Info (slow)
</select>

contains

<input name="what"><br>

<input type="submit" value="Find">
</form>
<hr>

<h2>Advanced Search</h2>
Only fields that are filled out will be used, default search will return
all images.

<form method="POST" action="%SELF_URI%">
<input type="hidden" name="func" value="search">

Category:
<select name="cat" multiple size="5">
%CAT_LIST%
</select>
<p>

<select name="fieldjoin">
    <option>and
    <option>or
</select>
<select name="field">
    <option value="keywords">Keyword
    <option value="descr">Info (slow)
</select>
contains
<select name="keyjoin">
    <option value="or">one of
    <option value="and">all of
</select>
<input name="what"><br>

<table>

<tr>
<td>
<select name="tstartjoin">
    <option>and
    <option>or
</select>
was taken since (date)
<input name="tstart"><br>
</td>

<td>
<select name="tendjoin">
    <option>and
    <option>or
</select>
was taken before (date)
<input name="tend"><br>
</td>
</tr>

<tr>
<td>
<select name="startjoin">
    <option>and
    <option>or
</select>
was added since (date)
<input name="start"><br>
</td>

<td>
<select name="endjoin">
    <option>and
    <option>or
</select>
was added before (date)
<input name="end"><br>
</td>
</tr>
</table>

Sort by when the picture was
<select name="order">
    <option value="a.ts">added
    <option selected value="a.taken">taken
</select>
and show
<select name="sdirection">
    <option value="">oldest
    <option value="desc">newest
</select>
images first.
<br>
Return at most
<select name="maxret">
    <option value="5">5
    <option selected value="10">10
    <option value="50">50
    <option value="100">100
    <option value="0">All
</select>
images at a time.

<br>
<input type="submit" value="Find">
<input type="reset" value="Clear Form">

</form>

<!--#include virtual="../tail.shtml"-->
</body>
</html>
