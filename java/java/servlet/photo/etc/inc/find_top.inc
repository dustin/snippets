<html>
<head><title>Find Results</title></head>


<body bgcolor="#fFfFfF">

<!--
%QUERY%
-->

<table width="100%"> <!-- % -->
<tr>
	<td valign="top" align="left">
		<font size="+3"><b>Search matched %TOTAL% entries</b></font>
	</td>

	<td valign="top" align="right">
		<form method="POST" action="%SELF_URI%">
			<input type="hidden" name="func" value="savesearch">
			<input type="hidden" name="search" value="%SEARCH%">
			Save search as:  <input name="name">
			<input type="submit" value="Save">
		</form>
	</td>
</tr>
</table>

<p>

<table>
<tr>
