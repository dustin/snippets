<form method="POST" action="%CGIDIR%/admin/admin.cgi">

<input type="hidden" name="func" value="savecat">

<table border="3">

<input type="hidden" name="id" value="%CATID%">

<tr>
    <td>ID:</td>
    <td>%CATID%</td>
</tr>

<tr>
    <td>Name:</td>
    <td><input name="name" value="%CATNAME%"></td>
</tr>

</table>

<br>

<input type="submit" value="Save"> <input type="reset" value="Reset">

</form>
