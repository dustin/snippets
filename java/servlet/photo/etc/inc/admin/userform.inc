<h2>Editing user %USER%</h2>
<form method="POST" action="%CGIDIR%/admin/admin.cgi">

<input type="hidden" name="func" value="saveuser">

<table border="3">

%USERPART%

<tr>
    <td>Real Name:</td>
    <td><input name="realname" value="%REALNAME%"></td>
</tr>

<tr>
    <td>E-mail address:</td>
    <td><input name="email" value="%EMAIL%"></td>
</tr>

<tr>
    <td>Password:</td>
    <td><input name="password" value="%PASS%"></td>
</tr>

<tr>
    <td>Can Add:</td>
    <td>
	Yes <input type="radio" name="canadd" %CANADD% value="t">
	No  <input type="radio" name="canadd" %CANNOTADD% value="f">
    </td>
</tr>

</table>

<br>

<table border="1">
<tr>
    <th>Category</th>
    <th>Visible</th>
</tr>
