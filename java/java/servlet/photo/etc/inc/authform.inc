<html><head><title>Authenticate</title>
</head>

<body bgcolor="#fFfFfF">

<h2>Authenticate</h2>

<form method="POST" action="%SELF_URI%">

<input type="hidden" name="func" value="setcred">

<table>
   <tr>
       <td>Username:</td><td><input name="username" size="8"></td>
   </tr>
   <tr>
       <td>Password:</td><td><input type=password name="password"></td>
   </tr>
</table>

<input type="submit" value="Authenticate">
<input type="reset" value="Clear">

</form>

