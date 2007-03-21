<html><head><title>Set Your Style</title>
%STYLESHEET%
</head>

<body bgcolor="#fFfFfF">

<h2>Set Your Style</h2>

<form method="POST"
action="%SELF_URI%">

<input type=hidden name=func value=setstyle>

<table>
   <tr>
       <td>Font:</td>
       <td>
	   <select name="font">
	   <option>Arial
	   <option>Helvetica
	   <option>cursive
	   <option>fantasy
	   <option>sans-serif
	   <option>serif
	   </select>
       </td>
   </tr>

   <tr>
       <td>Header Transform:</td>
	   <td>
	   <select name="h_transform">
	   <option>none
	   <option>capitalize
	   <option>uppercase
	   <option>lowercase
	   </select>
	   </td>
   </tr>

   <tr>
       <td>Description Transform:</td>
	   <td>
	   <select name="d_transform">
	   <option>none
	   <option>capitalize
	   <option>uppercase
	   <option>lowercase
	   </select>
	   </td>
   </tr>

   <tr>
   		<td>Background Color:</td>
		<td>
			<select name="bgcolor">
			<option>white
			<option>red
			<option>green
			<option>blue
			<option value="#ccffcc">TUMS green
			<option value="#ccccff">Light blue
			<option value="#ffcccc">Nasty pink
			</select>
		</td>
   </tr>
</table>

<input type="submit" value="Set Style">
<input type="reset" value="Clear">

</form>

</body></html>
