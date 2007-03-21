<html><head><title>Add a Photo</title>
%STYLESHEET%
</head>

<body bgcolor="#fFfFfF">

<h2>Add a Photo</h2>

<form method="POST" enctype="multipart/form-data"
action="%SELF_URI%">

<input type="hidden" name="func" value="addimage">

<table>
   <tr>
       <td>Category:</td>
       <td>
	   <select name="category" size="5">
	   	%CAT_LIST%
	   </select>
       </td>
   </tr>
   <tr>
       <td>Date Taken:</td><td><input name="taken" value="%TODAY%"></td>
   </tr>
   <tr>
       <td>Keywords:</td><td><input name="keywords"></td>
   </tr>
   <tr>
       <td>Picture:</td><td><input type="file" name="picture"</td>
   </tr>
</table>

<br>
Short Description:<br>
<textarea name="info" cols="60" rows="5" wrap="hard">
</textarea>
<br>

<input type="submit" value="Add Image">
<input type="reset" value="Clear">

</form>

