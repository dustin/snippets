<%@ page import="java.util.*" %>
<%@ page import="net.spy.*" %>
<%@ page import="net.spy.util.*" %>
<%@ page import="net.spy.dsservlet.*" %>

<jsp:useBean id="dsbean" scope="session" class="net.spy.dsservlet.DSBean"/>

<html><head><title>Show All Movie Distributions</title></head>

<body bgcolor="#fFfFfF">

<h1>All Distributions</h1>

<center>
<table border="0">
<tr>
	<th>Show ID</th>
	<th>Submitted To</th>
	<th>Size</th>
	<th>Submitted Date</th>
	<th>Complete Date</th>
	<th>Time to Completion</th>
</tr>
<%
	int i=0;
	for(Enumeration e=dsbean.listAll(); e.hasMoreElements(); ) {
	Show show=(Show)e.nextElement();
%>

	<% if(! show.isComplete() ) { %>
		<tr bgcolor="ff3f3f">
	<% } else { %>
		<% if( (i++ % 2) == 0) { %>
			<tr bgcolor="ffffff">
		<% } else { %>
			<tr bgcolor="cfcfff">
		<% } %>
	<% } %>

		<td><%= show.getShowID() %></a></td>
		<td><%= show.getSubittedTo() %></a></td>
		<td><%= show.getLength() %></td>
		<td><%= show.getSubmittedDate() %></td>
		<td>
		<% if(show.isComplete()) { %>
			<%= show.getCompletedDate() %>
			</td>
			<td>
				<%= new TimeSpan(show.getSubmittedDate(),
					show.getCompletedDate()) %>
		<% } else { %>
			<i>Not Shown</i>
			</td>
			<td>
				&nbsp;
		<% } %>
		</td>
	</tr>

<% } %>

</table>
</center>

</body>
</html>
