<%@ page import="java.util.*" %>
<%@ page import="net.spy.*" %>
<%@ page import="net.spy.dsservlet.*" %>

<jsp:useBean id="dsbean" scope="session" class="net.spy.dsservlet.DSBean"/>

<html><head><title>Show Pickup</title></head>

<body bgcolor="#fFfFfF">

<h1>All Shows for <jsp:getProperty name="dsbean" property="username"/></h1>

<center>
<table border="0" width="50%">
<tr>
	<th>Show ID</th>
	<th>Size</th>
	<th>Submitted Date</th>
	<th>Date Shown</th>
</tr>
<%
	int i=0;
	for(Enumeration e=dsbean.list(); e.hasMoreElements(); ) {
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
		<td>
			<a href="/servlet/net.spy.dsservlet.FileServlet/<%=
			show.getShowID() %>?show_id=<%= show.getShowID() %>"><%=
				show.getShowID() %></a>
		</td>
		<td><%= show.getLength() %></td>
		<td><%= show.getSubmittedDate() %></td>
		<td>
		<% if(show.isComplete()) { %>
			<%= show.getCompletedDate() %>
		<% } else { %>
			<i>Not Shown</i>
		<% } %>
		</td>
	</tr>

<% } %>

</table>
</center>

<hr>
Logged in as
<jsp:getProperty name="dsbean" property="username"/>.

</body>
</html>
