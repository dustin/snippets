<%@ page import="java.util.*" %>
<%@ page import="net.spy.*" %>
<%@ page import="net.spy.dsservlet.*" %>

<jsp:useBean id="dsbean" scope="session" class="net.spy.dsservlet.DSBean"/>

<html><head><title>Show Pickup</title></head>

<body bgcolor="#fFfFfF">

<h1>Unclaimed Shows for <jsp:getProperty name="dsbean"
	property="username"/></h1>

<center>
<table border="0" width="50%">
<tr>
	<th>Show ID</th>
	<th>Size</th>
	<th>Submitted Date</th>
</tr>
<%
	int i=0;
	for(Enumeration e=dsbean.listUnseen(); e.hasMoreElements(); ) {
	Show show=(Show)e.nextElement();
%>

	<% if( (i++ % 2) == 0) { %>
		<tr bgcolor="ffffff">
	<% } else { %>
		<tr bgcolor="cfcfff">
	<% } %>
		<td>
			<a href="/servlet/net.spy.dsservlet.FileServlet/<%=
			show.getShowID() %>?show_id=<%= show.getShowID() %>"><%=
				show.getShowID() %></a>
		</td>
		<td><%= show.getLength() %></td>
		<td><%= show.getSubmittedDate() %></td>
	</tr>

<% } %>

</table>
</center>

<a href="listall.jsp">Show All Movies</a>.

<hr>
Logged in as
<jsp:getProperty name="dsbean" property="username"/>.

</body>
</html>
