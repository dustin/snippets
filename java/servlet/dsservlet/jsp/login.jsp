<%@ page import="java.util.*" %>
<%@ page import="net.spy.*" %>
<%@ page import="net.spy.dsservlet.*" %>
<%@ page errorPage="errors/auth.jsp" %>

<jsp:useBean id="dsbean" scope="session" class="net.spy.dsservlet.DSBean"/>
<jsp:setProperty name="dsbean" property="*"/>

<%
	response.sendRedirect("list.jsp");
%>
