<%
	dim qsServer
	dim qsPort
	dim qsDatabase
	dim qsUser
	dim qsPassword

	dim qsAction
	dim qsTable
	dim qsId
	dim qsField
	dim qsValue

	qsServer = request.querystring("server")
	qsPort = request.querystring("port")
	qsDatabase = request.querystring("database")
	qsUser = request.querystring("user")
	qsPassword = request.querystring("password")

	qsAction = request.querystring("action")
	qsTable = request.querystring("table")
	qsId = request.querystring("id")
	qsField = request.querystring("field")
	qsValue = request.querystring("value")
	qsCondition = request.querystring("condition")
	qsConcat = request.querystring("concat")

	if qsAction <> "" then
		set conn = server.createobject("adodb.connection")
		conn.open "driver={MySQL ODBC 3.51 Driver};server=" & qsServer & ";port=" & qsPort & ";database=" & qsDatabase & ";user=" & qsUser & ";password=" & qsPassword & ";option=3;"
	end if

	select case qsAction
	case "fieldget"
		'SELECT STATEMENT
		if qsCondition <> "" then
			set rs = server.createobject("adodb.recordset")
			rs.open "select " & qsField & " as 'custfield' from " & qsTable & " where " & qsCondition & ";", conn

			if rs.eof = false then
				do while rs.eof = false
					response.write(rs.fields.item(0) & chr(13) + chr(10))

					rs.movenext
				loop
			end if
		else
			set rs = server.createobject("adodb.recordset")
			rs.open "select " & qsField & " from " & qsTable & ";", conn

			if rs.eof = false then
				do while rs.eof = false
					response.write(rs.fields.item(0) & chr(13) + chr(10))

					rs.movenext
				loop
			end if
		end if
	case "appendblank"
		'INSERT STATEMENT
		conn.execute "insert into " & qsTable & " values();"

		set rs = server.createobject("adodb.recordset")
		rs.open "select max(" & qsId & ") from " & qsTable & ";", conn

		if rs.eof = false then
			response.write(rs.fields.item(0))
		end if

		' -> laatste cRenNo teruggeven
		' -> Veld 'cRecNo' moet auto increment zijn
	case "fieldput"
		'UPDATE STATEMENT
		if qsCondition <> "" then
			if qsConcat <> "0" then
				conn.execute "update " & qsTable & " set " & qsField & " = concat_ws('', " & qsField & ", '" & qsValue & "') where " & qsCondition & ";"
			else
				conn.execute "update " & qsTable & " set " & qsField & " = '" & qsValue & "' where " & qsCondition & ";", conn
			end if
		end if
	case "delete"
		'DELETE STATEMENT
		if qsCondition <> "" then
			conn.execute "delete from " & qsTable & " where " & qsCondition & ";"
		else
			conn.execute "delete from " & qsTable & ";"
		end if
	case else
		response.redirect(".")
	end select
%>