import java.sql.*;

public class TableList {

	private static void dumpResults(ResultSet rs) throws Exception {
		ResultSetMetaData rsmd=rs.getMetaData();
		int result_no=0;
		while(rs.next()) {
			System.out.println("Result #" + result_no);
			for(int i=1; i<=rsmd.getColumnCount(); i++) {
				String name=rsmd.getColumnName(i);
				String val=rs.getString(i);
				if(name.equals("DATA_TYPE")) {
					int type=rs.getInt(i);
					val+=" (" + net.spy.db.TypeNames.getTypeName(type) + ")";
				}
				System.out.println("\t" + name + "=" + val);
			}
			result_no++;
		}
	}

	public static void main(String args[]) throws Exception
	{

		if(args.length<4) {
			System.err.println("Usage:  TableList driver source user pass");
			throw new Exception("Insufficient arguments.");
		}

		String driver=args[0];
		String source=args[1];
		String username=args[2];
		String password=args[3];

		// Load the postgres driver.
		Class.forName(driver);
		Connection conn = DriverManager.getConnection(source,
			username, password);

		DatabaseMetaData dbmd=conn.getMetaData();

		System.out.println("Capabilities:\n");

		System.out.println("allProceduresAreCallable:  "
			+ dbmd.allProceduresAreCallable());
		System.out.println("allTablesAreSelectable:  "
			+ dbmd.allTablesAreSelectable());
		System.out.println("dataDefinitionCausesTransactionCommit:  "
			+ dbmd.dataDefinitionCausesTransactionCommit());
		System.out.println("dataDefinitionIgnoredInTransactions:  "
			+ dbmd.dataDefinitionIgnoredInTransactions());
		System.out.println("doesMaxRowSizeIncludeBlobs:  "
			+ dbmd.doesMaxRowSizeIncludeBlobs());
		System.out.println("isCatalogAtStart:  "
			+ dbmd.isCatalogAtStart());
		System.out.println("isReadOnly:  "
			+ dbmd.isReadOnly());
		System.out.println("nullPlusNonNullIsNull:  "
			+ dbmd.nullPlusNonNullIsNull());
		System.out.println("nullsAreSortedAtEnd:  "
			+ dbmd.nullsAreSortedAtEnd());
		System.out.println("nullsAreSortedAtStart:  "
			+ dbmd.nullsAreSortedAtStart());
		System.out.println("nullsAreSortedHigh:  "
			+ dbmd.nullsAreSortedHigh());
		System.out.println("nullsAreSortedLow:  "
			+ dbmd.nullsAreSortedLow());
		System.out.println("storesLowerCaseIdentifiers:  "
			+ dbmd.storesLowerCaseIdentifiers());
		System.out.println("storesLowerCaseQuotedIdentifiers:  "
			+ dbmd.storesLowerCaseQuotedIdentifiers());
		System.out.println("storesMixedCaseIdentifiers:  "
			+ dbmd.storesMixedCaseIdentifiers());
		System.out.println("storesMixedCaseQuotedIdentifiers:  "
			+ dbmd.storesMixedCaseQuotedIdentifiers());
		System.out.println("storesUpperCaseIdentifiers:  "
			+ dbmd.storesUpperCaseIdentifiers());
		System.out.println("storesUpperCaseQuotedIdentifiers:  "
			+ dbmd.storesUpperCaseQuotedIdentifiers());
		System.out.println("supportsANSI92EntryLevelSQL:  "
			+ dbmd.supportsANSI92EntryLevelSQL());
		System.out.println("supportsANSI92FullSQL:  "
			+ dbmd.supportsANSI92FullSQL());
		System.out.println("supportsANSI92IntermediateSQL:  "
			+ dbmd.supportsANSI92IntermediateSQL());
		System.out.println("supportsAlterTableWithAddColumn:  "
			+ dbmd.supportsAlterTableWithAddColumn());
		System.out.println("supportsAlterTableWithDropColumn:  "
			+ dbmd.supportsAlterTableWithDropColumn());
		System.out.println("supportsCatalogsInDataManipulation:  "
			+ dbmd.supportsCatalogsInDataManipulation());
		System.out.println("supportsCatalogsInIndexDefinitions:  "
			+ dbmd.supportsCatalogsInIndexDefinitions());
		System.out.println("supportsCatalogsInPrivilegeDefinitions:  "
			+ dbmd.supportsCatalogsInPrivilegeDefinitions());
		System.out.println("supportsCatalogsInProcedureCalls:  "
			+ dbmd.supportsCatalogsInProcedureCalls());
		System.out.println("supportsCatalogsInTableDefinitions:  "
			+ dbmd.supportsCatalogsInTableDefinitions());
		System.out.println("supportsColumnAliasing:  "
			+ dbmd.supportsColumnAliasing());
		System.out.println("supportsConvert:  "
			+ dbmd.supportsConvert());
		System.out.println("supportsCoreSQLGrammar:  "
			+ dbmd.supportsCoreSQLGrammar());
		System.out.println("supportsCorrelatedSubqueries:  "
			+ dbmd.supportsCorrelatedSubqueries());
		System.out.println("supportsDataDefinitionAndDataManipulationTransactions:  "
			+ dbmd.supportsDataDefinitionAndDataManipulationTransactions());
		System.out.println("supportsDataManipulationTransactionsOnly:  "
			+ dbmd.supportsDataManipulationTransactionsOnly());
		System.out.println("supportsDifferentTableCorrelationNames:  "
			+ dbmd.supportsDifferentTableCorrelationNames());
		System.out.println("supportsExpressionsInOrderBy:  "
			+ dbmd.supportsExpressionsInOrderBy());
		System.out.println("supportsExtendedSQLGrammar:  "
			+ dbmd.supportsExtendedSQLGrammar());
		System.out.println("supportsFullOuterJoins:  "
			+ dbmd.supportsFullOuterJoins());
		System.out.println("supportsGroupBy:  "
			+ dbmd.supportsGroupBy());
		System.out.println("supportsGroupByBeyondSelect:  "
			+ dbmd.supportsGroupByBeyondSelect());
		System.out.println("supportsGroupByUnrelated:  "
			+ dbmd.supportsGroupByUnrelated());
		System.out.println("supportsIntegrityEnhancementFacility:  "
			+ dbmd.supportsIntegrityEnhancementFacility());
		System.out.println("supportsLikeEscapeClause:  "
			+ dbmd.supportsLikeEscapeClause());
		System.out.println("supportsLimitedOuterJoins:  "
			+ dbmd.supportsLimitedOuterJoins());
		System.out.println("supportsMinimumSQLGrammar:  "
			+ dbmd.supportsMinimumSQLGrammar());
		System.out.println("supportsMixedCaseIdentifiers:  "
			+ dbmd.supportsMixedCaseIdentifiers());
		System.out.println("supportsMixedCaseQuotedIdentifiers:  "
			+ dbmd.supportsMixedCaseQuotedIdentifiers());
		System.out.println("supportsMultipleResultSets:  "
			+ dbmd.supportsMultipleResultSets());
		System.out.println("supportsMultipleTransactions:  "
			+ dbmd.supportsMultipleTransactions());
		System.out.println("supportsNonNullableColumns:  "
			+ dbmd.supportsNonNullableColumns());
		System.out.println("supportsOpenCursorsAcrossCommit:  "
			+ dbmd.supportsOpenCursorsAcrossCommit());
		System.out.println("supportsOpenCursorsAcrossRollback:  "
			+ dbmd.supportsOpenCursorsAcrossRollback());
		System.out.println("supportsOpenStatementsAcrossCommit:  "
			+ dbmd.supportsOpenStatementsAcrossCommit());
		System.out.println("supportsOpenStatementsAcrossRollback:  "
			+ dbmd.supportsOpenStatementsAcrossRollback());
		System.out.println("supportsOrderByUnrelated:  "
			+ dbmd.supportsOrderByUnrelated());
		System.out.println("supportsOuterJoins:  "
			+ dbmd.supportsOuterJoins());
		System.out.println("supportsPositionedDelete:  "
			+ dbmd.supportsPositionedDelete());
		System.out.println("supportsPositionedUpdate:  "
			+ dbmd.supportsPositionedUpdate());
		System.out.println("supportsSchemasInDataManipulation:  "
			+ dbmd.supportsSchemasInDataManipulation());
		System.out.println("supportsSchemasInIndexDefinitions:  "
			+ dbmd.supportsSchemasInIndexDefinitions());
		System.out.println("supportsSchemasInPrivilegeDefinitions:  "
			+ dbmd.supportsSchemasInPrivilegeDefinitions());
		System.out.println("supportsSchemasInProcedureCalls:  "
			+ dbmd.supportsSchemasInProcedureCalls());
		System.out.println("supportsSchemasInTableDefinitions:  "
			+ dbmd.supportsSchemasInTableDefinitions());
		System.out.println("supportsSelectForUpdate:  "
			+ dbmd.supportsSelectForUpdate());
		System.out.println("supportsStoredProcedures:  "
			+ dbmd.supportsStoredProcedures());
		System.out.println("supportsSubqueriesInComparisons:  "
			+ dbmd.supportsSubqueriesInComparisons());
		System.out.println("supportsSubqueriesInExists:  "
			+ dbmd.supportsSubqueriesInExists());
		System.out.println("supportsSubqueriesInIns:  "
			+ dbmd.supportsSubqueriesInIns());
		System.out.println("supportsSubqueriesInQuantifieds:  "
			+ dbmd.supportsSubqueriesInQuantifieds());
		System.out.println("supportsTableCorrelationNames:  "
			+ dbmd.supportsTableCorrelationNames());
		System.out.println("supportsTransactions:  "
			+ dbmd.supportsTransactions());
		System.out.println("supportsUnion:  "
			+ dbmd.supportsUnion());
		System.out.println("supportsUnionAll:  "
			+ dbmd.supportsUnionAll());
		System.out.println("usesLocalFilePerTable:  "
			+ dbmd.usesLocalFilePerTable());
		System.out.println("usesLocalFiles:  "
			+ dbmd.usesLocalFiles());
		System.out.println("getDefaultTransactionIsolation:  "
			+ dbmd.getDefaultTransactionIsolation());
		System.out.println("getDriverMajorVersion:  "
			+ dbmd.getDriverMajorVersion());
		System.out.println("getDriverMinorVersion:  "
			+ dbmd.getDriverMinorVersion());
		System.out.println("getMaxBinaryLiteralLength:  "
			+ dbmd.getMaxBinaryLiteralLength());
		System.out.println("getMaxCatalogNameLength:  "
			+ dbmd.getMaxCatalogNameLength());
		System.out.println("getMaxCharLiteralLength:  "
			+ dbmd.getMaxCharLiteralLength());
		System.out.println("getMaxColumnNameLength:  "
			+ dbmd.getMaxColumnNameLength());
		System.out.println("getMaxColumnsInGroupBy:  "
			+ dbmd.getMaxColumnsInGroupBy());
		System.out.println("getMaxColumnsInIndex:  "
			+ dbmd.getMaxColumnsInIndex());
		System.out.println("getMaxColumnsInOrderBy:  "
			+ dbmd.getMaxColumnsInOrderBy());
		System.out.println("getMaxColumnsInSelect:  "
			+ dbmd.getMaxColumnsInSelect());
		System.out.println("getMaxColumnsInTable:  "
			+ dbmd.getMaxColumnsInTable());
		System.out.println("getMaxConnections:  "
			+ dbmd.getMaxConnections());
		System.out.println("getMaxCursorNameLength:  "
			+ dbmd.getMaxCursorNameLength());
		System.out.println("getMaxIndexLength:  "
			+ dbmd.getMaxIndexLength());
		System.out.println("getMaxProcedureNameLength:  "
			+ dbmd.getMaxProcedureNameLength());
		System.out.println("getMaxRowSize:  "
			+ dbmd.getMaxRowSize());
		System.out.println("getMaxSchemaNameLength:  "
			+ dbmd.getMaxSchemaNameLength());
		System.out.println("getMaxStatementLength:  "
			+ dbmd.getMaxStatementLength());
		System.out.println("getMaxStatements:  "
			+ dbmd.getMaxStatements());
		System.out.println("getMaxTableNameLength:  "
			+ dbmd.getMaxTableNameLength());
		System.out.println("getMaxTablesInSelect:  "
			+ dbmd.getMaxTablesInSelect());
		System.out.println("getMaxUserNameLength:  "
			+ dbmd.getMaxUserNameLength());
		System.out.println("getCatalogSeparator:  "
			+ dbmd.getCatalogSeparator());
		System.out.println("getCatalogTerm:  "
			+ dbmd.getCatalogTerm());
		System.out.println("getDatabaseProductName:  "
			+ dbmd.getDatabaseProductName());
		System.out.println("getDatabaseProductVersion:  "
			+ dbmd.getDatabaseProductVersion());
		System.out.println("getDriverName:  "
			+ dbmd.getDriverName());
		System.out.println("getDriverVersion:  "
			+ dbmd.getDriverVersion());
		System.out.println("getExtraNameCharacters:  "
			+ dbmd.getExtraNameCharacters());
		System.out.println("getIdentifierQuoteString:  "
			+ dbmd.getIdentifierQuoteString());
		System.out.println("getNumericFunctions:  "
			+ dbmd.getNumericFunctions());
		System.out.println("getProcedureTerm:  "
			+ dbmd.getProcedureTerm());
		System.out.println("getSQLKeywords:  "
			+ dbmd.getSQLKeywords());
		System.out.println("getSchemaTerm:  "
			+ dbmd.getSchemaTerm());
		System.out.println("getSearchStringEscape:  "
			+ dbmd.getSearchStringEscape());
		System.out.println("getStringFunctions:  "
			+ dbmd.getStringFunctions());
		System.out.println("getSystemFunctions:  "
			+ dbmd.getSystemFunctions());
		System.out.println("getTimeDateFunctions:  "
			+ dbmd.getTimeDateFunctions());
		System.out.println("getURL:  "
			+ dbmd.getURL());
		System.out.println("getUserName:  "
			+ dbmd.getUserName());

		System.out.println("---------------------------------");

		/*
		// TMI right now
		System.out.println("Procedures:\n");
		dumpResults(dbmd.getProcedures(null, null, null));
		*/

		System.out.println("Table types:\n");
		dumpResults(dbmd.getTableTypes());

		System.out.println("Tables:\n");
		dumpResults(dbmd.getTables(null, null, null, null));

		/*
		System.out.println("TypeInfo:\n");
		dumpResults(dbmd.getTypeInfo());
		*/

		System.out.println("Schemas:\n");
		dumpResults(dbmd.getSchemas());

		System.out.println("Catalogs:\n");
		dumpResults(dbmd.getCatalogs());

		System.out.println("Native SQL:");
		System.out.println(conn.nativeSQL("insert into blah(col) values(?)"));

		System.out.println("Current catalog:");
		System.out.println(conn.getCatalog());

	}
}
