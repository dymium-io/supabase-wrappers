package io.dymium.services

import io.dymium.errors.DbAnalyzerError
import com.github.michaelbull.result.*
import com.google.gson.Gson
import dymium.io.dto.*
import io.github.oshai.kotlinlogging.KotlinLogging
import io.ktor.server.config.*
import io.ktor.server.engine.*
import kotlinx.serialization.json.*
import java.sql.DriverManager
import java.sql.JDBCType
import java.sql.ResultSet

private val logger = KotlinLogging.logger {}


class DbService(val config: ApplicationConfig) {
    suspend fun dbPing(dbInfo: DbConnectDto): Result<DbConnectResponse, DbAnalyzerError> {
        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var msg: String
                        connection.use {
                            msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"
                        }
                        logger.debug { "DB Connection: $url - $msg" }
                        Ok(DbConnectResponse("Ok", msg))
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )
    }

    suspend fun dbSchemas(dbInfo: DbConnectDto): Result<DbSchemasResponse, DbAnalyzerError> {
        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var response: DbSchemasResponse
                        connection.use {
                            val msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"
                            val schemas = mutableListOf<String>()
                            val rsSchemas: ResultSet = it.metaData.schemas
                            while (rsSchemas.next()) {
                                schemas.add(rsSchemas.getString("TABLE_SCHEM"))
                            }
                            if (schemas.isEmpty()) {
                                schemas.add("_defaultdb_")
                            }
                            response = DbSchemasResponse("Ok", msg, schemas)
                        }
                        logger.debug { "DB Schemas: ${response.schemas}" }
                        Ok(response)
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )
    }

    suspend fun dbTabTypes(dbInfo: DbConnectDto): Result<DbTabTypesResponse, DbAnalyzerError> {
        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var response: DbTabTypesResponse
                        connection.use {
                            val msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"
                            //Get a list of table types
                            val tableTypes = mutableListOf<String>()
                            val rsTableTypes: ResultSet = it.metaData.tableTypes
                            while (rsTableTypes.next()) {
                                tableTypes.add(rsTableTypes.getString("TABLE_TYPE"))
                            }
                            response = DbTabTypesResponse("Ok", msg, tableTypes)
                        }
                        logger.debug { "DB Table Types: ${response}" }
                        Ok(response)
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )
    }

    fun convertToJson(resultSet: ResultSet): JsonObject {
        val metaData = resultSet.metaData
        val row = mutableMapOf<String, Any?>()
        for (i in 1..metaData.columnCount) {
            val columnName = metaData.getColumnName(i)
            val value = resultSet.getObject(columnName)
            row[columnName] = value
        }
        // kotlin serialization doesn't have serializer for Any. Using gson for that.
        val gson = Gson()
        return gson.toJson(row).let { Json.parseToJsonElement(it) }.jsonObject
    }

    fun dbSample(dbInfo: DbConnectDto, dbschema: String?, table: String?, sampleSize: Int?): Result<DbSampleResponse, DbAnalyzerError> {
        if (dbschema.isNullOrEmpty()) {
            if (dbInfo.dbType !in listOf("elasticsearch", "es") ) {
                val err = DbAnalyzerError.BadRequest("schema is required")
                logError(err)
                return Err(err)
            }
        }

        val schema = if (dbInfo.dbType in listOf("elasticsearch", "es") && dbschema == "_defaultdb_")
            null
        else
            dbschema

        if (table.isNullOrEmpty()) {
            val err = DbAnalyzerError.BadRequest("table name is required")
            logError(err)
            return Err(err)
        }
        if (sampleSize == null) {
            val err = DbAnalyzerError.BadRequest("sample size is required")
            logError(err)
            return Err(err)
        }


        val query = if (schema.isNullOrEmpty()) {
            "SELECT * FROM $table LIMIT $sampleSize"
        } else {
            "SELECT * FROM $schema.$table LIMIT $sampleSize"
        }

        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var response: DbSampleResponse
                        connection.use {
                            val msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"

                            val statement = connection.createStatement()
                            val resultSet = statement.executeQuery(query)
                            val jsonArray = mutableListOf<JsonObject>()
                            while (resultSet.next()) {
                                jsonArray.add((convertToJson(resultSet)))
                            }
                            response = DbSampleResponse("Ok", msg, data = jsonArray)
                        }
                        //logger.debug { "DB Sample: ${response}" }
                        Ok(response)
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )

    }


    suspend fun dbTabList(dbInfo: DbConnectDto, schema: String?): Result<DbTabListResponse, DbAnalyzerError> {
        if (schema.isNullOrEmpty() && dbInfo.dbType !in listOf("elasticsearch", "es")) {
            val err = DbAnalyzerError.BadRequest("schema is required")
            logError(err)
            return Err(err)
        }

        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var response: DbTabListResponse
                        connection.use {
                            val msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"
                            // Get list of table types from config for this dbType
                            val tableTypes =
                                (config.propertyOrNull("datasources.${dbInfo.dbType}.tableTypes")?.getList()
                                    ?: listOf("TABLE", "VIEW")).toTypedArray()
                            // Get a list of all tables and views in the database
                            val tables = mutableListOf<String>()
                            val resultSet: ResultSet = it.metaData.getTables(
                                null,
                                schema,
                                "%",
                                tableTypes
                            )
                            while (resultSet.next()) {
                                tables.add(resultSet.getString("TABLE_NAME"))
                            }
                            response = DbTabListResponse("Ok", msg, tables)
                        }
                        //logger.debug { "DB Schemas: ${response}" }
                        Ok(response)
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )
    }

    suspend fun dbColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?
    ): Result<DbColListResponse, DbAnalyzerError> {
        if (schema.isNullOrEmpty() && dbInfo.dbType !in listOf("elasticsearch", "es")) {
            val err = DbAnalyzerError.BadRequest("schema is required")
            logError(err)
            return Err(err)
        }
        if (tabname.isNullOrEmpty()) {
            val err = DbAnalyzerError.BadRequest("table name is required")
            logError(err)
            return Err(err)
        }

        return validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }
                    try {
                        val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
                        var response: DbColListResponse
                        connection.use {
                            val msg =
                                "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"
                            // Get a list of all tables and views in the database
                            val columns = mutableListOf<DbColumnInfo>()
                            val rsColumns = it.metaData.getColumns(null, schema, tabname, null)
                            while (rsColumns.next()) {
                                columns.add(
                                    DbColumnInfo(
                                        tableCat = rsColumns.getString("TABLE_CAT"),
                                        tableSchema = rsColumns.getString("TABLE_SCHEM"),
                                        tableName = rsColumns.getString("TABLE_NAME"),
                                        columnName = rsColumns.getString("COLUMN_NAME"),
                                        dataType = rsColumns.getInt("DATA_TYPE"),
                                        dataTypeStr = rsColumns.getInt("DATA_TYPE").let {
                                            try { JDBCType.valueOf(it).getName() }
                                            catch (e: Exception) { "unknown" }
                                        },
                                        typeName = rsColumns.getString("TYPE_NAME"),
                                        columnSize = rsColumns.getInt("COLUMN_SIZE"),
                                        decimalDigits = rsColumns.getInt("DECIMAL_DIGITS"),
                                        numPrecRadix = rsColumns.getInt("NUM_PREC_RADIX"),
                                        nullable = rsColumns.getInt("NULLABLE"),
                                        remarks = rsColumns.getString("REMARKS"),
                                        columnDef = rsColumns.getString("COLUMN_DEF"),
                                        charOctetLength = rsColumns.getInt("CHAR_OCTET_LENGTH"),
                                        ordinalPosition = rsColumns.getInt("ORDINAL_POSITION"),
                                        isNullable = rsColumns.getString("IS_NULLABLE"),
                                        scopeCatalog = rsColumns.getString("SCOPE_CATALOG"),
                                        scopeSchema = rsColumns.getString("SCOPE_SCHEMA"),
                                        scopeTable = rsColumns.getString("SCOPE_TABLE"),
                                        sourceData = rsColumns.getInt("SOURCE_DATA_TYPE"),
                                        sourceDataStr = rsColumns.getInt("SOURCE_DATA_TYPE").let {
                                            if (it == 0) null
                                            else
                                                try {JDBCType.valueOf(it).getName()}
                                                catch (e: Exception) {"unknown"}
                                        },
                                        isAutoIncrement = rsColumns.getString("IS_AUTOINCREMENT"),
                                        isGeneratedColumn = rsColumns.getString("IS_GENERATEDCOLUMN"),
                                    )
                                )
                            }
                            response = DbColListResponse("Ok", msg, columns)
                        }
                        //logger.debug { "DB Schemas: ${response}" }
                        Ok(response)
                    } catch (e: Exception) {
                        val err = DbAnalyzerError.BadRequest("Error connecting to database: ${e.message}")
                        logError(err)
                        Err(err)
                    }
                },
                failure = { err ->
                    logError(err)
                    Err(err)
                }
            )
    }

    private fun getConnectionURL(dbInfo: DbConnectDto): String {
        // TODO: Other ways to connect to DB: API key, SSL/TLS, etc. as well as dbType is known
        return when (dbInfo.dbType) {
            "sqlserver" -> {
                // Creating SQL Server Connection URL
                // FIXME - for now we hard code the encryption to false
                "jdbc:sqlserver://${dbInfo.host}:${dbInfo.port};databaseName=${dbInfo.database};encrypt=false"
            }
            "oracle" -> {
                // Creating Oracle Connection URL
                // FIXME - for now we hard code the encryption to false
                "jdbc:oracle:thin:@${dbInfo.host}:${dbInfo.port}/${dbInfo.database}"
            }
            "elasticsearch", "es" -> {
                // Creating Elasticsearch Connection URL
                // FIXME - for now we hard code the encryption to false
                if (dbInfo.database.isNullOrEmpty()) {
                    "jdbc:elasticsearch://${dbInfo.host}:${dbInfo.port}"
                } else {
                    // TODO: This is Elasticsearch feature in preview, can be removed in the future
                    "jdbc:elasticsearch://${dbInfo.host}:${dbInfo.port}/?catalog=${dbInfo.database}"
                }
            }
            else -> {
                var url = "jdbc:${dbInfo.dbType}://${dbInfo.host}:${dbInfo.port}/${dbInfo.database}"
                if (!dbInfo.properties.isNullOrEmpty())
                    url = "$url?${dbInfo.properties}"
                url
            }
        }
    }

    private fun logError(error: DbAnalyzerError): Unit =
        when (error) {
            is DbAnalyzerError.BadRequest -> {
                logger.error { "Bad Request: ${error.message}" }
            }

            is DbAnalyzerError.NotFound -> {
                logger.error { "Not Found: ${error.message}" }
            }

            is DbAnalyzerError.Unauthorized -> {
                logger.error { "Unauthorized: ${error.message}" }
            }

            is DbAnalyzerError.Forbidden -> {
                logger.error { "Forbidden: ${error.message}" }
            }
        }

    private fun validateRequestDbInfo(dbInfo: DbConnectDto): Result<DbConnectDto, DbAnalyzerError> =
        when {
            //TODO validate dbType - is known
            dbInfo.host.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("Host is required")
                Err(err)
            }

            dbInfo.dbType.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("dbType is required")
                Err(err)
            }

            dbInfo.dbType !in config.property("datasources.knownDBs").getList() -> {
                val err = DbAnalyzerError.BadRequest("dbType is not known")
                Err(err)
            }

            dbInfo.database.isNullOrEmpty() && dbInfo.dbType !in listOf("elasticsearch", "es")-> {
                val err = DbAnalyzerError.BadRequest("database is required")
                Err(err)
            }

            dbInfo.user.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("user is required")
                Err(err)
            }

            dbInfo.password.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("password is required")
                Err(err)
            }

            else -> Ok(dbInfo)
        }
}