package io.dymium.services

import com.github.michaelbull.result.Err
import com.github.michaelbull.result.Ok
import com.github.michaelbull.result.Result
import com.github.michaelbull.result.mapBoth
import com.google.gson.Gson
import dymium.io.dto.*
import io.dymium.errors.DbAnalyzerError
import io.github.oshai.kotlinlogging.KotlinLogging
import io.ktor.server.config.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonObject
import java.sql.DriverManager
import java.sql.JDBCType
import java.sql.ResultSet
import kotlin.collections.List
import kotlin.collections.MutableList
import kotlin.collections.contains
import kotlin.collections.listOf
import kotlin.collections.mutableListOf
import kotlin.collections.mutableMapOf
import kotlin.collections.set
import kotlin.collections.toTypedArray

private val logger = KotlinLogging.logger {}


/**
 * Class used to validate database connection information and handle database connection errors.
 *
 * @property config The application configuration object.
 */
open class DbValidator(val config: ApplicationConfig) {
    fun validateRequestDbInfo(dbInfo: DbConnectDto): Result<DbConnectDto, DbAnalyzerError> {
        return when {
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

            dbInfo.database.isNullOrEmpty() && dbInfo.dbType !in listOf("elasticsearch", "es") -> {
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

    fun getConnectionURL(dbInfo: DbConnectDto): String {
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
                val protocol = if (dbInfo.tls) "https" else "http"
                if (dbInfo.database.isNullOrEmpty()) {
                    "jdbc:elasticsearch://${protocol}://${dbInfo.host}:${dbInfo.port}"
                } else {
                    // TODO: This is Elasticsearch feature in preview, can be removed in the future
                    "jdbc:elasticsearch://${protocol}://${dbInfo.host}:${dbInfo.port}/?catalog=${dbInfo.database}"
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

    fun handleError(errorMessage: String): Err<DbAnalyzerError> {
        val error = DbAnalyzerError.BadRequest(errorMessage)
        logError(error)
        return Err(error)
    }

    private fun logError(error: DbAnalyzerError) {
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
    }

}

class ExtendedDbValidator(config: ApplicationConfig) : DbValidator(config) {

    fun <S> validateAndHandle(
        dbInfo: DbConnectDto,
        handleSuccess: (java.sql.Connection) -> Result<S, DbAnalyzerError>
    ): Result<S, DbAnalyzerError> {
        return validateRequestDbInfo(dbInfo).mapBoth(
            success = { handleDbConnection(it, handleSuccess) },
            failure = { handleError(it.message) }
        )
    }

    private fun <S> handleDbConnection(
        dbInfo: DbConnectDto,
        handleSuccess: (java.sql.Connection) -> Result<S, DbAnalyzerError>
    ): Result<S, DbAnalyzerError> {
        val url = getConnectionURL(dbInfo)
        return try {
            DriverManager.getConnection(url, dbInfo.user, dbInfo.password).use { handleSuccess(it) }
        } catch (e: Exception) {
            handleError("Error connecting to database: ${e.message}")
        }
    }
}

class DbService(config: ApplicationConfig) {

    private val validator = ExtendedDbValidator(config)
    private val SUCCESS_RESPONSE = "Ok"

    private fun <T, S, E> handleRequest(
        dbInfo: DbConnectDto,
        handleSuccess: (java.sql.Connection) -> Result<S, E>
    ): Result<S, DbAnalyzerError> where T : Result<S, E>, S : Any, E : DbAnalyzerError {
        return validator.validateAndHandle(dbInfo, handleSuccess)
    }

    private fun getDatabaseMessage(it: java.sql.Connection) =
        "Database Name: ${it.metaData.databaseProductName} (${it.metaData.databaseProductVersion})"

    fun dbPing(dbInfo: DbConnectDto): Result<DbConnectResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val msg = getDatabaseMessage(it)
            logger.debug { "DB Connection: ${validator.getConnectionURL(dbInfo)} - $msg" }
            Ok(DbConnectResponse(SUCCESS_RESPONSE, msg))
        }
    }

    fun dbSchemas(dbInfo: DbConnectDto): Result<DbSchemasResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val msg = getDatabaseMessage(it)
            val schemas = mutableListOf<String>()
            val rsSchemas: ResultSet = it.metaData.schemas
            while (rsSchemas.next()) {
                schemas.add(rsSchemas.getString("TABLE_SCHEM"))
            }
            if (schemas.isEmpty()) {
                schemas.add("_defaultdb_")
            }
            Ok(DbSchemasResponse(SUCCESS_RESPONSE, msg, schemas))
        }
    }

    fun dbTabTypes(dbInfo: DbConnectDto): Result<DbTabTypesResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val msg = getDatabaseMessage(it)
            //Get a list of table types
            val tableTypes = mutableListOf<String>()
            val rsTableTypes: ResultSet = it.metaData.tableTypes
            while (rsTableTypes.next()) {
                tableTypes.add(rsTableTypes.getString("TABLE_TYPE"))
            }
            Ok(DbTabTypesResponse(SUCCESS_RESPONSE, msg, tableTypes))
        }
    }

    private fun convertToJson(resultSet: ResultSet): JsonObject {
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

    fun dbSample(
        dbInfo: DbConnectDto,
        dbschema: String?,
        table: String?,
        sampleSize: Int?,
    ): Result<DbSampleResponse, DbAnalyzerError> {
        val isElasticsearchDb = dbInfo.dbType in listOf("elasticsearch", "es")
        val defaultDb = "_defaultdb_"
        val schema = handleDefaultDbCase(isElasticsearchDb, dbschema, defaultDb)
        val columns = dbInfo.columns ?: ""

        val checkResult = checkRequiredData(dbschema, table, sampleSize, isElasticsearchDb)
        if (checkResult is Err) return checkResult

        val query = createQueryStatement(schema, table, sampleSize, columns)
  
        return validator.validateRequestDbInfo(dbInfo).mapBoth(
            success = { dbInfo -> processDbRequest(query, dbInfo) },
            failure = { err -> validator.handleError(err.message) }
        )
    }

    private fun processDbRequest(query: String, dbInfo: DbConnectDto): Result<DbSampleResponse, DbAnalyzerError> {
        val url = validator.getConnectionURL(dbInfo)
        logger.debug { "DB Connection URL: $url" }
        return try {
            val connection = DriverManager.getConnection(url, dbInfo.user, dbInfo.password)
            connection.use { processConnection(it, query) }
        } catch (e: Exception) {
            validator.handleError("Error connecting to database: ${e.message}")
        }
    }

    private fun processConnection(
        connection: java.sql.Connection,
        query: String
    ): Result<DbSampleResponse, DbAnalyzerError> {
        val msg = getDatabaseMessage(connection)
        val jsonArray = executeQueryAndFetchData(connection, query)
        return createSampleResponse(msg, jsonArray)
    }

    private fun executeQueryAndFetchData(connection: java.sql.Connection, query: String): MutableList<JsonObject> {
        val statement = connection.createStatement()
        val resultSet = statement.executeQuery(query)
        return fillDataArray(resultSet)
    }

    private fun createSampleResponse(
        message: String,
        data: MutableList<JsonObject>
    ): Result<DbSampleResponse, DbAnalyzerError> {
        val response = DbSampleResponse(SUCCESS_RESPONSE, message, data)
        return Ok(response)
    }

    private fun fillDataArray(resultSet: ResultSet): MutableList<JsonObject> {
        val jsonArray = mutableListOf<JsonObject>()
        while (resultSet.next()) {
            jsonArray.add((convertToJson(resultSet)))
        }
        return jsonArray
    }

    private fun checkRequiredData(
        dbschema: String?,
        table: String?,
        sampleSize: Int?,
        isElasticsearchDb: Boolean
    ): Result<Unit, DbAnalyzerError> {
        if (dbschema.isNullOrEmpty() && !isElasticsearchDb) return validator.handleError("schema is required")

        if (table.isNullOrEmpty()) return validator.handleError("table name is required")

        if (sampleSize == null) return validator.handleError("sample size is required")

        return Ok(Unit)
    }

    private fun handleDefaultDbCase(isElasticsearchDb: Boolean, dbschema: String?, defaultDb: String) =
        if (isElasticsearchDb && dbschema == defaultDb) null else dbschema


    private fun createQueryStatement(schema: String?, table: String?, sampleSize: Int?, columns: String?): String {
        val cols = if (columns.isNullOrEmpty()) "*" else columns
        return if (schema.isNullOrEmpty()) {
            "SELECT $cols FROM $table LIMIT $sampleSize"
        } else {
            "SELECT $cols FROM $schema.$table LIMIT $sampleSize"
        }
    }


    fun dbTabList(dbInfo: DbConnectDto, schema: String?): Result<DbTabListResponse, DbAnalyzerError> {
        if (schema.isNullOrEmpty() && dbInfo.dbType !in listOf("elasticsearch", "es")) {
            return validator.handleError("schema is required")
        }

        return validator.validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { validatedDbInfo ->
                    val url = validator.getConnectionURL(dbInfo)
                    logger.debug { "DB Connection URL: $url" }

                    try {
                        val connection =
                            DriverManager.getConnection(url, validatedDbInfo.user, validatedDbInfo.password)
                        var response: DbTabListResponse

                        connection.use {
                            val msg = getDatabaseMessage(it)
                            val tableTypes =
                                getTableTypesFromConfigOrDefault(validatedDbInfo.dbType!!) // It's been validated
                            val tables = getAllDatabaseTables(it, tableTypes, schema)
                            response = createDbTabListResponse(msg, tables)
                        }

                        Ok(response)
                    } catch (e: Exception) {
                        validator.handleError("Error connecting to database: ${e.message}")
                    }
                },
                failure = { error ->
                    validator.handleError(error.message)
                }
            )
    }

    private fun getTableTypesFromConfigOrDefault(dbType: String): Array<String> {
        val tableTypes =
            (validator.config.propertyOrNull("datasources.$dbType.tableTypes")?.getList() ?: listOf("TABLE", "VIEW"))
        return tableTypes.toTypedArray()
    }

    private fun getAllDatabaseTables(
        connection: java.sql.Connection,
        tableTypes: Array<String>,
        schema: String?
    ): MutableList<String> {
        val tables = mutableListOf<String>()
        val resultSet: ResultSet = connection.metaData.getTables(null, schema, "%", tableTypes)

        while (resultSet.next()) {
            tables.add(resultSet.getString("TABLE_NAME"))
        }

        return tables
    }

    private fun createDbTabListResponse(message: String, tables: List<String>): DbTabListResponse {
        return DbTabListResponse("Ok", message, tables)
    }

    fun dbColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?
    ): Result<DbColListResponse, DbAnalyzerError> {
        validateParameters(schema, tabname, dbInfo.dbType)?.let { error ->
            return validator.handleError(error.message)
        }

        return validator.validateRequestDbInfo(dbInfo)
            .mapBoth(
                success = { dbInfo ->
                    val url = validator.getConnectionURL(dbInfo)
                    try {
                        val connection = getConnection(url, dbInfo.user, dbInfo.password)
                        return prepareDbColListResponse(connection, schema, tabname)
                    } catch (e: Exception) {
                        validator.handleError("Error connecting to database: ${e.message}")
                    }
                },
                failure = { error ->
                    validator.handleError(error.message)
                }
            )
    }

    private fun validateParameters(
        schema: String?,
        tabname: String?,
        dbType: String?
    ): DbAnalyzerError? {
        if (schema.isNullOrEmpty() && dbType !in listOf("elasticsearch", "es")) {
            return DbAnalyzerError.BadRequest("schema is required")
        }
        if (tabname.isNullOrEmpty()) {
            return DbAnalyzerError.BadRequest("table name is required")
        }
        return null
    }

    private fun getConnection(url: String, user: String?, password: String?): java.sql.Connection {
        return DriverManager.getConnection(url, user, password)
    }

    private fun prepareDbColListResponse(
        connection: java.sql.Connection,
        schema: String?,
        tabname: String?
    ): Result<DbColListResponse, DbAnalyzerError> {
        var response: DbColListResponse
        connection.use {
            val msg = getDatabaseMessage(it)
            val columns = retrieveColumns(it, schema, tabname)
            response = DbColListResponse(SUCCESS_RESPONSE, msg, columns)
        }
        return Ok(response)
    }

    private fun retrieveColumns(
        connection: java.sql.Connection,
        schema: String?,
        tabname: String?
    ): MutableList<DbColumnInfo> {
        val columns = mutableListOf<DbColumnInfo>()
        val rsColumns = connection.metaData.getColumns(null, schema, tabname, null)

        while (rsColumns.next()) {
            columns.add(
                DbColumnInfo(
                    tableCat = rsColumns.getString("TABLE_CAT"),
                    tableSchema = rsColumns.getString("TABLE_SCHEM"),
                    tableName = rsColumns.getString("TABLE_NAME"),
                    columnName = rsColumns.getString("COLUMN_NAME"),
                    dataType = rsColumns.getInt("DATA_TYPE"),
                    dataTypeStr = rsColumns.getInt("DATA_TYPE").let {
                        try {
                            JDBCType.valueOf(it).getName()
                        } catch (e: Exception) {
                            "unknown"
                        }
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
                            try {
                                JDBCType.valueOf(it).getName()
                            } catch (e: Exception) {
                                "unknown"
                            }
                    },
                    isAutoIncrement = rsColumns.getString("IS_AUTOINCREMENT"),
                    isGeneratedColumn = rsColumns.getString("IS_GENERATEDCOLUMN"),
                )
            )
        }
        return columns
    }
}