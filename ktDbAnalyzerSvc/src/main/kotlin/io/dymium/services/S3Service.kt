package io.dymium.services

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.JsonNodeFactory
import com.github.michaelbull.result.*
import dymium.io.dto.*
import io.dymium.errors.DbAnalyzerError
import io.github.oshai.kotlinlogging.KotlinLogging
import io.ktor.server.config.*
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.s3a.S3AFileSystem
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.Logger
import org.apache.parquet.hadoop.ParquetFileReader
import org.apache.parquet.hadoop.metadata.ParquetMetadata
import org.apache.parquet.hadoop.util.HadoopInputFile
import org.apache.spark.SparkConf
import org.apache.spark.api.java.JavaSparkContext
import org.apache.spark.sql.SQLContext
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.GetObjectRequest
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request
import java.nio.charset.StandardCharsets
import java.time.DayOfWeek
import java.time.Month
import com.saasquatch.jsonschemainferrer.*
import kotlinx.serialization.json.*
import java.net.URLEncoder
import java.util.Collections.emptyIterator


private val logger = KotlinLogging.logger {}

class S3ExtendedDbValidator(config: ApplicationConfig) : DbValidator(config) {

    fun <S> validateAndHandle(
        dbInfo: DbConnectDto,
        handleSuccess: (S3Client) -> Result<S, DbAnalyzerError>
    ): Result<S, DbAnalyzerError> {
        return validateRequestDbInfo(dbInfo).mapBoth(
            success = { handleS3Connection(it, handleSuccess) },
            failure = { handleError(it.message) }
        )
    }

    private fun <S> handleS3Connection(
        dbInfo: DbConnectDto,
        handleSuccess: (S3Client) -> Result<S, DbAnalyzerError>
    ): Result<S, DbAnalyzerError> {
        val credentials = AwsBasicCredentials.create(dbInfo.user, dbInfo.password)
        val region = Region.of(dbInfo.properties)
        val s3 = S3Client.builder()
            .region(region)
            .credentialsProvider { credentials }
            .build()
        return try {
            s3.headBucket { it.bucket(dbInfo.database) }
            handleSuccess(s3)
        } catch (e: Exception) {
            handleError("Bucket ${dbInfo.database} does not exist or you do not have access to it: ${e.message}")
        }
    }

    override fun validateRequestDbInfo(dbInfo: DbConnectDto): Result<DbConnectDto, DbAnalyzerError> {
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

            dbInfo.database.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("bucket is required")
                Err(err)
            }

            dbInfo.user.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("access key is required")
                Err(err)
            }

            dbInfo.password.isNullOrEmpty() -> {
                val err = DbAnalyzerError.BadRequest("secret key is required")
                Err(err)
            }

            else -> Ok(dbInfo)
        }
    }

}


class S3Service(config: ApplicationConfig) : IDbService {
    private val SUCCESS_RESPONSE = "Ok"
    private val validator = S3ExtendedDbValidator(config)

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

    private fun <T, S, E> handleRequest(
        dbInfo: DbConnectDto,
        handleSuccess: (S3Client) -> Result<S, E>
    ): Result<S, DbAnalyzerError> where T : Result<S, E>, S : Any, E : DbAnalyzerError {
        return validator.validateAndHandle(dbInfo, handleSuccess)
    }


    override fun dbPing(dbInfo: DbConnectDto): Result<DbConnectResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val msg = it.headBucket { it.bucket(dbInfo.database) }
            logger.debug { "S3 Connection:  $msg" }
            Ok(DbConnectResponse(SUCCESS_RESPONSE, "Bucket ${dbInfo.database} exests."))
        }
    }

    override fun dbSchemas(dbInfo: DbConnectDto): Result<DbSchemasResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val req = ListObjectsV2Request.builder().bucket(dbInfo.database).build()
            val schemas = mutableListOf<String>()
            schemas.add("/")
            it.listObjectsV2(req).contents().forEach { s3Object ->
                val objKey = s3Object.key()
                if (objKey.endsWith("/")) schemas.add(objKey)
            }
            Ok(DbSchemasResponse("Ok", "Bucket: ${dbInfo.database}", schemas))
        }

    }

    override fun dbTabTypes(dbInfo: DbConnectDto): Result<DbTabTypesResponse, DbAnalyzerError> {
        return Err(DbAnalyzerError.BadRequest("Not implemented - this doesn't have any meaning in S3 context."))
    }

    enum class FileType {
        CSV,
        JSON,
        PARQUET,
        UNKNOWN
    }


    private fun getFiletype(filename: String?): FileType {
        return when {
            filename == null -> FileType.UNKNOWN
            filename.endsWith(".csv") -> FileType.CSV
            filename.endsWith(".json") || filename.endsWith(".jsonl") -> FileType.JSON
            filename.endsWith(".parquet") -> FileType.PARQUET
            else -> FileType.UNKNOWN
        }
    }

    override fun dbSample(
        dbInfo: DbConnectDto,
        dbschema: String?,
        table: String?,
        sampleSize: Int?
    ): Result<DbSampleResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            when (getFiletype(table)) {
                FileType.CSV -> getCSVSample(dbInfo, dbschema, table, sampleSize)
                FileType.JSON -> getJsonSample(dbInfo, dbschema, table, sampleSize)
                FileType.PARQUET -> getParquetSample(dbInfo, dbschema, table, sampleSize)
                FileType.UNKNOWN -> Err(DbAnalyzerError.BadRequest("Unsupported file type."))
            }
        }
    }

    private fun getCSVSample(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbSampleResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }
        if (sampleSize == null || sampleSize <= 1) {
            return Err(DbAnalyzerError.BadRequest("Sample size must be greater than 1 (Line 0 is header)."))
        }

        val fileName = if (schema.isNullOrEmpty() || schema.equals("/")) tabname else {
            if (schema.endsWith("/")) "$schema$tabname" else "$schema/$tabname"
        }
        val rows = mutableListOf<JsonObject>()

        return try {
            val s3 = createS3Client(dbInfo)
            val getObjectRequest = GetObjectRequest.builder()
                .bucket(dbInfo.database)
                .key(fileName)
                .build()
            val obj = s3.getObject(getObjectRequest)

            obj.readAllBytes()
                .toString(StandardCharsets.UTF_8)
                .lines()
                .filter { it.isNotBlank() }
                .let { lines ->
                    val size = minOf(sampleSize, lines.size)
                    lines.take(size)
                }.let { lines ->
                    val csvHeader = lines[0].split(",")
                    val csvData = lines.drop(1)
                    csvData.map { dataRow ->
                        val data = dataRow.split(",")
                        if (data.size == csvHeader.size) {
                            val jsonObject = buildJsonObject {
                                csvHeader.zip(data).forEach { (key, value) ->
                                    put(key, value)
                                }
                            }
                            rows.add(jsonObject)
                        }
                    }
                }
            Ok(DbSampleResponse(SUCCESS_RESPONSE, "Columns", rows))
        } catch (e: Exception) {
            Err(DbAnalyzerError.BadRequest(e.message ?: ""))
        }
    }

    private fun getJsonSample(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbSampleResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }
        if (sampleSize == null || sampleSize <= 0) {
            return Err(DbAnalyzerError.BadRequest("Sample size must be greater than zero."))
        }

        val fileName = if (schema.isNullOrEmpty() || schema.equals("/")) tabname else {
            if (schema.endsWith("/")) "$schema$tabname" else "$schema/$tabname"
        }
        val rows = mutableListOf<JsonObject>()

        return try {
            val s3 = createS3Client(dbInfo)
            val getObjectRequest = GetObjectRequest.builder()
                .bucket(dbInfo.database)
                .key(fileName)
                .build()
            val obj = s3.getObject(getObjectRequest)

            obj.readAllBytes()
                .toString(StandardCharsets.UTF_8)
                .lines()
                .filter { it.isNotBlank() }
                .let { lines ->
                    val size = minOf(sampleSize, lines.size)
                    lines.take(size)
                }.let { lines ->
                    lines.map {
                        Json.parseToJsonElement(it).jsonObject
                    }.let {
                        rows.addAll(it)
                    }
                }
            Ok(DbSampleResponse(SUCCESS_RESPONSE, "Columns", rows))
        } catch (e: Exception) {
            Err(DbAnalyzerError.BadRequest(e.message ?: ""))
        }
    }

    object SparkContextHolder {
        val conf by lazy {
            SparkConf()
                .setAppName("App")
                .setMaster("local[2]")
                .set("spark.sql.legacy.parquet.nanosAsLong", "true")
        }
        val sc by lazy { JavaSparkContext(conf) }
    }

    private fun getParquetSample(
        dbInfo: DbConnectDto,
        dbschema: String?,
        table: String?,
        sampleSize: Int?
    ): Result<DbSampleResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }
        // Make use of the globally initialized SparkContext
        val sc = SparkContextHolder.sc
        // Set log level to INFO
        sc.setLogLevel("INFO")
        // Set Hadoop's logger to INFO
        Logger.getLogger("org.apache").level = Level.INFO
        val jettyLogger = LogManager.getLogger("org.sparkproject.jetty")
        jettyLogger.level = Level.INFO
        val rootLogger = LogManager.getRootLogger()
        rootLogger.level = Level.INFO

        sc.hadoopConfiguration().set("fs.s3a.access.key", dbInfo.user)
        sc.hadoopConfiguration().set("fs.s3a.secret.key", dbInfo.password)
        sc.hadoopConfiguration().set("fs.s3a.impl", "org.apache.hadoop.fs.s3a.S3AFileSystem")
        sc.hadoopConfiguration().set("com.amazonaws.services.s3.enableV4", "true")
        sc.hadoopConfiguration().set("fs.s3a.endpoint", "s3.amazonaws.com")

        val sqlContext = SQLContext(sc)

        val fileName = if (dbschema.isNullOrEmpty() || dbschema.equals("/")) table else {
            if (dbschema.endsWith("/")) "$dbschema$table" else "$dbschema/$table"
        }

        // Construct the S3 path
        val s3Path = "s3a://${dbInfo.database}/$fileName"
        // Step 2: Load the Parquet file into a DataFrame
        val df = sqlContext.read().parquet(s3Path)

        // Step 3: Take the desired number of samples from the DataFrame and print
        val rows = sampleSize?.let { df.takeAsList(it) }
        val samples = mutableListOf<JsonObject>()
        if (rows != null) {
            for (row in rows) {
                samples.add(Json.parseToJsonElement((row.json())).jsonObject)
            }
        }
        return Ok(DbSampleResponse(SUCCESS_RESPONSE, "Sample data", samples))
    }

    override fun dbTabList(dbInfo: DbConnectDto, schema: String?): Result<DbTabListResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            val req = ListObjectsV2Request.builder().bucket(dbInfo.database).build()
            val tables = mutableListOf<String>()
            it.listObjectsV2(req).contents().forEach { s3Object ->
                val objKey = s3Object.key()
                if (schema.isNullOrEmpty() || schema.equals("/")) {
                    if (!objKey.endsWith("/") && !objKey.contains("/")) {
                        tables.add(objKey)
                    }
                } else {
                    val fileSuffix = objKey.substringAfter(schema.removePrefix("/"), "/")
                    if (objKey.startsWith(schema.removePrefix("/")) && !fileSuffix.isNullOrEmpty() && !fileSuffix.contains("/")) {
                        if (getFiletype(fileSuffix) != FileType.UNKNOWN) {
                            tables.add(fileSuffix)
                        }
                    }
                }
            }
            Ok(DbTabListResponse("Ok", "Bucket: ${dbInfo.database}", tables))
        }
    }

    override fun dbColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbColListResponse, DbAnalyzerError> {
        return handleRequest(dbInfo) {
            when (getFiletype(tabname)) {
                FileType.CSV -> getCSVColList(dbInfo, schema, tabname, sampleSize)
                FileType.JSON -> getJsonColList(dbInfo, schema, tabname, sampleSize)
                FileType.PARQUET -> getParquetColList(dbInfo, schema, tabname)
                FileType.UNKNOWN -> Err(DbAnalyzerError.BadRequest("Unsupported file type."))
            }
        }
    }

    /*
    Postgres Type	Parquet Type
    boolean	BooleanType
    char	Int8Type
    smallint	Int16Type
    real	Float32Type
    integer	Int32Type
    double precision	Float64Type
    bigint	Int64Type
    numeric	Float64Type
    text	ByteArrayType
    date	Date64Type
    timestamp	TimestampNanosecondType
     */

    private fun getParquetColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?
    ): Result<DbColListResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }

        val s3conf = Configuration().apply {
            set("fs.s3a.impl", S3AFileSystem::class.java.name)
            set("fs.s3a.access.key", dbInfo.user)
            set("fs.s3a.secret.key", dbInfo.password)
            set("fs.s3a.endpoint", "s3.amazonaws.com")
        }
        val fileName = if (schema.isNullOrEmpty() || schema.equals("/")) tabname else {
            if (schema.endsWith("/")) "$schema$tabname" else "$schema/$tabname"
        }
        val fileLocation = "s3a://${dbInfo.database}/$fileName"

        val inputFile = HadoopInputFile.fromPath(org.apache.hadoop.fs.Path(fileLocation), s3conf)
        var parquetFileReader: ParquetFileReader? = null
        val columns = mutableListOf<DbColumnInfo>()
        try {
            parquetFileReader = ParquetFileReader.open(inputFile)
            val metadata: ParquetMetadata = parquetFileReader.footer
            var pos = 0
            metadata.fileMetaData.schema.fields.forEach { field ->
                columns.add(
                    DbColumnInfo(
                        tableName = tabname!!,
                        tableSchema = schema,
                        columnName = field.name,
                        dataTypeStr = field.asPrimitiveType().primitiveTypeName.toString(),
                        typeName = if (field.logicalTypeAnnotation == null) "none" else field.logicalTypeAnnotation.toString(),
                        //typeName = field.asPrimitiveType().primitiveTypeName.toString(),
                        //dataTypeStr = if (field.logicalTypeAnnotation == null) "none" else field.logicalTypeAnnotation.toString(),
                        ordinalPosition = pos++,
                        //tableCat = null,
                        dataType = -1,
                        columnSize = -1,
                        decimalDigits = -1,
                        numPrecRadix = -1,
                        nullable = 1,
                        //remarks = null,
                        //columnDef = null,
                        charOctetLength = 0,
                        isNullable = "true", // We assume the fields are nullable for now
                        //isNullable = if (field.repetition.equals("OPTIONAL")) "true" else "false",
                        //scopeCatalog = null,
                        //scopeSchema = null,
                        //scopeTable = null,
                        //sourceData = null,
                        //sourceDataStr = null,
                        isAutoIncrement = "UNDEF",
                        isGeneratedColumn = "UNDEF",
                    )
                )
            }
        } catch (e: Exception) {
            return Err(DbAnalyzerError.BadRequest(e.message ?: ""))
        } finally {
            parquetFileReader?.close()
        }
        return Ok(DbColListResponse(SUCCESS_RESPONSE, "Columns", columns))
    }


    private fun getJsonColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbColListResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }
        if (sampleSize == null || sampleSize <= 0) {
            return Err(DbAnalyzerError.BadRequest("Sample size must be greater than zero."))
        }

        val fileName = if (schema.isNullOrEmpty() || schema.equals("/")) tabname else {
            if (schema.endsWith("/")) "$schema$tabname" else "$schema/$tabname"
        }
        val columns = mutableListOf<DbColumnInfo>()

        return try {
            val s3 = createS3Client(dbInfo)
            val getObjectRequest = GetObjectRequest.builder()
                .bucket(dbInfo.database)
                .key(fileName)
                .build()
            val obj = s3.getObject(getObjectRequest)

            obj.readAllBytes()
                .toString(StandardCharsets.UTF_8)
                .lines()
                .filter { it.isNotBlank() }
                .let { lines ->
                    val size = minOf(sampleSize, lines.size)
                    lines.take(size)
                }.let { lines ->
                    lines.map { mapper.readTree(it) }.let { inferrer.inferForSamples(it) }
                }.let { inferredSchema ->
                    createColumns(inferredSchema, schema, tabname).let { columns.addAll(it) }
                }

            Ok(DbColListResponse(SUCCESS_RESPONSE, "Sample data", columns))
        } catch (e: Exception) {
            Err(DbAnalyzerError.BadRequest(e.message ?: ""))
        }
    }

    private fun createS3Client(dbInfo: DbConnectDto): S3Client {
        val credentials = AwsBasicCredentials.create(dbInfo.user, dbInfo.password)
        val region = Region.of(dbInfo.properties)
        return S3Client.builder()
            .region(region)
            .credentialsProvider { credentials }
            .build()
    }

    private fun createColumns(jschema: JsonNode, schema: String?, tableName: String?): MutableList<DbColumnInfo> {
        val types = inferFieldType(jschema) ?: JsonNodeFactory.instance.objectNode()
        return types.fieldNames().asSequence().mapIndexed { pos, key ->
            val type: JsonNode? = types.get(key)?.get("type")
            val format: JsonNode? = types.get(key)?.get("format")
            DbColumnInfo(
                tableName = tableName!!,
                tableSchema = schema ?: "",
                columnName = key,
                typeName = format?.asText() ?: "",
                dataTypeStr = type?.asText() ?: "none",
                ordinalPosition = pos,
                dataType = -1,
                columnSize = -1,
                decimalDigits = -1,
                numPrecRadix = -1,
                nullable = 1,
                charOctetLength = 0,
                isNullable = "true",
                isAutoIncrement = "UNDEF",
                isGeneratedColumn = "UNDEF",
            )
        }.toMutableList()
    }


    private fun getCSVColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbColListResponse, DbAnalyzerError> {
        validator.validateRequestDbInfo(dbInfo).mapError { return Err(DbAnalyzerError.BadRequest(it.message)) }
        if (sampleSize == null || sampleSize <= 1) {
            return Err(DbAnalyzerError.BadRequest("Sample size must be greater than 1 (Line 0 is header)."))
        }

        val fileName = if (schema.isNullOrEmpty() || schema.equals("/")) tabname else {
            if (schema.endsWith("/")) "$schema$tabname" else "$schema/$tabname"
        }
        val columns = mutableListOf<DbColumnInfo>()

        return try {
            val s3 = createS3Client(dbInfo)
            val getObjectRequest = GetObjectRequest.builder()
                .bucket(dbInfo.database)
                .key(fileName)
                .build()
            val obj = s3.getObject(getObjectRequest)

            obj.readAllBytes()
                .toString(StandardCharsets.UTF_8)
                .lines()
                .filter { it.isNotBlank() }
                .let { lines ->
                    val size = minOf(sampleSize, lines.size)
                    lines.take(size)
                }.let { lines ->
                    val csvHeader = lines[0].split(",")
                    val csvData = lines.drop(1)
                    val jsonData : List<JsonNode> = csvData.map { dataRow ->
                        val data = dataRow.split(",")
                        if (data.size == csvHeader.size) {
                            val jsonObject = buildJsonObject {
                                csvHeader.zip(data).forEach { (key, value) ->
                                    put(key, value)
                                }
                            }
                            mapper.readTree(jsonObject.toString())
                        } else null
                    }.filterNotNull()
                    inferrer.inferForSamples(jsonData)
                }.let { inferredSchema ->
                    createColumns(inferredSchema, schema, tabname).let { columns.addAll(it) }
                }

            Ok(DbColListResponse(SUCCESS_RESPONSE, "Sample data", columns))
        } catch (e: Exception) {
            Err(DbAnalyzerError.BadRequest(e.message ?: ""))
        }
    }

    private val mapper = ObjectMapper()
    private val inferrer: JsonSchemaInferrer = JsonSchemaInferrer.newBuilder()
        .setSpecVersion(SpecVersion.DRAFT_07) // Requires commons-validator
        .addFormatInferrers(
            FormatInferrers.email(),
            FormatInferrers.ip(),
            FormatInferrers.dateTime(),
            AbsURIInferrer()
            //FormatInferrers.uri(),
            //FormatInferrers.hostname(),
            //FormatInferrers.ipv4(),
            //FormatInferrers.ipv6()
            //FormatInferrers.uriReference()
        )
        .setAdditionalPropertiesPolicy(AdditionalPropertiesPolicies.notAllowed())
        .setRequiredPolicy(RequiredPolicies.nonNullCommonFields())
        .addEnumExtractors(
            EnumExtractors.validEnum(Month::class.java),
            EnumExtractors.validEnum(DayOfWeek::class.java)
        )
        .build()

}
