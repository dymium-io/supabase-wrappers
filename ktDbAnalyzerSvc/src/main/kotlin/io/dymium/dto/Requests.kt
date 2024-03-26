package dymium.io.dto

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject

@Serializable
data class DbConnectDto (
    val dbType: String? = null,
    val host: String? = null,
    val port: Int? = null,
    val database: String? = null,
    val properties: String? = null,
    val user: String? = null,
    val password: String? = null,
    val columns: String? = null,
    val tls: Boolean = false,
)


@Serializable
data class DbConnectResponse (
    val status: String,
    val message: String,
)

@Serializable
data class DbSchemasResponse (
    val status: String,
    val message: String,
    val schemas: List<String>,
)

@Serializable
data class DbTabTypesResponse (
    val status: String,
    val message: String,
    val tabtypes: List<String>,
)

@Serializable
data class DbTabListResponse (
    val status: String,
    val message: String,
    val tables: List<String>,
)

@Serializable
data class DbColListResponse (
    val status: String,
    val message: String,
    val columns: List<DbColumnInfo>,
)

@Serializable
data class DbColumnInfo (
    val tableCat: String? = null,   //TABLE_CAT String => table catalog (may be null)
    val tableSchema: String? = null,//TABLE_SCHEM String => table schema (may be null)
    val tableName: String,          //TABLE_NAME String => table name
    val columnName: String,         //COLUMN_NAME String => column name
    val dataType: Int,              //DATA_TYPE int => SQL type from java.sql.Types
    val dataTypeStr: String,        //Added conversion from java.sql.Types to String
    val typeName: String,           //TYPE_NAME String => Data source dependent type name,
                                    // for a UDT the type name is fully qualified
    val columnSize: Int,            //COLUMN_SIZE int => column size.
                                    //The COLUMN_SIZE column specifies the column size for the given column.
                                    //For numeric data, this is the maximum precision.
                                    //For character data, this is the length in characters.
                                    //For datetime datatypes, this is the length in characters of the String representation
                                    //(assuming the maximum allowed precision of the fractional seconds component).
                                    //For binary data, this is the length in bytes.
                                    //For the ROWID datatype, this is the length in bytes. Null is returned for data types where the column size is not applicable.
    val decimalDigits: Int,          //DECIMAL_DIGITS int => the number of fractional digits. Null is returned for data types where DECIMAL_DIGITS is not applicable.
    val numPrecRadix: Int,          //NUM_PREC_RADIX int => Radix (typically either 10 or 2)
    val nullable: Int,              //NULLABLE int => is NULL allowed.
                                        //columnNoNulls - might not allow NULL values
                                        //columnNullable - definitely allows NULL values
                                        //columnNullableUnknown - nullability unknown
    val remarks: String? = null,    //REMARKS String => comment describing column (may be null)
    val columnDef: String? = null,  //COLUMN_DEF String => default value for the column, which should be interpreted
                                    // as a string when the value is enclosed in single quotes (may be null)
    val charOctetLength: Int,       //CHAR_OCTET_LENGTH int => for char types the maximum number of bytes in the column
    val ordinalPosition: Int,       //ORDINAL_POSITION int => index of column in table (starting at 1)
    val isNullable: String,         //IS_NULLABLE String => ISO rules are used to determine the nullability for a column.
                                        //YES --- if the column can include NULLs
                                        //NO --- if the column cannot include NULLs
                                        //empty string --- if the nullability for the column is unknown
    val scopeCatalog: String? = null,//SCOPE_CATALOG String => catalog of table that is the scope of a reference attribute (null if DATA_TYPE isn't REF)
    val scopeSchema: String? = null,//SCOPE_SCHEMA String => schema of table that is the scope of a reference attribute (null if the DATA_TYPE isn't REF)
    val scopeTable: String? = null, //SCOPE_TABLE String => table name that this the scope of a reference attribute (null if the DATA_TYPE isn't REF)
    val sourceData: Int? = null,    //SOURCE_DATA_TYPE short => source type of a distinct type or user-generated Ref type, SQL type from java.sql.Types
                                    // (null if DATA_TYPE isn't DISTINCT or user-generated REF)
    val sourceDataStr: String? = null, // Convert sourceData to name
    val isAutoIncrement: String,    //IS_AUTOINCREMENT String => Indicates whether this column is auto incremented
                                        //YES --- if the column is auto incremented
                                        //NO --- if the column is not auto incremented
                                        //empty string --- if it cannot be determined whether the column is auto incremented
    val isGeneratedColumn: String,  //IS_GENERATEDCOLUMN String => Indicates whether this is a generated column
                                        //YES --- if this a generated column
                                        //NO --- if this not a generated column
                                        //empty string --- if it cannot be determined whether this is a generated column
)

@Serializable
data class DbSampleResponse (
    val status: String,
    val message: String,
    val data: List<JsonElement>,
)
