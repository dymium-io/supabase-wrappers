package io.dymium.services

import com.github.michaelbull.result.Result
import dymium.io.dto.*
import io.dymium.errors.DbAnalyzerError

interface IDbService {
    fun dbPing(dbInfo: DbConnectDto): Result<DbConnectResponse, DbAnalyzerError>
    fun dbSchemas(dbInfo: DbConnectDto): Result<DbSchemasResponse, DbAnalyzerError>
    fun dbTabTypes(dbInfo: DbConnectDto): Result<DbTabTypesResponse, DbAnalyzerError>
    fun dbSample(
        dbInfo: DbConnectDto,
        dbschema: String?,
        table: String?,
        sampleSize: Int?,
    ): Result<DbSampleResponse, DbAnalyzerError>
    fun dbTabList(dbInfo: DbConnectDto, schema: String?): Result<DbTabListResponse, DbAnalyzerError>
    fun dbColList(
        dbInfo: DbConnectDto,
        schema: String?,
        tabname: String?,
        sampleSize: Int?
    ): Result<DbColListResponse, DbAnalyzerError>

}