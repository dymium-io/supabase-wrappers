package io.dymium.routes

import com.github.michaelbull.result.mapBoth
import dymium.io.config.appConfig
import dymium.io.dto.DbConnectDto
import io.dymium.errors.DbAnalyzerError
import io.dymium.services.DbService
import io.github.oshai.kotlinlogging.KotlinLogging
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.util.pipeline.*

private const val ENDPOINT = "api/dbanalyzer" // Endpoint

private val logger = KotlinLogging.logger {}

fun Application.DbAnalyzerRoutes() {
    val dbService = DbService(appConfig.applicationConfiguration)
    routing {
        route("/$ENDPOINT") {

            // Connect/Ping DB --> POST /api/dbanalyzer/dbping
            post("/dbping") {
                logger.debug { "POST dbPing /$ENDPOINT/dbping" }

                val dto = call.receive<DbConnectDto>()
                dbService.dbPing(dto)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get Schemas --> POST /api/dbanalyzer/dbschemas
            post("/dbschemas") {
                logger.debug { "POST dbSchemas /$ENDPOINT/dbschemas" }

                val dto = call.receive<DbConnectDto>()
                dbService.dbSchemas(dto)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get Tab Types --> POST /api/dbanalyzer/dbtabtypes
            post("/dbtabtypes") {
                logger.debug { "POST dbTabTypes /$ENDPOINT/dbtabtypes" }

                val dto = call.receive<DbConnectDto>()
                dbService.dbTabTypes(dto)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get List of Tables for data sources without schema (ex. Elasticsearch) --> POST /api/dbanalyzer/dbtables
            post("/dbtables") {
                logger.debug { "POST dbTables /$ENDPOINT/dbtables" }

                val dto = call.receive<DbConnectDto>()
                dbService.dbTabList(dto,null)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get List of Tables for given Schema --> POST /api/dbanalyzer/dbtables/{schema}
            post("/dbtables/{schema}") {
                logger.debug { "POST dbTables /$ENDPOINT/dbtables/{schema}" }

                val schema = call.parameters["schema"]
                val dto = call.receive<DbConnectDto>()
                dbService.dbTabList(dto,schema)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get List of Columns for given Table for data sources without schema (ex. Elasticsearch) --> POST /api/dbanalyzer/dbcolumns/{table}
            post("/dbcolumns/{table}") {
                logger.debug { "POST dbColumns /$ENDPOINT/dbcolumns/{schema}/{table}" }

                val table = call.parameters["table"]
                val dto = call.receive<DbConnectDto>()
                dbService.dbColList(dto,null,table)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get List of Columns for given Table --> POST /api/dbanalyzer/dbcolumns/{schema}/{table}
            post("/dbcolumns/{schema}/{table}") {
                logger.debug { "POST dbColumns /$ENDPOINT/dbcolumns/{schema}/{table}" }

                val schema = call.parameters["schema"]
                val table = call.parameters["table"]
                val dto = call.receive<DbConnectDto>()
                dbService.dbColList(dto,schema,table)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
            // Get sample data for given Schema (Optional), Table with sample size --> POST /api/dbanalyzer/dbsample?schema=ss&&table=t&&samplesize=sz
            post("/dbsample") {
                logger.debug { "POST dbSample /$ENDPOINT/dbsample" }

                val schema = call.request.queryParameters["schema"]
                val table = call.request.queryParameters["table"]
                val sampleSize = call.request.queryParameters["samplesize"]?.toInt()
                val dto = call.receive<DbConnectDto>()
                dbService.dbSample(dto,schema,table,sampleSize)
                    .mapBoth(
                        success = { call.respond(HttpStatusCode.OK, it) },
                        failure = { handleRequestError(it) }
                    )
            }
        }
    }
}


private suspend fun PipelineContext<Unit, ApplicationCall>.handleRequestError(
    error: Any
) {
    when (error) {
        // Users
        is DbAnalyzerError.BadRequest -> call.respond(HttpStatusCode.BadRequest, error.message)
        is DbAnalyzerError.NotFound -> call.respond(HttpStatusCode.NotFound, error.message)
        is DbAnalyzerError.Unauthorized -> call.respond(HttpStatusCode.Unauthorized, error.message)
        is DbAnalyzerError.Forbidden -> call.respond(HttpStatusCode.Forbidden, error.message)
    }
}