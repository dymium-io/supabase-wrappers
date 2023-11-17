package io.dymium.plugins

import io.dymium.routes.DbAnalyzerRoutes
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

fun Application.configureRouting() {
    routing {
        get("/") {
            call.respondText("Hello World!")
        }
    }
    DbAnalyzerRoutes()
}
