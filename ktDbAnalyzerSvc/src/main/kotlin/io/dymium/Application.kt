package io.dymium

import dymium.io.config.appConfig
import io.dymium.plugins.*
import io.ktor.server.application.*

fun main(args: Array<String>) {
    io.ktor.server.netty.EngineMain.main(args)
}

@Suppress("unused") // Referenced in application.conf
fun Application.module() {
    configureSerialization()
    configureRouting()
}
