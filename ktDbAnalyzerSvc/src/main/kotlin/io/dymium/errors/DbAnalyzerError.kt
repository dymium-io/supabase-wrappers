package io.dymium.errors

sealed class DbAnalyzerError(val message: String) {
    class NotFound(message: String) : DbAnalyzerError(message)
    class BadRequest(message: String) : DbAnalyzerError(message)
    class Unauthorized(message: String) : DbAnalyzerError(message)
    class Forbidden(message: String) : DbAnalyzerError(message)
}
