package io.dymium.services

import com.saasquatch.jsonschemainferrer.*
import java.net.URI

class AbsURIInferrer : FormatInferrer {
    override fun inferFormat(input: FormatInferrerInput): String? {
        val textValue = input.sample.textValue() ?: return null
        try {
            val uri = URI(input.sample.textValue())
            if (uri.isAbsolute()) {
                return "uri"
            }
        } catch (e: java.lang.Exception) {
            // ignore
        }
        return null
    }
}