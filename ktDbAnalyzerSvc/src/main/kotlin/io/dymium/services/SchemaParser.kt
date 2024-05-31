package io.dymium.services

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import java.io.IOException

class Node(val name: String, var value: String? = null) {
    val children = mutableListOf<Node>()
    val arrayChildren = mutableListOf<Node>()
}


class SchemaParser {

    @Throws(IOException::class)
    fun parseJsonToTree(jsonString: String): Node {
        val objectMapper = ObjectMapper()
        val rootNode = objectMapper.readTree(jsonString)
        return parseNode(rootNode, "root")
    }

    fun parseNode(node: JsonNode, name: String): Node {
        var treeNode = Node(name)

        node.fields().forEach { field ->
            val fieldName = field.key
            val fieldValue = field.value
            if (fieldValue.isObject) {
                treeNode.children.add(parseNode(fieldValue, fieldName))
            } else if (fieldValue.isArray) {
                val arrayNode = Node(fieldName)
                fieldValue.forEach { arrayValue ->
                    if (arrayValue.isObject) {
                        arrayNode.arrayChildren.add(parseNode(arrayValue, "_element_"))
                    } else {
                        arrayNode.arrayChildren.add(Node(arrayValue.asText(), arrayValue.nodeType.toString()))
                    }
                }
                treeNode.children.add(arrayNode)
            } else if (fieldValue.isTextual) {
                treeNode.children.add(Node(fieldName, fieldValue.asText()))
            } else {
                treeNode.children.add(Node(fieldName, fieldValue.asText()))
            }
        }
        return treeNode
    }

}

@Throws(IOException::class)
fun findNode(root: JsonNode, name: String): JsonNode? {
    if (root.has(name)) {
        return root.get(name)
    }

    root.fields().forEach { field ->
        val foundNode = findNode(field.value, name)

        if (foundNode != null) {
            return foundNode
        }
    }

    return null
}


fun parseJsonToTree(jsonNode: JsonNode): Node {
    val parser = SchemaParser()
    return parser.parseNode(jsonNode, "root")
}

fun processSchema(node: Node, parent: String = ""): JsonNode {
    val objectMapper = ObjectMapper()
    val objectNode = objectMapper.createObjectNode()
    node.children.forEach { child ->
        if (child.children.isNotEmpty()) {
            objectNode.set(child.name, processSchema(child, child.name))
        } else if (child.arrayChildren.isNotEmpty()) {
            val arrayNode = objectMapper.createArrayNode()
            child.arrayChildren.forEach { arrayChild ->
                arrayNode.add(processSchema(arrayChild, arrayChild.name))
            }
            objectNode.set(child.name, arrayNode)
        } else {
            objectNode.put(child.name, child.value)
        }
    }
    return objectNode
}

fun processJsonSchema(jsonNode: JsonNode): JsonNode? {
    val objectMapper = ObjectMapper()
    val objectNode = objectMapper.createObjectNode()

    findNode(jsonNode, "properties").let {
        jsonNode.fields().forEach { field ->
            val fieldName = field.key
            val fieldValue = field.value
            inferFieldType(fieldValue).let {
                objectNode.set<JsonNode>(fieldName, it)
            }
        }
    }
    return objectNode
}

data class TypeFormat(val type: String, val format: String? = null)

fun getCommonBaseType(types: List<TypeFormat>): TypeFormat {
    val type = if (types.size == 1) {
        types[0].type
    } else {
        val baseType = "string"
        types.forEach { typeFormat ->
            // get the common base type for list of types

        }
        baseType
    }
    when (type) {
        "array" -> {
            return TypeFormat("array")
        }

        "string" -> {
            return TypeFormat("string")
        }

        "number" -> {
            return TypeFormat("number")
        }

        "integer" -> {
            return TypeFormat("integer")
        }

        "boolean" -> {
            return TypeFormat("boolean")
        }

        else -> {
            return TypeFormat("string")
        }
    }
}

fun getTypeForAnyOf(anyOf: JsonNode): TypeFormat {
    val types = mutableListOf<TypeFormat>()
    if (anyOf.isArray) {
        anyOf.forEach { anyOfNode ->
            anyOfNode.forEach { typeNode ->
                if (typeNode.has("enum")) {
                    val enum = typeNode.get("enum")
                    if (enum.isArray) {
                        enum.forEach { enumElement ->
                            types.add(TypeFormat(enumElement.nodeType.toString().lowercase()))
                        }
                    }
                } else if (typeNode.has("type")) {
                    val type = typeNode.get("type")
                    if (type.isTextual) {
                        val format = typeNode.get("format")?.asText()
                        types.add(TypeFormat(type.asText(), format))
                    } else if (type.isArray) {
                        type.forEach { typeElement ->
                            if (typeElement.isTextual) {
                                types.add(TypeFormat(typeElement.asText()))
                            }
                        }
                    }
                }
            }
        }
    } else {
        println("Error: anyOf node is not an array")
    }
    return getCommonBaseType(types)
}

fun inferFieldType(node: JsonNode): JsonNode? {
    val objectMapper = ObjectMapper()
    val objectNode = objectMapper.createObjectNode()
    findNode(node, "properties")?.let {
        it.fields().forEach { field ->
            val fieldName = field.key
            val fieldValue = field.value
            if (fieldValue.has("anyOf")) {
                val anyOf = fieldValue.get("anyOf")
                val typeObj = getTypeForAnyOf(anyOf)
                val typeNode = objectMapper.createObjectNode()
                objectNode.set<JsonNode>(fieldName, typeNode.put("type", typeObj.type))
                typeObj.format?.let {
                    typeNode.put("format", it)
                }

            } else {
                val typeField = fieldValue.get("type").asText()
                val typeNode = objectMapper.createObjectNode()
                when (typeField) {
                    null -> {
                        println("Error: No type field found for $node")
                    }

                    "string" -> {
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("string"))).type)
                        )
                    }

                    "object" -> {
                        // we don't parse nested json objects for now - return type as string with hint that it's a json object
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("string"))).type)
                        )
                        typeNode.put("format", "json")
                    }

                    "array" -> {
                        val arrayNode = objectMapper.createObjectNode()
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("array"))).type)
                        )

                        val items = fieldValue.get("items")
                        if (items.has("anyOf")) {
                            val anyOf = items.get("anyOf")
                            val typeObj = getTypeForAnyOf(anyOf)
                            arrayNode.put("type", typeObj.type)
                            typeObj.format?.let {
                                arrayNode.put("format", it)
                            }
                        } else if (items.has("enum")) {
                            val enumArray = items.get("enum")
                            // enum has to be an array of the same type
                            val type = enumArray[0].nodeType.toString().lowercase()
                            arrayNode.put("type", getCommonBaseType(listOf(TypeFormat(type))).type)
                            arrayNode.put("format", "enum")

                        } else {
                            if (items.has("type")) {
                                val type = items.get("type").asText()
                                arrayNode.put("type", type)
                            }
                            if (items.has("format")) {
                                val format = items.get("format").asText()
                                arrayNode.put("format", format)
                            }
                        }
                        typeNode.set("itemsType", arrayNode)
                    }

                    "number" -> {
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("number"))).type)
                        )
                    }

                    "integer" -> {
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("integer"))).type)
                        )
                    }

                    "boolean" -> {
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("boolean"))).type)
                        )
                    }

                    else -> {
                        objectNode.set<JsonNode>(
                            fieldName,
                            typeNode.put("type", getCommonBaseType(listOf(TypeFormat("any"))).type)
                        )
                    }
                }
            }
        }
    }
    return objectNode
}