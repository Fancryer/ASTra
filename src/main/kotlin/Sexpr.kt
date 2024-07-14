package org.fancryer.bf

import kotlin.reflect.*
import kotlin.reflect.full.declaredMembers
import kotlin.reflect.full.memberProperties
import kotlin.reflect.jvm.isAccessible

// Define a helper function that does the actual work.
fun <T : Any> T.sexprHelper(): String {
	val tClass = this::class
	val name = tClass.simpleName ?: return "'()'"

	val propexprs = tClass.memberProperties
		// Filter out properties that we don't want to include.
		.filterNot { it.isIgnoredForSexpr() }
		// Map each property to its s-expression string.
		.joinToString(" ") { prop ->
			val propName = prop.name
			val propValue = try {
				// Call the getter of the property if it's accessible.
				if (prop.visibility == KVisibility.PUBLIC) {
					prop.getter.call(this)?._sexprHelper() ?: "null"
				} else {
					"inaccessible"
				}
			} catch (e: Exception) {
				"error '${e.localizedMessage}'"
			}

			"($propName $propValue)"
		}

	return "($name [$propexprs])"
}

// Extension function to determine if a property should be included in the s-expression.
private fun KProperty<*>.isIgnoredForSexpr(): Boolean {
	return false
	//return name.startsWith("component") || name == "copy"// || isAccessible.not()
}

// Inline function that calls the helper.
inline fun <reified T : Any> T.sexpr(): String = sexprHelper()

// Extension function to convert the value to its s-expression representation.
// You may need to define how different types should be converted to their s-expression strings.
fun Any?._sexprHelper(): String {
	return when (this) {
		null -> "null"
		is Number, is Boolean, is String -> toString()
		else -> (this as? Any)?.sexprHelper() ?: toString()
	}
}