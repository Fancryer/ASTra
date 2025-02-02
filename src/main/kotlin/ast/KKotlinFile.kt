package org.fancryer.bf.ast

import arrow.core.Option
import arrow.core.none

data class KKotlinFile(
	val shebang:(Option<String>)=none(),
	val annotations:(List<KFileAnnotation>)=emptyList(),
	val packageHeader:KPackageHeader=KPackageHeader(),
	val importList:(List<KImportHeader>)=emptyList(),
	val topLevelObjectList:(List<KTopLevelObject>)=emptyList()
):KotlinAst
{
	override val code:String=buildString {
		shebang.onSome(::appendLine)
		annotations.map {it.code}.forEach(::appendLine)
		appendLine(packageHeader.code)
		importList.map {it.code}.forEach(::appendLine)
		topLevelObjectList.map {it.code}.forEach(::appendLine)
	}
}
/*
(fun fact n int (if (= n 0) 1 (* n (factorial (- n 1)))))

;*
let a = 5 - 3 in a * a
(5 - 3).let {a -> a * a}
*;


(let
  x
  10
  (let
    square
    (lambda (n) (* n n)))
    (square x))
*/