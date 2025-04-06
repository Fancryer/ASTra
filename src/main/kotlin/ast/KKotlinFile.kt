package ast

import arrow.core.Option
import arrow.core.none
import arrow.core.some

class KotlinFileBuilder
{
	private var shebang:(Option<String>)=none()
	private var annotations:(List<KFileAnnotation>)=emptyList()
	private var packageHeader:KPackageHeader=KPackageHeader()
	private var imports:(List<KImportHeader>)=emptyList()
	private var topLevelObjects:(List<KDeclaration>)=emptyList()

	fun shebang(shebang:String)=also {this.shebang=shebang.some()}
	operator fun String.unaryPlus()=shebang(this)

	fun anno(annotation:KFileAnnotation)=also {this.annotations+=annotation}
	fun annos(annotations:List<KFileAnnotation>)=also {this.annotations+=annotations}

	operator fun KFileAnnotation.unaryPlus()=anno(this)

	fun header(header:KPackageHeader)=also {this.packageHeader=header}
	operator fun KPackageHeader.unaryPlus()=header(this)

	fun import(import:KImportHeader)=also {this.imports+=import}
	fun imports(importList:List<KImportHeader>)=also {this.imports+=importList}

	operator fun KImportHeader.unaryPlus()=import(this)

	fun topLevelObject(topLevelObject:KDeclaration)=also {this.topLevelObjects+=topLevelObject}
	fun topLevelObjects(topLevelObjectList:List<KDeclaration>)=also {this.topLevelObjects+=topLevelObjectList}

	operator fun KDeclaration.unaryPlus()=topLevelObject(this)

	private fun build():KKotlinFile=KKotlinFile(
		shebang,
		annotations,
		packageHeader,
		imports,
		topLevelObjects
	)

	companion object
	{
		fun kotlinFile(init:KotlinFileBuilder.()->Unit)=
			KotlinFileBuilder().also(init).build()
	}
}

data class KKotlinFile(
	val shebang:(Option<String>)=none(),
	val annotations:(List<KFileAnnotation>)=emptyList(),
	val packageHeader:KPackageHeader=KPackageHeader(),
	val imports:(List<KImportHeader>)=emptyList(),
	val topLevelObjects:(List<KDeclaration>)=emptyList()
):KotlinAst