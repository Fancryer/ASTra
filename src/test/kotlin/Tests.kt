//import arrow.core.nonEmptyListOf
//import arrow.core.none
//import arrow.core.some
//import org.fancryer.bf.*
//import org.fancryer.bf.ast.*
//import kotlin.test.Test
//import kotlin.test.assertTrue
//
//class Tests
//{
//	@Test
//	fun test1()
//	{
//		val mainDecl1=KFunctionDeclaration(
//			identifier=KIdentifierInner("main"),
//			functionValueParameters=KFunctionValueParameters(none()),
//			functionBody=KBlock(
//				nonEmptyListOf(
//					KStatement(
//						emptyList(),
//						KPostfixUnaryExpression(
//							KIdentifierInner("println"),
//							listOf(
//								KCallSuffix(
//									none(),
//									KValueArguments(
//										nonEmptyListOf(
//											KValueArgument(
//												none(),
//												none(),
//												false,
//												KLineStringLiteral(
//													listOf(1.ast.add(1.ast).strExpr)
//												)
//											)
//										)
//									)
//								)
//							)
//						)
//					)
//				).some()
//			).some()
//		)
//
//		val mainDecl2=funct("main".id) {
//			body {
//				block {
//					+call(
//						"println".id,
//						KCallSuffix(
//							none(),
//							KValueArguments(
//								nonEmptyListOf(
//									KValueArgument(
//										none(),
//										none(),
//										false,
//										KLineStringLiteral(
//											1.ast.add(1.ast).strExpr.list()
//										)
//									)
//								)
//							)
//						)
//					)
//				}
//			}
//		}
//
//		val kotlinFile1=KKotlinFile(
//			none(),
//			listOf(),
//			identifier("org") {+"fancryer";+"main"}.packageHeader,
//			emptyList(),
//			mainDecl1.topLevel.list()
//		)
//
//		val kotlinFile2=KKotlinFile(
//			none(),
//			listOf(),
//			identifier("org") {+"fancryer";+"main"}.packageHeader,
//			emptyList(),
//			mainDecl2.topLevel.list()
//		)
//		println(kotlinFile1)
//		println(kotlinFile2)
//		println(kotlinFile1.code)
//		println(kotlinFile2.code)
//		assertTrue {kotlinFile1.code==kotlinFile2.code}
//	}
//}