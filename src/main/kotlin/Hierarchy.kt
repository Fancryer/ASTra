package org.fancryer.bf

import arrow.core.const
import java.lang.reflect.ParameterizedType
import kotlin.reflect.KClass
import kotlin.reflect.full.allSuperclasses
import kotlin.reflect.jvm.javaType
import kotlin.reflect.jvm.jvmErasure

sealed interface Distance:Comparable<Distance>
{
	data class RealDistance(val distance:Int):Distance
	{
		override fun compareTo(other:Distance)=
			when(other)
			{
				is RealDistance->distance.compareTo(other.distance)
				is UnrealDistance->-1
			}

		override fun toString()="$distance.rd"
	}

	data object UnrealDistance:Distance
	{
		override fun compareTo(other:Distance)=
			when(other)
			{
				is RealDistance->1
				is UnrealDistance->0
			}

		override fun toString()="urd"
	}

	val Int.rd get()=RealDistance(this)
}

fun Distance.isZero()=
	this is Distance.RealDistance&&distance==0

// Возвращает "расстояние" от предка до потомка.
fun distanceFromAncestor(descendant:KClass<*>,ancestor:KClass<*>):Distance
{
	var distance=0
	var currentClass:KClass<*>?=descendant

	while(currentClass!=null&&currentClass!=ancestor)
	{
		distance++
		currentClass=currentClass.allSuperclasses.firstOrNull {it==ancestor||ancestor in it.allSuperclasses}
	}

	return if(currentClass==ancestor) Distance.RealDistance(distance) else Distance.UnrealDistance
}

fun distanceFromAncestorWithPath(descendant:KClass<*>,ancestor:KClass<*>):Pair<Distance,List<KClass<*>>>
{
	var distance=0
	var currentClass:KClass<*>?=descendant
	val path=mutableListOf<KClass<*>>()

	while(currentClass!=null&&currentClass!=ancestor)
	{
		distance++
		path.add(currentClass)
		currentClass=currentClass.allSuperclasses.firstOrNull {it==ancestor||ancestor in it.allSuperclasses}
	}

	path.add(currentClass ?: return Pair(Distance.UnrealDistance,path))

	return Pair(Distance.RealDistance(distance),path)
}

// Сортирует список классов по расстоянию от указанного класса.
fun <T:Any> sortClassesByDistanceTo(descendant:KClass<out T>,classes:List<KClass<out T>>):List<KClass<out T>>
{
	return classes.sortedBy {distanceFromAncestor(descendant,it)}
}

inline fun <reified T:Any> groupClasses(classes:List<KClass<out T>>)=
	classes.groupBy {distanceFromAncestor(it,T::class)}

sealed class A
sealed class B:A()
sealed class C:A()
sealed class D:B()
sealed class E:C()


fun <T:Any,R:Any> compareClasses(t:KClass<T>,r:KClass<R>)=println("$t ${if(t==r) '=' else '!'}= $r")

fun <T:Any,R:Any> compareClassesStrict(t:KClass<T>,r:KClass<R>)=println("$t ${if(t===r) '=' else '!'}== $r")

fun <T:Any,R:Any> compareClasses(t:Class<T>,r:Class<R>)=println("$t ${if(t==r) '=' else '!'}= $r")

fun <T:Any,R:Any> compareClassesStrict(t:Class<T>,r:Class<R>)=println("$t ${if(t===r) '=' else '!'}== $r")

fun <T:Any,R:Any> foo(t:KClass<T>,r:KClass<R>)
{
	buildString {
		val tt=t.typeParameters.flatMap {
			it.upperBounds
		}.map {
			"{${it.classifier} ${it.arguments} ${it.jvmErasure}}"
		}

		val rt=r.typeParameters.flatMap {
			it.upperBounds
		}.map {
			it
		}

		when(val tj=t)
		{
			is ParameterizedType->
			{
				append(tj.actualTypeArguments)
			}
		}
		append(tt)
		append(" ${if(tt==rt) '=' else '!'}= ")
		append(r.simpleName)
		append(rt)
	}.also(::println)
	//println("${t} ${if(t==r) '=' else '!'}= $r")
}

fun main()
{
	val i:Any=listOf(1,2)
	val j:Any=listOf("","")
	compareClasses(i::class,j::class)
	compareClasses(i::class.java,j::class.java)
	compareClassesStrict(i::class,j::class)
	compareClassesStrict(i::class.java,j::class.java)
	foo(i::class,j::class)
	//	val classes:(List<KClass<out A>>)=listOf(D::class,B::class,A::class,C::class,E::class)
	//	val sortedClasses=sortClassesByDistanceTo(E::class,classes)
	//
	//	println(sortedClasses) // Выведет список отсортированный по "расстоянию" до E
	//
	//	classes.groupBy {distanceFromAncestor(E::class,it)}.toList().sortedByDescending {
	//		it.first
	//	}.also {println(it)}
}

/*
[
	(2147483647,
		[class org.fancryer.bf.D,
			class org.fancryer.bf.B]),
	(2, [class org.fancryer.bf.A]),
	(1, [class org.fancryer.bf.C]),
	(0, [class org.fancryer.bf.E])]
*/