package ast

import org.antlr.v4.runtime.Lexer
import org.antlr.v4.runtime.Parser
import org.antlr.v4.runtime.tree.ParseTree

class DefaultRule<L:Lexer,P:Parser>(val how:(ParseTree)->KotlinAst)

abstract class Character(val name: String, var health: Int) {
	abstract fun attack(target: Character)

	fun isAlive(): Boolean = health > 0

	open fun takeDamage(amount: Int) {
		health -= amount
		println("$name получил $amount урона. Осталось $health HP.")
		if (health <= 0) println("$name пал в бою...")
	}
}

// Класс Героя
class Hero(name: String, health: Int, val power: Int) : Character(name, health) {
	override fun attack(target: Character) {
		println("$name атакует ${target.name}!")
		target.takeDamage(power)
	}
}

// Класс Монстра
class Monster(name: String, health: Int, val damage: Int) : Character(name, health) {
	override fun attack(target: Character) {
		println("$name кусает ${target.name}!")
		target.takeDamage(damage)
	}
}

fun main() {
	val hero = Hero("Алиса", 100, 20);
	val goblin = Monster("Гоблин", 100, 20);
	val characters = listOf(hero, goblin);
	while(hero.isAlive()&&goblin.isAlive()){
		hero.attack(goblin);
		if (goblin.isAlive()) hero.attack(goblin);
		if (goblin.isAlive()) goblin.attack(hero);
		println();};}