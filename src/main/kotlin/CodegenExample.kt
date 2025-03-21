import java.util.*

data class Task(val id:Int,val description:String,var isDone:Boolean=false)

class TaskManager
{
	private val tasks=mutableListOf<Task>()
	private var nextId=1

	fun addTask(description:String)
	{
		val task=Task(nextId++,description)
		tasks.add(task)
		println("Задача добавлена: $task")
	}

	fun listTasks()
	{
		if(tasks.isEmpty())
		{
			println("Нет задач.")
			return
		}
		println("Список задач:")
		tasks.forEach {task->
			val status=if(task.isDone) "[✓]" else "[ ]"
			println("$status ${task.id}: ${task.description}")
		}
	}

	fun markTaskAsDone(id:Int)
	{
		val task=tasks.find {it.id==id}
		if(task!=null)
		{
			task.isDone=true
			println("Задача выполнена: ${task.description}")
		}
		else
		{
			println("Задача с ID $id не найдена.")
		}
	}

	fun removeTask(id:Int)
	{
		if(tasks.removeIf {it.id==id})
		{
			println("Задача $id удалена.")
		}
		else
		{
			println("Задача с ID $id не найдена.")
		}
	}
}

fun main()
{
	val scanner=Scanner(System.`in`)
	val manager=TaskManager()

	while(true)
	{
		println("\nВыберите действие: 1 - Добавить, 2 - Список, 3 - Завершить, 4 - Удалить, 0 - Выход")
		when(scanner.nextInt())
		{
			1->
			{
				print("Введите описание задачи: ")
				scanner.nextLine() // consume newline
				val desc=scanner.nextLine()
				manager.addTask(desc)
			}

			2->manager.listTasks()
			3->
			{
				print("Введите ID задачи для завершения: ")
				val id=scanner.nextInt()
				manager.markTaskAsDone(id)
			}

			4->
			{
				print("Введите ID задачи для удаления: ")
				val id=scanner.nextInt()
				manager.removeTask(id)
			}

			0->
			{
				println("Выход из программы.")
				return
			}

			else->println("Неверный ввод, попробуйте снова.")
		}
	}
}
