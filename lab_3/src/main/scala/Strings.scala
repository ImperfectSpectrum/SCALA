

/** Напишите ваши решения в тестовых функциях.
  * 
  * https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/StringOps.html
  */
object Strings {

  def Uppercase(str: String): String = str.toUpperCase

  def testUppercase(str: String): String = Uppercase(str)

  /* b) Вставьте следующие значения в строку:
   *       Hi my name is <name> and I am <age> years old.
   *    
   */
  def Interpolations(name: String, age: Int): String = s"Hi my name is $name and I am $age years old."

  def testInterpolations(name: String, age: Int): String = Interpolations(name,age)

  /* c) Добавьте два числа в следующую строку:
   *       Hi,
   *       now follows a quite hard calculation. We try to add:
   *         a := <value of a>
   *         b := <value of b>
   * 
   *         result is <a + b>
   * 
   *   
   */
  def testComputation(a: Int, b: Int): String = s"Hi,\n now follows a quite hard calculation. We try to add: \n a := $a \n b := $b \n\n result is ${a+b} "

  /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.
   */
  def TakeTwo(str: String): String = str match {
    case str: String  if (str.length == 2) => str
    case _ => str.take(2)
  }

  def testTakeTwo(str: String): String = TakeTwo(str)

  def main(args: Array[String]): Unit ={
    println("testUppercase=>" + testUppercase("asdzxc"))
    println("testInterpolations=>" + testInterpolations("Michael",27))
    println("testComputation=>" + testComputation(5,10))
    println("testTakeTwo=>" + testTakeTwo("asdzxc"))
  }

}
