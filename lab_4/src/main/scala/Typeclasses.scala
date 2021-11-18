object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.

  trait Reversable[T] {
    def reverse(x:T): T
  }

  implicit object ReverseStr extends Reversable[String] {
    def reverse(x: String): String = x.reverse
  }

  // b) Реализуйте функцию Reverse для String.

  def reverse[T:Reversable](x:T):T = implicitly[Reversable[T]].reverse(x)

  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(str: String): String = reverse(str)

  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.

  trait Smash[T] {
    def smash(x:T,b:T): T
  }

  implicit object SmashInt extends Smash[Int] {
    def smash(a: Int, b: Int): Int = a + b
  }

  implicit object SmashDouble extends Smash[Double] {
    def smash(a: Double, b: Double): Double = a * b
  }

  implicit object SmashStr extends Smash[String] {
    def smash(a: String, b: String): String = a + b
  }

  def smash[T:Smash](a:T,b:T):T = implicitly[Smash[T]].smash(a,b)

  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int у умножение для типа Double.


  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = smash(a,b)


  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра. 



  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = smash(a,b)





  def main(args: Array[String]): Unit = {
    println("testReversableString: " + testReversableString("abc"))
    println("testSmashInt: " + testSmashInt(5,6))
    println("testSmashDouble: " + testSmashDouble(5.5,2))
    println("testSmashString: "+ testSmashString("abc","qwe"))
  }
}

// Реализуйте тестовые функции с выводом на экран проверки разработанных функций.