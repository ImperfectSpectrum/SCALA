/** Напишите ваши решения в виде функций. */
object HigherOrder {

  val plus: (Int, Int) => Int = _ + _
  val multiply: (Int, Int) => Int =  _ * _

  // a)
  def nTimes(f: (Int,Int)=> Int, a: Int, b: Int, n: Int): Int = n*f(a,b)

  def testNTimes(f: (Int, Int) => Int, a: Int, b: Int, n: Int): Int = nTimes(f,a,b,n)

  // b)
  val maxmin: (Int,Int) =>Int = _ max _

  def testAnonymousNTimes(a: Int, b: Int, n: Int): Int = n * maxmin(a,b)

  def main(args: Array[String]): Unit = {
    println(testAnonymousNTimes(1,2,3))
  }
}

  /* a) Напишите функцию, которая принимает `f: (Int, Int) => Int`, параменты `a` и `b`
   *    и коэффициент умножения `n` и возвращает n * f(a, b). Назовите `nTimes`.
   */



  // примените вашу функцию (a) здесь, не изменяйте сигнатуру


  /* b) Напишите анонимную функцию, функцию без идентификатора ((a, b) => ???) для `nTimes` которая
   *    выполняет следующее:
   *          if (a > b) a else b
   */


