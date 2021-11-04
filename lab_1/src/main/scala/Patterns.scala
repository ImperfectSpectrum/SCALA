import javax.naming.spi.DirStateFactory.Result


object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal



  // a)

  def IntToString(value: Int): String = value match {
    case 1 => "it is one"
    case 2 => "it is two"
    case 3 => "it is three"
    case _ => "what's that"
  }

  def testIntToString(value: Int): String = IntToString(value)

  // b)

  def IsMaxAndMoritz(value: String): Boolean = value match {
    case "Max" => true
    case "max" => true
    case "Moritz" => true
    case "moritz" => true
    case _ => false
  }

  def testIsMaxAndMoritz(value: String): Boolean = IsMaxAndMoritz(value)

  // c)
  def IsEven(value: Int): Boolean = (value % 2) match {
    case 0 => true
    case _ => false
  }

  def testIsEven(value: Int): Boolean = IsEven(value)

  // d)

  def WinsA(a: Hand, b: Hand): Result = (a,b) match {
    case (Rock,Rock) | (Paper,Paper) | (Scissor,Scissor) => Draw
    case (Paper,Rock) | (Scissor,Paper) | (Rock,Scissor) => Win
    case (Rock, Paper) | (Paper, Scissor) | (Scissor, Rock) => Lose
  }

  def testWinsA(a: Hand, b: Hand): Result = WinsA(a,b)




  // e)

  def ExtractMammalWeight(animal: Animal): Int = animal match {
    case value: Mammal => value.weight
    case _ => -1
  }

  def testExtractMammalWeight(animal: Animal): Int = ExtractMammalWeight(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.

  def UpdateFood(animal: Animal): Animal = animal match {
    case Fish(_,_) => Fish(animal.name, Plants)
    case Bird(_,_) => Bird(animal.name, Plants)
    case _ => animal
  }

  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = UpdateFood(animal)

  def main(args: Array[String]): Unit = {
    val bear = Mammal("Bear", Meat, 300)
    val pigeon = Bird("pigeon", Vegetables)
    println("До " + pigeon.food)
    println("После " + testUpdateFood(pigeon).food)
    println(testExtractMammalWeight(bear))
    println(testExtractMammalWeight(pigeon))
  }



}

/* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */

// примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру

/* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
 *     "max" или "Max
 *     "moritz" или "Moritz"
 */



// примените функции из пункта (b) здесь, не изменяя сигнатуру

// c) Напишите функцию проверки является ли `value` четным



// примените функции из пункта (c) здесь, не изменяя сигнатуру



/* d) Напишите функцию, моделирующую игру в Камень ножницы бумага
 *     1. камень побеждает ножницы
 *     2. ножницы побеждают бумагу
 *     3. бумага побеждает камень
 *    Выиграет ли игрок `a`?
 */


// примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру



// Примечание: используйте определение Animals

// e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.



// примените функцию из пункта (e) здесь, не изменяйте сигнатуру


// f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.



// примените функцию из пункта (f) здесь, не изменяйте сигнатуру

