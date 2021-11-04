object Functions {

  // a)
  def area_circle(rad: Double): Double = rad * rad * Math.PI

  def testCircle(r: Double): Double = area_circle(r)

  // b)
  def area_1(a: Double)(b: Double): Double = a*b

  def testRectangleCurried(a: Double, b: Double): Double = area_1(a)(b)

  // c)
  def area_2(a: Double, b: Double): Double = a * b

  def testRectangleUc(a: Double, b: Double): Double = area_2(a,b)

}

