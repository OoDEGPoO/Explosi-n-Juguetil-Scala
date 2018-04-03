object ToyBlast {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import util.Random
  
  val (r1)= (1+Random.nextInt(4))                 //> r1  : Int = 2
  
  def iniFichasNivel(n:Int, l:List[Int]): List[Int]={
  	var c:Int = (1+Random.nextInt(8)) //tomamos un color del 1 al 8
  	var laux:List[Int] = l:::c::Nil
  	if (n == 0) l
  	else
  		if (l.contains(c)) iniFichasNivel(n, l)
  		else iniFichasNivel(n-1, laux)
  }                                               //> iniFichasNivel: (n: Int, l: List[Int])List[Int]
  
  def iniTablerox(y:Int, n:Int, fichas:List[Int]): List[Int]={
  	if (y == 0) Nil
  	else iniTablerox(y-1, n, fichas):::fichas.apply(Random.nextInt(n))::Nil
  }                                               //> iniTablerox: (y: Int, n: Int, fichas: List[Int])List[Int]
  
  def iniTablero(x:Int, y:Int, n:Int, fichas:List[Int]): List[List[Int]]={
  	if (x == 0) Nil
  	else iniTablero(x-1, y, n, fichas):::iniTablerox(y, n, fichas)::Nil
  }                                               //> iniTablero: (x: Int, y: Int, n: Int, fichas: List[Int])List[List[Int]]
  
  def iniFichasN1(): List[Int]={
  	iniFichasNivel(4, Nil)
  }                                               //> iniFichasN1: ()List[Int]
  
  def iniFichasN2(): List[Int]={
  	iniFichasNivel(5, Nil)
  }                                               //> iniFichasN2: ()List[Int]
  
  def iniFichasN3(): List[Int]={
  	iniFichasNivel(6, Nil)
  }                                               //> iniFichasN3: ()List[Int]
  
  def iniTableroN1(fichas:List[Int]): List[List[Int]]={
  	iniTablero(7, 9, 4, fichas)
  }                                               //> iniTableroN1: (fichas: List[Int])List[List[Int]]
  
  def iniTableroN2(fichas:List[Int]): List[List[Int]]={
  	iniTablero(11, 17, 5, fichas)
  }                                               //> iniTableroN2: (fichas: List[Int])List[List[Int]]
  
  def iniTableroN3(fichas:List[Int]): List[List[Int]]={
  	iniTablero(15, 27, 6, fichas)
  }                                               //> iniTableroN3: (fichas: List[Int])List[List[Int]]
  
  def iniActuarN1(): List[List[Boolean]]={
  	List.fill(7)(List.fill(9)(false))
  }                                               //> iniActuarN1: ()List[List[Boolean]]
  
  def iniActuarN2(): List[List[Boolean]]={
  	List.fill(11)(List.fill(17)(false))
  }                                               //> iniActuarN2: ()List[List[Boolean]]
  
  def iniActuarN3(): List[List[Boolean]]={
  	List.fill(15)(List.fill(27)(false))
  }                                               //> iniActuarN3: ()List[List[Boolean]]
  
  val fichas1 = iniFichasN1()                     //> fichas1  : List[Int] = List(3, 2, 8, 4)
  val tablero1 = iniTableroN1(fichas1)            //> tablero1  : List[List[Int]] = List(List(8, 2, 4, 8, 3, 2, 8, 2, 3), List(2,
                                                  //|  8, 2, 3, 3, 3, 8, 2, 3), List(2, 3, 8, 2, 3, 8, 8, 4, 8), List(8, 8, 8, 8,
                                                  //|  4, 3, 8, 4, 3), List(8, 2, 4, 2, 8, 4, 3, 8, 3), List(3, 3, 3, 8, 2, 3, 3,
                                                  //|  3, 8), List(2, 3, 4, 2, 4, 4, 3, 2, 2))
  val fichas2 = iniFichasN2()                     //> fichas2  : List[Int] = List(3, 4, 5, 8, 7)
  val tablero2 = iniTableroN2(fichas2)            //> tablero2  : List[List[Int]] = List(List(8, 7, 3, 7, 8, 3, 4, 5, 4, 4, 7, 4,
                                                  //|  3, 5, 4, 7, 3), List(8, 8, 3, 5, 5, 5, 5, 7, 8, 5, 5, 8, 4, 8, 7, 8, 7), L
                                                  //| ist(3, 3, 3, 4, 3, 3, 7, 8, 3, 3, 3, 7, 3, 5, 3, 5, 4), List(8, 4, 3, 5, 4,
                                                  //|  5, 7, 8, 3, 3, 7, 5, 7, 5, 3, 4, 7), List(5, 5, 8, 8, 4, 8, 5, 5, 8, 4, 3,
                                                  //|  7, 5, 7, 4, 7, 7), List(3, 8, 3, 8, 7, 4, 7, 7, 5, 8, 3, 5, 8, 7, 5, 5, 7)
                                                  //| , List(8, 4, 8, 3, 5, 5, 3, 3, 8, 3, 4, 8, 7, 8, 5, 8, 7), List(7, 4, 4, 4,
                                                  //|  3, 5, 8, 5, 4, 8, 7, 3, 5, 4, 8, 3, 5), List(4, 8, 3, 3, 3, 7, 4, 7, 4, 3,
                                                  //|  3, 3, 7, 7, 5, 8, 4), List(3, 3, 3, 5, 3, 7, 4, 3, 4, 4, 8, 4, 8, 8, 3, 5,
                                                  //|  4), List(3, 7, 8, 5, 7, 3, 5, 8, 7, 7, 3, 8, 7, 4, 3, 4, 8))
  val fichas3 = iniFichasN3()                     //> fichas3  : List[Int] = List(3, 2, 6, 8, 4, 5)
  val tablero3 = iniTableroN3(fichas3)            //> tablero3  : List[List[Int]] = List(List(8, 4, 2, 5, 8, 6, 5, 6, 2, 4, 5, 5,
                                                  //|  3, 4, 8, 8, 6, 6, 8, 6, 2, 2, 8, 5, 2, 6, 3), List(6, 8, 4, 8, 8, 2, 8, 6,
                                                  //|  5, 6, 5, 5, 5, 8, 2, 8, 2, 4, 3, 4, 3, 8, 2, 2, 5, 2, 6), List(5, 5, 3, 3,
                                                  //|  8, 8, 4, 3, 5, 5, 2, 3, 4, 8, 8, 8, 5, 3, 8, 5, 5, 4, 2, 8, 3, 4, 8), List
                                                  //| (6, 8, 3, 8, 5, 2, 6, 5, 2, 4, 3, 2, 6, 6, 3, 2, 3, 3, 2, 8, 6, 6, 5, 3, 8,
                                                  //|  5, 6), List(4, 2, 2, 8, 6, 5, 6, 8, 3, 2, 2, 6, 3, 6, 5, 5, 3, 3, 6, 8, 2,
                                                  //|  2, 6, 4, 8, 4, 8), List(3, 3, 8, 2, 3, 3, 2, 4, 3, 6, 3, 6, 5, 8, 5, 5, 4,
                                                  //|  8, 5, 8, 6, 3, 6, 8, 4, 2, 8), List(4, 2, 4, 2, 2, 4, 8, 8, 5, 4, 2, 5, 6,
                                                  //|  4, 5, 4, 5, 8, 6, 8, 8, 6, 3, 3, 6, 6, 6), List(6, 3, 2, 3, 6, 2, 3, 8, 5,
                                                  //|  3, 3, 3, 4, 6, 8, 6, 8, 6, 6, 4, 3, 3, 8, 3, 5, 8, 5), List(4, 8, 6, 8, 3,
                                                  //|  5, 3, 3, 4, 3, 8, 3, 5, 8, 6, 4, 8, 2, 3, 2, 3, 8, 4, 2, 5, 6, 5), List(3,
                                                  //|  4, 4, 2, 2, 8, 6, 8, 3, 3, 4, 6, 4, 2, 5, 6, 4, 5, 5, 6, 4, 5, 3, 4, 5, 2,
                                                  //|  2), List(4, 5, 3, 3, 4
                                                  //| Output exceeds cutoff limit.
  val act1 = iniActuarN1()                        //> act1  : List[List[Boolean]] = List(List(false, false, false, false, false, 
                                                  //| false, false, false, false), List(false, false, false, false, false, false,
                                                  //|  false, false, false), List(false, false, false, false, false, false, false
                                                  //| , false, false), List(false, false, false, false, false, false, false, fals
                                                  //| e, false), List(false, false, false, false, false, false, false, false, fal
                                                  //| se), List(false, false, false, false, false, false, false, false, false), L
                                                  //| ist(false, false, false, false, false, false, false, false, false))
  val act2 = iniActuarN2()                        //> act2  : List[List[Boolean]] = List(List(false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false), List(false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false, false, false, false, false), List(false, fal
                                                  //| se, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false), List(false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false), List(false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false, false, false, false, false), List(false, fal
                                                  //| se, false, false, false, false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false), List(false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false), List(false, f
                                                  //| Output exceeds cutoff limit.
  val act3 = iniActuarN3()                        //> act3  : List[List[Boolean]] = List(List(false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false, false, false, false, false
                                                  //| , false, false, false, false, false, false, false, false, false, false, fal
                                                  //| se), List(false, false, false, false, false, false, false, false, false, fa
                                                  //| lse, false, false, false, false, false, false, false, false, false, false, 
                                                  //| false, false, false, false, false, false, false), List(false, false, false,
                                                  //|  false, false, false, false, false, false, false, false, false, false, fals
                                                  //| e, false, false, false, false, false, false, false, false, false, false, fa
                                                  //| lse, false, false), List(false, false, false, false, false, false, false, f
                                                  //| alse, false, false, false, false, false, false, false, false, false, false,
                                                  //|  false, false, false, false, false, false, false, false, false), List(false
                                                  //| , false, false, false, false, false, false, false, false, false, false, fal
                                                  //| se, false, false, false
                                                  //| Output exceeds cutoff limit.
}