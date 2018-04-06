object ToyBlast {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala workshit")
  
  import util.Random;$skip(61); 
  
  val (r1)= (1+Random.nextInt(4));System.out.println("""r1  : Int = """ + $show(r1 ));$skip(267); 
  
  def iniFichasNivel(n:Int, l:List[Int]): List[Int]={
  	val c:Int = (1+Random.nextInt(8)) //tomamos un color del 1 al 8
  	val laux:List[Int] = l:::c::Nil
  	if (n == 0) l
  	else
  		if (l.contains(c)) iniFichasNivel(n, l)
  		else iniFichasNivel(n-1, laux)
  };System.out.println("""iniFichasNivel: (n: Int, l: List[Int])List[Int]""");$skip(164); 
  
  def iniTablerox(y:Int, n:Int, fichas:List[Int]): List[Int]={
  	if (y == 0) Nil
  	else iniTablerox(y-1, n, fichas):::fichas.apply(Random.nextInt(n))::Nil
  };System.out.println("""iniTablerox: (y: Int, n: Int, fichas: List[Int])List[Int]""");$skip(172); 
  
  def iniTablero(x:Int, y:Int, n:Int, fichas:List[Int]): List[List[Int]]={
  	if (x == 0) Nil
  	else iniTablero(x-1, y, n, fichas):::iniTablerox(y, n, fichas)::Nil
  };System.out.println("""iniTablero: (x: Int, y: Int, n: Int, fichas: List[Int])List[List[Int]]""");$skip(1561); 
  /*
  def eliminarXYTab(x:Int, y:Int, t:List[List[Int]]): List[List[Int]]={
  	if (x == 0) if (y == 0) 0::t.head.tail
  		else eliminarXYTab(x, y-1, )
  }*/
  
  def borradoTab(x:Int, y:Int, ly:Int tablero:List[List[Int]], actuar:List[List[Boolean]]): List[List[Int]]={
  	if (x < 0) Nil
  	else if (y < 0) borradoTab(x-1, ly, ly, tablero, actuar)
  		else if (actuar.apply(x).apply(y)) if (y == 0) (0::Nil)::borradoTab(x-1, ly, ly, (tablero.tail))
  											else (0::tablero.head.tail)::borradoTab(x, y-1, ly, )
  }
  
  def iniFichasN1(): List[Int]={
  	iniFichasNivel(4, Nil)
  }
  
  def iniFichasN2(): List[Int]={
  	iniFichasNivel(5, Nil)
  }
  
  def iniFichasN3(): List[Int]={
  	iniFichasNivel(6, Nil)
  }
  
  def iniTableroN1(fichas:List[Int]): List[List[Int]]={
  	iniTablero(7, 9, 4, fichas)
  }
  
  def iniTableroN2(fichas:List[Int]): List[List[Int]]={
  	iniTablero(11, 17, 5, fichas)
  }
  
  def iniTableroN3(fichas:List[Int]): List[List[Int]]={
  	iniTablero(15, 27, 6, fichas)
  }
  
  def iniActuarN1(): List[List[Boolean]]={
  	List.fill(7)(List.fill(9)(false))
  }
  
  def iniActuarN2(): List[List[Boolean]]={
  	List.fill(11)(List.fill(17)(false))
  }
  
  def iniActuarN3(): List[List[Boolean]]={
  	List.fill(15)(List.fill(27)(false))
  }
  
  val fichas1 = iniFichasN1()
  val tablero1 = iniTableroN1(fichas1)
  val fichas2 = iniFichasN2()
  val tablero2 = iniTableroN2(fichas2)
  val fichas3 = iniFichasN3()
  val tablero3 = iniTableroN3(fichas3)
  val act1 = iniActuarN1()
  val act2 = iniActuarN2()
  val act3 = iniActuarN3();System.out.println("""borradoTab: (x: Int, y: Int, ly: <error>)Unit""")}
}
