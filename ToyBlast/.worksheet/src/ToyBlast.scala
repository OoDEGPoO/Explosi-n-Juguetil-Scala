object ToyBlast {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
  println("Welcome to the Scala worksheet")
  
  import util.Random;$skip(61); 
  
  val (r1)= (1+Random.nextInt(4));System.out.println("""r1  : Int = """ + $show(r1 ));$skip(267); 
  
  def iniFichasNivel(n:Int, l:List[Int]): List[Int]={
  	var c:Int = (1+Random.nextInt(8)) //tomamos un color del 1 al 8
  	var laux:List[Int] = l:::c::Nil
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
  };System.out.println("""iniTablero: (x: Int, y: Int, n: Int, fichas: List[Int])List[List[Int]]""");$skip(66); 
  
  def iniFichasN1(): List[Int]={
  	iniFichasNivel(4, Nil)
  };System.out.println("""iniFichasN1: ()List[Int]""");$skip(66); 
  
  def iniFichasN2(): List[Int]={
  	iniFichasNivel(5, Nil)
  };System.out.println("""iniFichasN2: ()List[Int]""");$skip(66); 
  
  def iniFichasN3(): List[Int]={
  	iniFichasNivel(6, Nil)
  };System.out.println("""iniFichasN3: ()List[Int]""");$skip(94); 
  
  def iniTableroN1(fichas:List[Int]): List[List[Int]]={
  	iniTablero(7, 9, 4, fichas)
  };System.out.println("""iniTableroN1: (fichas: List[Int])List[List[Int]]""");$skip(96); 
  
  def iniTableroN2(fichas:List[Int]): List[List[Int]]={
  	iniTablero(11, 17, 5, fichas)
  };System.out.println("""iniTableroN2: (fichas: List[Int])List[List[Int]]""");$skip(96); 
  
  def iniTableroN3(fichas:List[Int]): List[List[Int]]={
  	iniTablero(15, 27, 6, fichas)
  };System.out.println("""iniTableroN3: (fichas: List[Int])List[List[Int]]""");$skip(87); 
  
  def iniActuarN1(): List[List[Boolean]]={
  	List.fill(7)(List.fill(9)(false))
  };System.out.println("""iniActuarN1: ()List[List[Boolean]]""");$skip(89); 
  
  def iniActuarN2(): List[List[Boolean]]={
  	List.fill(11)(List.fill(17)(false))
  };System.out.println("""iniActuarN2: ()List[List[Boolean]]""");$skip(89); 
  
  def iniActuarN3(): List[List[Boolean]]={
  	List.fill(15)(List.fill(27)(false))
  };System.out.println("""iniActuarN3: ()List[List[Boolean]]""");$skip(33); 
  
  val fichas1 = iniFichasN1();System.out.println("""fichas1  : List[Int] = """ + $show(fichas1 ));$skip(39); 
  val tablero1 = iniTableroN1(fichas1);System.out.println("""tablero1  : List[List[Int]] = """ + $show(tablero1 ));$skip(30); 
  val fichas2 = iniFichasN2();System.out.println("""fichas2  : List[Int] = """ + $show(fichas2 ));$skip(39); 
  val tablero2 = iniTableroN2(fichas2);System.out.println("""tablero2  : List[List[Int]] = """ + $show(tablero2 ));$skip(30); 
  val fichas3 = iniFichasN3();System.out.println("""fichas3  : List[Int] = """ + $show(fichas3 ));$skip(39); 
  val tablero3 = iniTableroN3(fichas3);System.out.println("""tablero3  : List[List[Int]] = """ + $show(tablero3 ));$skip(27); 
  val act1 = iniActuarN1();System.out.println("""act1  : List[List[Boolean]] = """ + $show(act1 ));$skip(27); 
  val act2 = iniActuarN2();System.out.println("""act2  : List[List[Boolean]] = """ + $show(act2 ));$skip(27); 
  val act3 = iniActuarN3();System.out.println("""act3  : List[List[Boolean]] = """ + $show(act3 ))}
}
