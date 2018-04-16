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
  };System.out.println("""iniTablero: (x: Int, y: Int, n: Int, fichas: List[Int])List[List[Int]]""");$skip(349); 
  
  //da igual como borremos, asi que borramos por filas
  def borradoTabx(lista:List[Int], actuarl:List[Boolean]): List[Int]={// se encarga de las filas borra los que estén a true en actuarl
  	if (lista.isEmpty) Nil
  	else if (actuarl.head) 0::borradoTabx(lista.tail, actuarl.tail)
  		else lista.head::borradoTabx(lista.tail, actuarl.tail)
  };System.out.println("""borradoTabx: (lista: List[Int], actuarl: List[Boolean])List[Int]""");$skip(210); 
  
  def borradoTab(tablero:List[List[Int]], actuar:List[List[Boolean]]): List[List[Int]]={
  	if (tablero.isEmpty) Nil
  	else borradoTabx(tablero.head, actuar.head)::borradoTab(tablero.tail, actuar.tail)
  };System.out.println("""borradoTab: (tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Int]]""");$skip(66); 
  
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
  };System.out.println("""iniActuarN3: ()List[List[Boolean]]""");$skip(186); 
  
  //Adaptación de setPosicionBool para enteros
  
  def setPosicionInt(v:Int, x: Int, y: Int, tablero: List[List[Int]]): List[List[Int]] = {
    setPosicionXInt(v, x, y, tablero)
  };System.out.println("""setPosicionInt: (v: Int, x: Int, y: Int, tablero: List[List[Int]])List[List[Int]]""");$skip(345); 
  
  def setPosicionXInt(v:Int, x: Int, y:Int, tablero: List[List[Int]]): List[List[Int]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (x == 0) {
        setPosicionYInt(v, y, tablero.head) :: tablero.tail
      } else {
        tablero.head :: setPosicionXInt(v, x - 1, y, tablero.tail)
      }
    }
  };System.out.println("""setPosicionXInt: (v: Int, x: Int, y: Int, tablero: List[List[Int]])List[List[Int]]""");$skip(202); 

  def setPosicionYInt(v:Int, y: Int, fila: List[Int]): List[Int] = {
      if (y == 0) {
        v :: fila.tail
      } else {
       fila.head :: setPosicionYInt(v, y - 1, fila.tail)
      }
    
  };System.out.println("""setPosicionYInt: (v: Int, y: Int, fila: List[Int])List[Int]""");$skip(181); 
  
  def tomaColumna(x:Int, y:Int, tablero:List[List[Int]]): List[Int]={//devuelve la columna X
  	if (y<0) Nil
  	else tablero.head.apply(x)::tomaColumna(x, y-1, tablero.tail)
  };System.out.println("""tomaColumna: (x: Int, y: Int, tablero: List[List[Int]])List[Int]""");$skip(264); 
   
  def tomaFicha(y:Int, columna:List[Int]): List[Int]={//toma la primera ficha mayor que 0 que encuentre antes del nivel 0
  	if (y<0) List(0, 0) // por seguridad
  	else if (columna.apply(y) == 0) tomaFicha(y-1, columna)
  		else List(columna.apply(y), y)
  };System.out.println("""tomaFicha: (y: Int, columna: List[Int])List[Int]""");$skip(562); 
  
  def rellenaColumnas(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	val c = tomaColumna(x, y, tablero)
  	val tomada = tomaFicha(y-1, c)
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0)
  			if (tomada.head == 0) rellenaColumnas(x, y-1, setPosicionInt(r,x,y,tablero), f, fichas)
  			else rellenaColumnas(x, y-1, setPosicionInt(tomada.head,x,y,setPosicionInt(0, x, tomada.last, tablero)), f, fichas)
  		else rellenaColumnas(x, y-1, tablero, f, fichas)
  };System.out.println("""rellenaColumnas: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(324); 
   
  def rellenaColumnasOptRand(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//rellena desde esa posición con enteros aleatorios posibles
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0) tablero
  	else rellenaColumnasOptRand(x, y-1, setPosicionInt(r,x,y,tablero), f, fichas)
  };System.out.println("""rellenaColumnasOptRand: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(596); 
  
  def rellenaColumnasOpt(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//Optimizado
  	val c = tomaColumna(x, y, tablero)
  	val tomada = tomaFicha(y-1, c)
  	if (y<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0)
  			if (tomada.head == 0) rellenaColumnasOptRand(x, y, tablero, f, fichas)//si no ha encontrado, los siguientes darán igual resultado, no busca más
  			else rellenaColumnasOpt(x, y-1, setPosicionInt(tomada.head,x,y,setPosicionInt(0, x, tomada.last, tablero)), f, fichas)
  		else rellenaColumnasOpt(x, y-1, tablero, f, fichas)
  };System.out.println("""rellenaColumnasOpt: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(210); 
  
  def rellenar(x:Int, ly:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	if (x<0) tablero
  	else rellenar(x-1, ly, rellenaColumnasOpt(x, ly, tablero, f, fichas), f, fichas)
  };System.out.println("""rellenar: (x: Int, ly: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(33); 
  
  val fichas1 = iniFichasN1();System.out.println("""fichas1  : List[Int] = """ + $show(fichas1 ));$skip(39); 
  val tablero1 = iniTableroN1(fichas1);System.out.println("""tablero1  : List[List[Int]] = """ + $show(tablero1 ));$skip(30); 
  val fichas2 = iniFichasN2();System.out.println("""fichas2  : List[Int] = """ + $show(fichas2 ));$skip(39); 
  val tablero2 = iniTableroN2(fichas2);System.out.println("""tablero2  : List[List[Int]] = """ + $show(tablero2 ));$skip(30); 
  val fichas3 = iniFichasN3();System.out.println("""fichas3  : List[Int] = """ + $show(fichas3 ));$skip(39); 
  val tablero3 = iniTableroN3(fichas3);System.out.println("""tablero3  : List[List[Int]] = """ + $show(tablero3 ));$skip(27); 
  val act1 = iniActuarN1();System.out.println("""act1  : List[List[Boolean]] = """ + $show(act1 ));$skip(27); 
  val act2 = iniActuarN2();System.out.println("""act2  : List[List[Boolean]] = """ + $show(act2 ));$skip(27); 
  val act3 = iniActuarN3();System.out.println("""act3  : List[List[Boolean]] = """ + $show(act3 ));$skip(48); 
  val act12 = setPosicionBool(true, 3, 4, act1);System.out.println("""act12  : List[List[Boolean]] = """ + $show(act12 ));$skip(46); 
  val tablero12 = borradoTab(tablero1, act12);System.out.println("""tablero12  : List[List[Int]] = """ + $show(tablero12 ));$skip(488); 
  
  
  /**
   * le pasamos la posicion, comprueba si se puede borrar
   * y llama a una funcion auxiliar para comprobar todos los que tendra que borrar, que es la lista que devuelve
   * Esta configurado para que empiece por 0, pero si cambiamos ==0 por ==1 serian desde 1 y habria que cambiar seleccionarFicha
   */
   // funciona bien
  def getPosicion(x: Int, y: Int, tablero: List[List[Int]]): Int = {
    val laux: List[Int] = getPosicionX(x, tablero)
    getPosicionY(y, laux)
  };System.out.println("""getPosicion: (x: Int, y: Int, tablero: List[List[Int]])Int""");$skip(256); 

  def getPosicionX(x: Int, tablero: List[List[Int]]): List[Int] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (x == 0) {
        tablero.head
      } else {
        getPosicionX(x - 1, tablero.tail)
      }
    }
  };System.out.println("""getPosicionX: (x: Int, tablero: List[List[Int]])List[Int]""");$skip(230); 

  def getPosicionY(y: Int, fila: List[Int]): Int = {
    if (fila.isEmpty) { //si no hay tablero
      -1
    } else {
      if (y == 0) {
        fila.head
      } else {
        getPosicionY(y - 1, fila.tail)
      }
    }
  };System.out.println("""getPosicionY: (y: Int, fila: List[Int])Int""");$skip(29); val res$0 = 

  getPosicionX(1, tablero3);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(30); val res$1 = 
  getPosicion(1, 7, tablero3);System.out.println("""res1: Int = """ + $show(res$1));$skip(214); 

  //igual pero de bool, funciona bien

  def getPosicionBool(x: Int, y: Int, tablero: List[List[Boolean]]): Boolean = {
    val laux: List[Boolean] = getPosicionXBool(x, tablero)
    getPosicionYBool(y, laux)
  };System.out.println("""getPosicionBool: (x: Int, y: Int, tablero: List[List[Boolean]])Boolean""");$skip(272); 

  def getPosicionXBool(x: Int, tablero: List[List[Boolean]]): List[Boolean] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (x == 0) {
        tablero.head
      } else {
        getPosicionXBool(x - 1, tablero.tail)
      }
    }
  };System.out.println("""getPosicionXBool: (x: Int, tablero: List[List[Boolean]])List[Boolean]""");$skip(249); 

  def getPosicionYBool(y: Int, fila: List[Boolean]): Boolean = {
    if (fila.isEmpty) { //si no hay tablero
      false
    } else {
      if (y == 0) {
        fila.head
      } else {
        getPosicionYBool(y - 1, fila.tail)
      }
    }
  };System.out.println("""getPosicionYBool: (y: Int, fila: List[Boolean])Boolean""");$skip(29); val res$2 = 

  getPosicionX(1, tablero3);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(30); val res$3 = 
  getPosicion(1, 7, tablero3);System.out.println("""res3: Int = """ + $show(res$3));$skip(204); 
  
  //PONE LA POSICON A LO QUE LE PASES, funciona guay

  def setPosicionBool(v:Boolean, x: Int, y: Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    setPosicionXBool(v, x, y, tablero)
  };System.out.println("""setPosicionBool: (v: Boolean, x: Int, y: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(360); 
  
  def setPosicionXBool(v:Boolean, x: Int, y:Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (x == 0) {
        setPosicionYBool(v, y, tablero.head) :: tablero.tail
      } else {
        tablero.head :: setPosicionXBool(v, x - 1, y, tablero.tail)
      }
    }
  };System.out.println("""setPosicionXBool: (v: Boolean, x: Int, y: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(216); 

  def setPosicionYBool(v:Boolean, y: Int, fila: List[Boolean]): List[Boolean] = {
      if (y == 0) {
        v :: fila.tail
      } else {
       fila.head :: setPosicionYBool(v, y - 1, fila.tail)
      }
    
  };System.out.println("""setPosicionYBool: (v: Boolean, y: Int, fila: List[Boolean])List[Boolean]""");$skip(1508); 
  

  //POSIBLE OPTIMIZACION PASAR AL AUX EL VALOR QUE TENEMSO QUE BUSCAR PARA NO MIRAR TODO EL RATO, IGUAL QUE AQUI EN LOS IF
  def seleccionarFicha(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Boolean]] = {

    val laux1 =
      if ((((x - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x - 1, y, tablero))) { //la x mayor o igual que 0
        //PONER A TRUEEE
        setPosicionBool(true, x, y, actuar)
        seleccionarFichaAux(x, y, tablero, actuar)
      } else {
        actuar
      }

    val laux2 =
      if ((((y - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x, y - 1, tablero))) { //la y mayor o igual que 0
        //PONER A TRUEEE
        setPosicionBool(true, x, y, laux1)
        seleccionarFichaAux(x, y, tablero, laux1)
      } else {
        laux1
      }

    val laux3 =
      if ((((x + 1) < tablero.length) && getPosicion(x, y, tablero) == getPosicion(x + 1, y, tablero))) { //la xmenor que la longitud del tablero (x)
        //PONER A TRUEEE
        setPosicionBool(true, x, y, laux2)
        seleccionarFichaAux(x, y, tablero, laux2)
      } else {
        laux2
      }
      

    val laux4 =
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero))) { //la y menor que la longitud de y
        //PONER A TRUEEE
        setPosicionBool(true, x, y, laux3)
        seleccionarFichaAux(x, y, tablero, laux3)
      } else {
        laux3
      }

    laux4
  };System.out.println("""seleccionarFicha: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(1346); 

  def seleccionarFichaAux(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Boolean]] = {
    //TRUEEEEEEEE
    print("Soy  " +x + y)
    val laux0 = setPosicionBool(true, x, y, actuar)
    val laux1 =
      if ((((x - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x - 1, y, tablero)) && !getPosicionBool(x-1, y, laux0)) { //la x mayor o igual que 0
        seleccionarFichaAux(x-1, y, tablero, laux0)
      } else {
        laux0
      }

    val laux2 =
      if ((((y - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x, y - 1, tablero)) && !getPosicionBool(x, y-1, laux1)) { //la y mayor o igual que 0
        seleccionarFichaAux(x, y-1, tablero, laux1)
      } else {
        laux1
      }

    val laux3 =
      if ((((x + 1) < tablero.length) && getPosicion(x, y, tablero) == getPosicion(x + 1, y, tablero)) && !getPosicionBool(x+1, y, laux2)) { //la xmenor que la longitud del tablero (x)
        seleccionarFichaAux(x+1, y, tablero, laux2)
      } else {
        laux2
      }

    val laux4 =
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero)) && !getPosicionBool(x, y+1, laux3)) { //la y menor que la longitud de y
        seleccionarFichaAux(x, y+1, tablero, laux3)
      } else {
        laux3
      }

    laux4
  };System.out.println("""seleccionarFichaAux: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(209); 
  
 def imprimirTablero(tablero: List[List[Int]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	imprimirTableroY(tablero.head)
    	imprimirTablero(tablero.tail)
    }
  };System.out.println("""imprimirTablero: (tablero: List[List[Int]])Int""");$skip(201); 

  def imprimirTableroY(fila: List[Int]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(fila.head + "  ")
    	imprimirTableroY(fila.tail)
    }
  };System.out.println("""imprimirTableroY: (fila: List[Int])Int""");$skip(227); 
  
   def imprimirTableroBool(tablero: List[List[Boolean]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	imprimirTableroYBool(tablero.head)
    	imprimirTableroBool(tablero.tail)
    }
  };System.out.println("""imprimirTableroBool: (tablero: List[List[Boolean]])Int""");$skip(213); 

  def imprimirTableroYBool(fila: List[Boolean]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(fila.head + "  ")
    	imprimirTableroYBool(fila.tail)
    }
  };System.out.println("""imprimirTableroYBool: (fila: List[Boolean])Int""");$skip(31); val res$4 = 
  
  imprimirTablero(tablero1);System.out.println("""res4: Int = """ + $show(res$4));$skip(28); val res$5 = 
  imprimirTableroBool(act3);System.out.println("""res5: Int = """ + $show(res$5));$skip(116); 
  
  // val salidaAux11 =setPosicionBool(true, 1, 1, act3)
   val salidaAux11 = seleccionarFicha(4,1,tablero1,act3);System.out.println("""salidaAux11  : List[List[Boolean]] = """ + $show(salidaAux11 ));$skip(34); val res$6 = 

imprimirTableroBool(salidaAux11);System.out.println("""res6: Int = """ + $show(res$6));$skip(32); 

  def guardarPartida() = {
  };System.out.println("""guardarPartida: ()Unit""");$skip(37); 

  def identificarPatrones() = {
  };System.out.println("""identificarPatrones: ()Unit""");$skip(30); 

  def estadisticas() = {
  };System.out.println("""estadisticas: ()Unit""")}



}
