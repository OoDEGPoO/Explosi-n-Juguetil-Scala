import util.Random
import java.io._

object ToyBlast {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(97); 
  println("Welcome to the Scala workshit");$skip(268); 
  
  def iniFichasNivel(n:Int, l:List[Int]): List[Int]={
  	val c:Int = (1+Random.nextInt(8)) //tomamos un color del 1 al 8
  	val laux:List[Int] = l:::c::Nil
  	if (n == 0) l
  	else
  		if (l.contains(c)) iniFichasNivel(n, l)
  		else iniFichasNivel(n-1, laux)
  };System.out.println("""iniFichasNivel: (n: Int, l: List[Int])List[Int]""");$skip(165); 
  
  def iniTablerox(x:Int, n:Int, fichas:List[Int]): List[Int]={
  	if (x == 0) Nil
  	else iniTablerox(x-1, n, fichas):::fichas.apply(Random.nextInt(n))::Nil
  };System.out.println("""iniTablerox: (x: Int, n: Int, fichas: List[Int])List[Int]""");$skip(173); 
  
  def iniTablero(x:Int, y:Int, n:Int, fichas:List[Int]): List[List[Int]]={
  	if (y == 0) Nil
  	else iniTablero(x, y-1, n, fichas):::iniTablerox(x, n, fichas)::Nil
  };System.out.println("""iniTablero: (x: Int, y: Int, n: Int, fichas: List[Int])List[List[Int]]""");$skip(166); 
  
  def sumaStats(v:Int, stats:List[Int]): List[Int]={
  	if (v==stats.size) stats.init:::List(stats.last+1)
  	else sumaStats(v, stats.init):::List(stats.last)
  };System.out.println("""sumaStats: (v: Int, stats: List[Int])List[Int]""");$skip(500); 
  
  //da igual como borremos, asi que borramos por filas
  def borradoTabx(lista:List[Int], actuarl:List[Boolean], stats:List[Int]): (List[Int], List[Int])={// se encarga de las filas borra los que estén a true en actuarl
  	if (lista.isEmpty) (Nil, stats)
  	else if (actuarl.head) {
  		val a1 = borradoTabx(lista.tail, actuarl.tail, sumaStats(lista.head, stats))
  		(0::a1._1, a1._2)
  	} else {
  		val a2 = borradoTabx(lista.tail, actuarl.tail, stats)
  		(lista.head::a2._1, a2._2)
  	}
  };System.out.println("""borradoTabx: (lista: List[Int], actuarl: List[Boolean], stats: List[Int])(List[Int], List[Int])""");$skip(336); 
  
  def borradoTab(tablero:List[List[Int]], actuar:List[List[Boolean]], stats:List[Int]): (List[List[Int]], List[Int])={
  	if (tablero.isEmpty) (Nil, stats)
  	else{
  		val auxBx = borradoTabx(tablero.head, actuar.head, stats)
  		val auxB = borradoTab(tablero.tail, actuar.tail, auxBx._2)
  		(auxBx._1::auxB._1, auxB._2)
  	}
  };System.out.println("""borradoTab: (tablero: List[List[Int]], actuar: List[List[Boolean]], stats: List[Int])(List[List[Int]], List[Int])""");$skip(67); 
  
  def iniFichasN1(): List[Int]={
  	iniFichasNivel(4, Nil)
  };System.out.println("""iniFichasN1: ()List[Int]""");$skip(67); 
  
  def iniFichasN2(): List[Int]={
  	iniFichasNivel(5, Nil)
  };System.out.println("""iniFichasN2: ()List[Int]""");$skip(67); 
  
  def iniFichasN3(): List[Int]={
  	iniFichasNivel(6, Nil)
  };System.out.println("""iniFichasN3: ()List[Int]""");$skip(95); 
  
  def iniTableroN1(fichas:List[Int]): List[List[Int]]={
  	iniTablero(7, 9, 4, fichas)
  };System.out.println("""iniTableroN1: (fichas: List[Int])List[List[Int]]""");$skip(97); 
  
  def iniTableroN2(fichas:List[Int]): List[List[Int]]={
  	iniTablero(11, 17, 5, fichas)
  };System.out.println("""iniTableroN2: (fichas: List[Int])List[List[Int]]""");$skip(97); 
  
  def iniTableroN3(fichas:List[Int]): List[List[Int]]={
  	iniTablero(15, 27, 6, fichas)
  };System.out.println("""iniTableroN3: (fichas: List[Int])List[List[Int]]""");$skip(88); 
  
  def iniActuarN1(): List[List[Boolean]]={
  	List.fill(9)(List.fill(7)(false))
  };System.out.println("""iniActuarN1: ()List[List[Boolean]]""");$skip(90); 
  
  def iniActuarN2(): List[List[Boolean]]={
  	List.fill(17)(List.fill(11)(false))
  };System.out.println("""iniActuarN2: ()List[List[Boolean]]""");$skip(90); 
  
  def iniActuarN3(): List[List[Boolean]]={
  	List.fill(27)(List.fill(15)(false))
  };System.out.println("""iniActuarN3: ()List[List[Boolean]]""");$skip(292); 
  
  //Adaptación de setPosicionBool para enteros
  
  def setPosicionInt(v:Int, x: Int, y: Int, tablero: List[List[Int]]): List[List[Int]] = {
    if (tablero.isEmpty || x<0 || y<0 || y>=tablero.size || x>=tablero.head.size) {
    	tablero
    } else setPosicionYInt(v, x, y, tablero)
  };System.out.println("""setPosicionInt: (v: Int, x: Int, y: Int, tablero: List[List[Int]])List[List[Int]]""");$skip(346); 
  
  def setPosicionYInt(v:Int, x: Int, y:Int, tablero: List[List[Int]]): List[List[Int]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (y == 0) {
        setPosicionXInt(v, x, tablero.head) :: tablero.tail
      } else {
        tablero.head :: setPosicionYInt(v, x, y - 1, tablero.tail)
      }
    }
  };System.out.println("""setPosicionYInt: (v: Int, x: Int, y: Int, tablero: List[List[Int]])List[List[Int]]""");$skip(204); 

  def setPosicionXInt(v:Int, x: Int, fila: List[Int]): List[Int] = {
      if (x == 0) {
        v :: fila.tail
      } else {
       fila.head :: setPosicionXInt(v, x - 1, fila.tail)
      }
    
  };System.out.println("""setPosicionXInt: (v: Int, x: Int, fila: List[Int])List[Int]""");$skip(562); 
  
  /*def tomaColumna(x:Int, y:Int, tablero:List[List[Int]]): List[Int]={
  	if (x<0 || y<0 || tablero.isEmpty) Nil
  	else tablero.apply(y).apply(x)::tomaColumna(x, y-1, tablero)
  }*/
  
  def tomaColumna(x:Int, y:Int, tablero:List[List[Int]]): List[Int]={
  	if (x<0 || tablero.isEmpty) Nil
  	else {
  		if (y<(tablero.size-1)) tomaColumna(x, y, tablero.init)//quitamos filas hasta llegar a y
  		else {
  			//println(tablero.last.apply(x))																					//Borrar
  			tomaColumna(x, y-1, tablero.init):::List(tablero.last.apply(x))
  		}
  	}
  };System.out.println("""tomaColumna: (x: Int, y: Int, tablero: List[List[Int]])List[Int]""");$skip(781); 
   
  /*def tomaFicha(y:Int, columna:List[Int]): List[Int]={//toma la primera ficha mayor que 0 que encuentre antes del nivel 0
  	if (y<0 || columna.isEmpty) {print("\nfin columna")
  		List(0, 0)} // por seguridad
  	else if (columna.apply(y) == 0){ tomaFicha(y-1, columna)
  		}else {print("\nencontré una! - " + y)
  			List(columna.apply(y), y)}
  }*/
  
  def tomaFicha(y:Int, columna:List[Int]): List[Int]={
  	if (columna.isEmpty) {
  		//println("Fin columna")																					//Borrar
  		List(0, 0)
  	} else if (columna.last == 0) {
  		//println("y" + y + "= " + columna.last)																					//Borrar
  		tomaFicha(y-1, columna.init)
  	}	else {
  		//println("--y" + y + "= " + columna.last)																					//Borrar
  		List(columna.last, y)
  	}
  };System.out.println("""tomaFicha: (y: Int, columna: List[Int])List[Int]""");$skip(911); 
  
  def rellenaColumnas(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	//print("-x" + x + "-\n")																					//Borrar
  	val tomada = tomaFicha(y, tomaColumna(x, y, tablero))
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0 || x<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0) {
  			//print("\n\nHay que sustituir\n")																					//Borrar
  			if (tomada.head == 0) {//print("por Random\n")																					//Borrar
  			rellenaColumnas(x, y-1, setPosicionInt(r, x, y, tablero), f, fichas)
  			} else {//print("\n Cambio " + y + " con " + tablero.apply(y).apply(x) + " por " + tomada.last + " con " +  + tomada.head + "\n")																					//Borrar
  			rellenaColumnas(x, y-1, setPosicionInt(tomada.head, x, y, setPosicionInt(0, x, tomada.last, tablero)), f, fichas)}
  		} else rellenaColumnas(x, y-1, tablero, f, fichas)
  };System.out.println("""rellenaColumnas: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(535); 
  
  /*def rellenaColumnas2(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	val c = tomaColumna2(x)
  }*/
   
  def rellenaColumnasOptRand(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//rellena desde esa posición con enteros aleatorios posibles
  	//print("-x" + x + "-\n")																					//Borrar
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0 || x<0) tablero
  	else rellenaColumnasOptRand(x, y-1, setPosicionInt(r,x,y,tablero), f, fichas)
  };System.out.println("""rellenaColumnasOptRand: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(948); 
  
  def rellenaColumnasOpt(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//Optimizado
  	//print("-x" + x + "-\n")																					//Borrar
  	val tomada = tomaFicha(y-1, tomaColumna(x, y, tablero))
  	if (y<0 || x<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0) {
  			//print("\n\nHay que sustituir\n")																					//Borrar
  			if (tomada.head == 0) { //print("por Random\n")																					//Borrar
  			rellenaColumnasOptRand(x, y, tablero, f, fichas)//si no ha encontrado, los siguientes darán igual resultado, no busca más
  			} else {//print("\n Cambio " + y + " con " + tablero.apply(y).apply(x) + " por " + tomada.last + " con " +  + tomada.head + "\n")																					//Borrar
  			rellenaColumnasOpt(x, y-1, setPosicionInt(tomada.head,x,y,setPosicionInt(0, x, tomada.last, tablero)), f, fichas)
  			}
  		} else rellenaColumnasOpt(x, y-1, tablero, f, fichas)
  };System.out.println("""rellenaColumnasOpt: (x: Int, y: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(208); 
  
  def rellenar(x:Int, ly:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	if (x<0) tablero
  	else rellenar(x-1, ly, rellenaColumnas(x, ly, tablero, f, fichas), f, fichas)
  };System.out.println("""rellenar: (x: Int, ly: Int, tablero: List[List[Int]], f: Int, fichas: List[Int])List[List[Int]]""");$skip(125); 
  
  def rellenarN1(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(6, 8, tablero, 4, fichas)
  };System.out.println("""rellenarN1: (tablero: List[List[Int]], fichas: List[Int])List[List[Int]]""");$skip(127); 
  
  def rellenarN2(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(10, 16, tablero, 5, fichas)
  };System.out.println("""rellenarN2: (tablero: List[List[Int]], fichas: List[Int])List[List[Int]]""");$skip(127); 
  
  def rellenarN3(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(14, 26, tablero, 6, fichas)
  };System.out.println("""rellenarN3: (tablero: List[List[Int]], fichas: List[Int])List[List[Int]]""");$skip(1091); 
  
  /*val fichas1 = iniFichasN1()
  val tablero1 = iniTableroN1(fichas1)
  val fichas2 = iniFichasN2()
  val tablero2 = iniTableroN2(fichas2)
  val fichas3 = iniFichasN3()
  val tablero3 = iniTableroN3(fichas3)
  val act12 = setPosicionBool(true, 3, 4, act1)
  val tablero12 = borradoTab(tablero1, act12)
  val tablero5 = iniTableroN1(List(4,2,4,2))
  imprimir(tablero5)
  printf("\n")
  val a5 = seleccionarFicha(3, 4, tablero5, act1)
  val t5_1 = borradoTab(tablero5, a5)
  imprimir(t5_1)
  printf("\n")
  val t5_2 = rellenarN1(t5_1, List(1,3,1,3))
  print("\n")
  imprimir(t5_2)
  printf("\n")*/
  
  
  /**
   * le pasamos la posicion, comprueba si se puede borrar
   * y llama a una funcion auxiliar para comprobar todos los que tendra que borrar, que es la lista que devuelve
   * Esta configurado para que empiece por 0, pero si cambiamos ==0 por ==1 serian desde 1 y habria que cambiar seleccionarFicha
   */
   // funciona bien
  def getPosicion(x: Int, y: Int, tablero: List[List[Int]]): Int = {
    val laux: List[Int] = getPosicionY(y, tablero)
    getPosicionX(x, laux)
  };System.out.println("""getPosicion: (x: Int, y: Int, tablero: List[List[Int]])Int""");$skip(310); 
 //y tiene lista de listas
 //x tiene lista de enteros
  def getPosicionY(y: Int, tablero: List[List[Int]]): List[Int] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (y == 0) {
        tablero.head
      } else {
        getPosicionY(y - 1, tablero.tail)
      }
    }
  };System.out.println("""getPosicionY: (y: Int, tablero: List[List[Int]])List[Int]""");$skip(231); 

  def getPosicionX(x: Int, fila: List[Int]): Int = {
    if (fila.isEmpty) { //si no hay tablero
      -1
    } else {
      if (x == 0) {
        fila.head
      } else {
        getPosicionX(x - 1, fila.tail)
      }
    }
  };System.out.println("""getPosicionX: (x: Int, fila: List[Int])Int""");$skip(280); 

  //getPosicionY(1, tablero3)
  //getPosicion(1, 7, tablero3)

  //igual pero de bool, funciona bien

  def getPosicionBool(x: Int, y: Int, tablero: List[List[Boolean]]): Boolean = {
    val laux: List[Boolean] = getPosicionYBool(y, tablero)
    getPosicionXBool(x, laux)
  };System.out.println("""getPosicionBool: (x: Int, y: Int, tablero: List[List[Boolean]])Boolean""");$skip(273); 

  def getPosicionYBool(y: Int, tablero: List[List[Boolean]]): List[Boolean] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (y == 0) {
        tablero.head
      } else {
        getPosicionYBool(y - 1, tablero.tail)
      }
    }
  };System.out.println("""getPosicionYBool: (y: Int, tablero: List[List[Boolean]])List[Boolean]""");$skip(250); 

  def getPosicionXBool(x: Int, fila: List[Boolean]): Boolean = {
    if (fila.isEmpty) { //si no hay tablero
      false
    } else {
      if (x == 0) {
        fila.head
      } else {
        getPosicionXBool(x - 1, fila.tail)
      }
    }
  };System.out.println("""getPosicionXBool: (x: Int, fila: List[Boolean])Boolean""");$skip(374); 

  //getPosicionY(1, tablero3)
  //getPosicion(1, 7, tablero3)
  
  //PONE LA POSICON A LO QUE LE PASES, funciona guay

  def setPosicionBool(v:Boolean, x: Int, y: Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    if (tablero.isEmpty || x<0 || y<0 || y>=tablero.size || x>=tablero.head.size) {
    	tablero
    } else setPosicionYBool(v, x, y, tablero)
  };System.out.println("""setPosicionBool: (v: Boolean, x: Int, y: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(361); 
  
  def setPosicionYBool(v:Boolean, x: Int, y:Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
      if (y == 0) {
        setPosicionXBool(v, x, tablero.head) :: tablero.tail
      } else {
        tablero.head :: setPosicionYBool(v, x, y - 1, tablero.tail)
      }
    }
  };System.out.println("""setPosicionYBool: (v: Boolean, x: Int, y: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(264); 

  def setPosicionXBool(v:Boolean, x: Int, fila: List[Boolean]): List[Boolean] = {
  	if (fila.isEmpty) {
  		null
  	} else {
	  		if (x == 0) {
	        v :: fila.tail
	      } else {
	       fila.head :: setPosicionXBool(v, x - 1, fila.tail)
	      }
  	}
  };System.out.println("""setPosicionXBool: (v: Boolean, x: Int, fila: List[Boolean])List[Boolean]""");$skip(155); 
  
 
 
 //Bomba horizontal
 def setFilaBool(v:Boolean, y: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {
    setFilaBoolY(v, y, actuar)
  };System.out.println("""setFilaBool: (v: Boolean, y: Int, actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(358); 
  
  def setFilaBoolY(v:Boolean, y: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {//recorre buscando la fila y
    if (actuar.isEmpty) { //si no hay tablero
      null
    } else {
      if (y == 0) {
        setFilaBoolX(v, actuar.head) :: actuar.tail
      } else {
        actuar.head :: setFilaBoolY(v, y - 1, actuar.tail)
      }
    }
  };System.out.println("""setFilaBoolY: (v: Boolean, y: Int, actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(200); 

  def setFilaBoolX(v:Boolean, fila: List[Boolean]): List[Boolean] = {//pone la fila a v
      if (fila.isEmpty) {
        Nil
      } else {
       v :: setFilaBoolX(v, fila.tail)
      }
    
  };System.out.println("""setFilaBoolX: (v: Boolean, fila: List[Boolean])List[Boolean]""");$skip(157); 
  
  //bomba vertical
  def setColumnaBool(v:Boolean, x: Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    setColumnaBoolY(v, x, tablero)
  };System.out.println("""setColumnaBool: (v: Boolean, x: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(267); 
  
  def setColumnaBoolY(v:Boolean, x:Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {//recorre todas las filas
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
        setColumnaBoolX(v, x, tablero.head) :: tablero.tail

    }
  };System.out.println("""setColumnaBoolY: (v: Boolean, x: Int, tablero: List[List[Boolean]])List[List[Boolean]]""");$skip(257); 

  def setColumnaBoolX(v:Boolean, x: Int, fila: List[Boolean]): List[Boolean] = {//cambia el valor x de la fila pasa por v
      if (x == 0) {
        v :: fila.tail
      } else {
       fila.head :: setColumnaBoolX(v, x - 1, fila.tail)
      }
    
  };System.out.println("""setColumnaBoolX: (v: Boolean, x: Int, fila: List[Boolean])List[Boolean]""");$skip(541); 
  
  //TNT
  
  def tnt(x: Int, y: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {//por comprobar
  
  	val aux1 = setPosicionBool(true, x+1, y, actuar)
  	val aux2 = setPosicionBool(true, x+1, y+1, aux1)
  	val aux3 = setPosicionBool(true, x+1, y-1, aux2)
  	val aux4 = setPosicionBool(true, x-1, y, aux3)
  	val aux5 = setPosicionBool(true, x-1, y+1, aux4)
  	val aux6 = setPosicionBool(true, x-1, y-1, aux5)
  	val aux7 = setPosicionBool(true, x, y+1, aux6)
  	val aux8 = setPosicionBool(true, x, y-1, aux7)
  	aux8
  
  };System.out.println("""tnt: (x: Int, y: Int, actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(243); 
  
   def rompecabezas(color: Int, tablero: List[List[Int]]): List[List[Boolean]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
    	rompecabezasY(color, tablero.head)::rompecabezas(color, tablero.tail)
    }
  };System.out.println("""rompecabezas: (color: Int, tablero: List[List[Int]])List[List[Boolean]]""");$skip(288); 

  def rompecabezasY(color:Int, fila: List[Int]): List[Boolean] ={
    if (fila.isEmpty) { //si no hay tablero
      null
    } else {
    	if (fila.head == color){
    			true::rompecabezasY(color, fila.tail)
    	} else {
    			false::rompecabezasY(color, fila.tail)
    	}
    }
  };System.out.println("""rompecabezasY: (color: Int, fila: List[Int])List[Boolean]""");$skip(1912); 

  

  //POSIBLE OPTIMIZACION PASAR AL AUX EL VALOR QUE TENEMSO QUE BUSCAR PARA NO MIRAR TODO EL RATO, IGUAL QUE AQUI EN LOS IF
  def seleccionarFicha(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Boolean]] = {
 		//comprobamos si es bomba
 		
 		if (getPosicion(x, y, tablero) == -1) { //vertical
 				setColumnaBool(true, x, actuar)
 		} else if  (getPosicion(x, y, tablero) == -2) { //horizontal
 				setFilaBool(true, x, actuar)
 		} else if  (getPosicion(x, y, tablero) == -3) { //TNT
 				tnt(x, y, actuar)
 		} else if  (getPosicion(x, y, tablero) >= 11) { //Rompecabezas
 				rompecabezas((getPosicion(x, y, tablero)-10), tablero)
 		} else {
 		
    val laux1 =
      if ((((x - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x - 1, y, tablero))) { //la x mayor o igual que 0
        //PONER A TRUEEE
        seleccionarFichaAux(x, y, tablero, setPosicionBool(true, x, y, actuar))
      } else {
        actuar
      }

    val laux2 =
      if ((((y - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x, y - 1, tablero))) { //la y mayor o igual que 0
        //PONER A TRUEEE
        seleccionarFichaAux(x, y, tablero, setPosicionBool(true, x, y, laux1))
      } else {
        laux1
      }

    val laux3 =
      if ((((x + 1) < tablero.length) && getPosicion(x, y, tablero) == getPosicion(x + 1, y, tablero))) { //la xmenor que la longitud del tablero (x)
        //PONER A TRUEEE
        seleccionarFichaAux(x, y, tablero, setPosicionBool(true, x, y, laux2))
      } else {
        laux2
      }
      

    val laux4 =
      if ((((y + 1) < getPosicionY(y, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero))) { //la y menor que la longitud de y
        //PONER A TRUEEE
        seleccionarFichaAux(x, y, tablero,  setPosicionBool(true, x, y, laux3))
      } else {
        laux3
      }

    laux4
    }
  };System.out.println("""seleccionarFicha: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(1336); 

  def seleccionarFichaAux(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Boolean]] = {
    //TRUEEEEEEEE
    
   
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
      if ((((y + 1) < getPosicionY(y, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero)) && !getPosicionBool(x, y+1, laux3)) { //la y menor que la longitud de y
        seleccionarFichaAux(x, y+1, tablero, laux3)
      } else {
        laux3
      }

    laux4
  };System.out.println("""seleccionarFichaAux: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Boolean]]""");$skip(236); 
  
 def imprimir(fichas:List[Int], stats:List[Int], tablero:List[List[Int]]){
 		if (!tablero.isEmpty){
 			print("  - ")
 			imprimirContadores(fichas, stats)+imprimirCabecera(0, tablero.head.size)+imprimirTablero(0, tablero)
 		}
 };System.out.println("""imprimir: (fichas: List[Int], stats: List[Int], tablero: List[List[Int]])Unit""");$skip(155); 
  
 def imprimirCabecera(c:Int, l:Int): Int={
 		if (c==l){
 			print("\n\n")
 			-1
 		} else {
 			print(c + "  ")
 			imprimirCabecera(c+1, l)
 		}
 };System.out.println("""imprimirCabecera: (c: Int, l: Int)Int""");$skip(244); 
  
 def imprimirTablero(c:Int, tablero: List[List[Int]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	print(c + " - ")
    	imprimirTableroY(tablero.head)
    	imprimirTablero(c+1, tablero.tail)
    }
  };System.out.println("""imprimirTablero: (c: Int, tablero: List[List[Int]])Int""");$skip(218); 

  def imprimirTableroY(fila: List[Int]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(colocarColores(fila.head) + "  ")
    	imprimirTableroY(fila.tail)
    }
  };System.out.println("""imprimirTableroY: (fila: List[Int])Int""");$skip(523); 
  
    //cambiamos los numeros por colores
  def colocarColores(color: Int): Char ={
    color match {
    	case 0 => '_'
      case 1 => 'A'
      case 2 => 'R'
      case 3 => 'N'
      case 4 => 'V'
      case 5 => 'P'
      case 6 => 'M'
      case 7 => 'G'
      case 8 => 'B'
      case -1 => '^'
      case -2 => '>'
      case -3 => '*'
      case 11 => 'a'
      case 12 => 'r'
      case 13 => 'n'
      case 14 => 'v'
      case 15 => 'p'
      case 16 => 'm'
      case 17 => 'g'
      case 18 => 'b'
    }
  };System.out.println("""colocarColores: (color: Int)Char""");$skip(341); 
  
  def imprimirContadores(fichas: List[Int], contador:List[Int]): Int={
        if (fichas.isEmpty){
             -1
         } else {
            val aux = fichas.head-1
             print( "Fichas de color " + colocarColores(aux) + " = "+getPosicionX(aux, contador))
             imprimirContadores(fichas.tail, contador)
         }
 	};System.out.println("""imprimirContadores: (fichas: List[Int], contador: List[Int])Int""");$skip(228); 
  
   def imprimirTableroBool(tablero: List[List[Boolean]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	imprimirTableroYBool(tablero.head)
    	imprimirTableroBool(tablero.tail)
    }
  };System.out.println("""imprimirTableroBool: (tablero: List[List[Boolean]])Int""");$skip(214); 

  def imprimirTableroYBool(fila: List[Boolean]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(fila.head + "  ")
    	imprimirTableroYBool(fila.tail)
    }
  };System.out.println("""imprimirTableroYBool: (fila: List[Boolean])Int""");$skip(179); 
  
   def contarBorrar(l: List[List[Boolean]]): Int = {
    if (l.isEmpty) { //si no hay tablero
      0
    } else {
    	contarBorrarY(l.head) + contarBorrar(l.tail)
    }
  };System.out.println("""contarBorrar: (l: List[List[Boolean]])Int""");$skip(213); 

  def contarBorrarY(l: List[Boolean]): Int ={
    if (l.isEmpty) { //si no hay tablero
      0
    } else {
    val sumar =
    if(l.head == true) 1
    else 0
    
    	sumar + contarBorrarY(l.tail)
    }
  };System.out.println("""contarBorrarY: (l: List[Boolean])Int""");$skip(769); 
  
  //borramos, comprobamos esto que pone la bomba y rellenamos despues haciendo que baje
  def colocarBomba(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Int]] = {

    val borrar = contarBorrar(actuar)

    if (borrar == 5) {
      //Ventilador
      val aux = (Random.nextInt(2))
      if (aux == 1) {
        //Ventilador vertical -1
        setPosicionInt(-1, x, y, tablero)
      } else {
        //Ventilador horizcontal -2
        setPosicionInt(-2, x, y, tablero)
      }
    } else if (borrar == 6) {
      //TNT -3
      setPosicionInt(-1, x, y, tablero)
    } else if (borrar == 7) {
      //Rompecabezas 11-18
      setPosicionInt(getPosicion(x, y, tablero) + 10, x, y, tablero)
    } else {
      tablero
    }
  };System.out.println("""colocarBomba: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[List[Int]]""");$skip(1329); 
  /*
  imprimir(tablero1)
  imprimirTableroBool(act1)
  
  // val salidaAux11 =setPosicionBool(true, 1, 1, act1)
   val salidaAux11 = seleccionarFicha(1,5,tablero1,act1)
   val tableroBomba = colocalBomba(1,5,tablero1,salidaAux11)

imprimirTableroBool(salidaAux11)

imprimir(tableroBomba)

	print("numero a borrar " + contarBorrar(salidaAux11))*/


	/*def guardar ( tablero: List[List[Int]], fichas: List[Int], contador:List[Int]) {
	printToFile(new File("tablero.txt")) { p =>
	 tablero.foreach(p.println)
	}
	printToFile(new File("fichas.txt")) { p =>
	 fichas.foreach(p.println)
	}
	printToFile(new File("contador.txt")) { p =>
	 contador.foreach(p.println)
	}
	}
	
	def cargar(): (List[List[Int]], List[Int], List[Int])={ //tablero,  fichas, contador
	    val bufferedSource = io.Source.fromFile("tablero.txt")
	    val tablero= (for (line <- bufferedSource.getLines()) yield line).toList
	    bufferedSource.close
	
			val bufferedSource = io.Source.fromFile("fichas.txt")
	    val fichas= (for (line <- bufferedSource.getLines()) yield line).toList
	    bufferedSource.close
	
			val bufferedSource = io.Source.fromFile("contador.txt")
	    val contador= (for (line <- bufferedSource.getLines()) yield line).toList
	    bufferedSource.close
	
	    (tablero, fichas, contador)
	}*/

  def identificarPatrones() = {
  };System.out.println("""identificarPatrones: ()Unit""");$skip(160); 
  
  def optimizacion(tablero:List[List[Int]], actuar:List[List[Boolean]]): List[Int]={
	    optimizacionY(tablero.head.size, tablero.size, tablero, actuar)
	};System.out.println("""optimizacion: (tablero: List[List[Int]], actuar: List[List[Boolean]])List[Int]""");$skip(367); 
	
	def optimizacionY (x: Int, y:Int, tablero:List[List[Int]], actuar:List[List[Boolean]]):  List[Int]={
	    if(y<0)
	        List(0,x,y)
	    else {
	        val aux = optimizacionX(x, y, tablero.head, actuar)
	        val aux2 = optimizacionY(x,y-1, tablero.tail, actuar)
	        if(aux.head < aux2.head)
	            aux2
	        else
	            aux
	    }
	};System.out.println("""optimizacionY: (x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]])List[Int]""");$skip(377); 
	
	def optimizacionX(x:Int, y:Int, lista:List[Int], actuar:List[List[Boolean]]): List[Int] ={
	    if (x<0)
	        List(0,x,y)
	    else {
	        val aux = List(1,x,y)//contarBorrar(seleccionarficha(x, y, lista, actuar))
	        val aux2 = optimizacionX(x-1, y, lista, actuar)
	        if(aux.head < aux2.head)
	            aux2
	        else
	            aux
	
	    }
	};System.out.println("""optimizacionX: (x: Int, y: Int, lista: List[Int], actuar: List[List[Boolean]])List[Int]""");$skip(31); 

  def estadisticas() = {
  };System.out.println("""estadisticas: ()Unit""");$skip(30); 
  
  val act1 = iniActuarN1();System.out.println("""act1  : List[List[Boolean]] = """ + $show(act1 ));$skip(27); 
  val act2 = iniActuarN2();System.out.println("""act2  : List[List[Boolean]] = """ + $show(act2 ));$skip(27); 
  val act3 = iniActuarN3();System.out.println("""act3  : List[List[Boolean]] = """ + $show(act3 ));$skip(179); 
  
  def checkN1(c:Int, stats:List[Int]): Boolean={
  	if (stats.isEmpty) c==4
  	else {
  		if (stats.head>=20) checkN1(c+1, stats.tail)
  		else checkN1(c, stats.tail)
  	}
  };System.out.println("""checkN1: (c: Int, stats: List[Int])Boolean""");$skip(179); 
  
  def checkN2(c:Int, stats:List[Int]): Boolean={
  	if (stats.isEmpty) c==5
  	else {
  		if (stats.head>=15) checkN2(c+1, stats.tail)
  		else checkN2(c, stats.tail)
  	}
  };System.out.println("""checkN2: (c: Int, stats: List[Int])Boolean""");$skip(179); 
  
  def checkN3(c:Int, stats:List[Int]): Boolean={
  	if (stats.isEmpty) c==6
  	else {
  		if (stats.head>=10) checkN3(c+1, stats.tail)
  		else checkN3(c, stats.tail)
  	}
  };System.out.println("""checkN3: (c: Int, stats: List[Int])Boolean""");$skip(893); 
  
  //--------------------------- juego -----------------------------------------
  
  def juegoN1(c:Int, fichas:List[Int], tablero:List[List[Int]], stats:List[Int]): Int={
  	if (tablero.isEmpty) juegoN1(c, fichas, iniTableroN1(fichas), stats)
  	else if (checkN1(0, stats)) 0
  	else {
  		imprimir(fichas, stats, tablero)
  		print("\nIntroduce la X: ")
  		val x = readInt()
  		print("\nIntroduce la Y: ")
  		val y = readInt()
  		
  		if (x<0 || x>6 || y<0 || y>8) {
  			println("\nERROR - Introduzca un ficha del rango del tablero\n")
  			juegoN1(c, fichas, tablero, stats)
  		} else {
  			val actAux = seleccionarFicha(x, y, tablero, act1)
  			val borrado = borradoTab(tablero, actAux, stats)
  			imprimir(fichas, borrado._2, borrado._1)
  			val relleno = rellenarN1(borrado._1, fichas)
  			
  			contarBorrar(actAux)+juegoN1(c+1, fichas, relleno, borrado._2)
  		}
  	}
  };System.out.println("""juegoN1: (c: Int, fichas: List[Int], tablero: List[List[Int]], stats: List[Int])Int""");$skip(812); 
  
    def juegoN2(c:Int, fichas:List[Int], tablero:List[List[Int]], stats:List[Int]): Int={
  	if (tablero.isEmpty) juegoN2(c, fichas, iniTableroN2(fichas), stats)
  	else if (checkN2(0, stats)) 0
  	else {
  		imprimir(fichas, stats, tablero)
  		print("\nIntroduce la X: ")
  		val x = readInt()
  		print("\nIntroduce la Y: ")
  		val y = readInt()
  		
  		if (x<0 || x>6 || y<0 || y>8) {
  			println("\nERROR - Introduzca un ficha del rango del tablero\n")
  			juegoN2(c, fichas, tablero, stats)
  		} else {
  			val actAux = seleccionarFicha(x, y, tablero, act1)
  			val borrado = borradoTab(tablero, actAux, stats)
  			imprimir(fichas, borrado._2, borrado._1)
  			val relleno = rellenarN2(borrado._1, fichas)
  			
  			contarBorrar(actAux)+juegoN2(c+1, fichas, relleno, borrado._2)
  		}
  	}
  };System.out.println("""juegoN2: (c: Int, fichas: List[Int], tablero: List[List[Int]], stats: List[Int])Int""");$skip(857); 
  
    def juegoN3(c:Int, fichas:List[Int], tablero:List[List[Int]], stats:List[Int]): Int={
  	if (tablero.isEmpty) juegoN3(c, fichas, iniTableroN3(fichas), stats)
  	else if (checkN3(0, stats)) 0
  	else {
  		imprimir(fichas, stats, tablero)
  		print("\nIntroduce la X: ")
  		val x = readInt
  		print("\nIntroduce la Y: ")
  		val y = readInt
  		
  		if (x<0 || x>6 || y<0 || y>8) {
  			println("\nERROR - Introduzca un ficha del rango del tablero\n")
  			juegoN3(c, fichas, tablero, stats)
  		} else {
  			val actAux = seleccionarFicha(x, y, tablero, act1)
  			val borrado = borradoTab(tablero, actAux, stats)
  			val newTab = colocarBomba(x, y, borrado._1, actAux)
  			imprimir(fichas, borrado._2, newTab)
  			val relleno = rellenarN3(newTab, fichas)
  			
  			contarBorrar(actAux)+juegoN3(c+1, fichas, relleno, borrado._2)
  		}
  	}
  };System.out.println("""juegoN3: (c: Int, fichas: List[Int], tablero: List[List[Int]], stats: List[Int])Int""");$skip(327); 
  
  //---------------------------------------------------------------------------
  
  def jugar(nivel:Int): Int={
  	nivel match {
  		case 1 => juegoN1(0, iniFichasN1(), Nil, List.fill(8)(0))
  		case 2 => juegoN2(0, iniFichasN2(), Nil, List.fill(8)(0))
			case 3 => juegoN3(0, iniFichasN3(), Nil, List.fill(8)(0))
  	}
  };System.out.println("""jugar: (nivel: Int)Int""");$skip(23); 
  
  val dif = readInt;System.out.println("""dif  : Int = """ + $show(dif ));$skip(27); 
  
	val juego = jugar(dif);System.out.println("""juego  : Int = """ + $show(juego ));$skip(45); 

	println("\n\nPuntuación Final: " + juego)}
}
