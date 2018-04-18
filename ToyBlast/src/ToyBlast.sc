object ToyBlast {
  println("Welcome to the Scala workshit")
  
  import util.Random
  
  val (r1)= (1+Random.nextInt(4))
  
  def iniFichasNivel(n:Int, l:List[Int]): List[Int]={
  	val c:Int = (1+Random.nextInt(8)) //tomamos un color del 1 al 8
  	val laux:List[Int] = l:::c::Nil
  	if (n == 0) l
  	else
  		if (l.contains(c)) iniFichasNivel(n, l)
  		else iniFichasNivel(n-1, laux)
  }
  
  def iniTablerox(y:Int, n:Int, fichas:List[Int]): List[Int]={
  	if (y == 0) Nil
  	else iniTablerox(y-1, n, fichas):::fichas.apply(Random.nextInt(n))::Nil
  }
  
  def iniTablero(x:Int, y:Int, n:Int, fichas:List[Int]): List[List[Int]]={
  	if (x == 0) Nil
  	else iniTablero(x-1, y, n, fichas):::iniTablerox(y, n, fichas)::Nil
  }
  
  //da igual como borremos, asi que borramos por filas
  def borradoTabx(lista:List[Int], actuarl:List[Boolean]): List[Int]={// se encarga de las filas borra los que estén a true en actuarl
  	if (lista.isEmpty) Nil
  	else if (actuarl.head) 0::borradoTabx(lista.tail, actuarl.tail)
  		else lista.head::borradoTabx(lista.tail, actuarl.tail)
  }
  
  def borradoTab(tablero:List[List[Int]], actuar:List[List[Boolean]]): List[List[Int]]={
  	if (tablero.isEmpty) Nil
  	else borradoTabx(tablero.head, actuar.head)::borradoTab(tablero.tail, actuar.tail)
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
  
  //Adaptación de setPosicionBool para enteros
  
  def setPosicionInt(v:Int, x: Int, y: Int, tablero: List[List[Int]]): List[List[Int]] = {
    setPosicionXInt(v, x, y, tablero)
  }
  
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
  }

  def setPosicionYInt(v:Int, y: Int, fila: List[Int]): List[Int] = {
      if (y == 0) {
        v :: fila.tail
      } else {
       fila.head :: setPosicionYInt(v, y - 1, fila.tail)
      }
    
  }
  
  def tomaColumna(x:Int, y:Int, tablero:List[List[Int]]): List[Int]={//devuelve la columna X
  	if (y<0 || x<0) Nil
  	else tablero.head.apply(x)::tomaColumna(x, y-1, tablero.tail)
  }
   
  def tomaFicha(y:Int, columna:List[Int]): List[Int]={//toma la primera ficha mayor que 0 que encuentre antes del nivel 0
  	if (y<0) List(0, 0) // por seguridad
  	else if (columna.apply(y) == 0) tomaFicha(y-1, columna)
  		else List(columna.apply(y), y)
  }
  
  def rellenaColumnas(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	val c = tomaColumna(x, y, tablero)
  	val tomada = tomaFicha(y-1, c)
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0 || x<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0)
  			if (tomada.head == 0) rellenaColumnas(x, y-1, setPosicionInt(r,x,y,tablero), f, fichas)
  			else rellenaColumnas(x, y-1, setPosicionInt(tomada.head,x,y,setPosicionInt(0, x, tomada.last, tablero)), f, fichas)
  		else rellenaColumnas(x, y-1, tablero, f, fichas)
  }
   
  def rellenaColumnasOptRand(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//rellena desde esa posición con enteros aleatorios posibles
  	val r = fichas.apply(Random.nextInt(f))
  	if (y<0 || x<0) tablero
  	else rellenaColumnasOptRand(x, y-1, setPosicionInt(r,x,y,tablero), f, fichas)
  }
  
  def rellenaColumnasOpt(x:Int, y:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={//Optimizado
  	val c = tomaColumna(x, y, tablero)
  	val tomada = tomaFicha(y-1, c)
  	if (y<0 || x<0) tablero
  	else
  		if (tablero.apply(y).apply(x) == 0)
  			if (tomada.head == 0) rellenaColumnasOptRand(x, y, tablero, f, fichas)//si no ha encontrado, los siguientes darán igual resultado, no busca más
  			else rellenaColumnasOpt(x, y-1, setPosicionInt(tomada.head,x,y,setPosicionInt(0, x, tomada.last, tablero)), f, fichas)
  		else rellenaColumnasOpt(x, y-1, tablero, f, fichas)
  }
  
  def rellenar(x:Int, ly:Int, tablero:List[List[Int]], f:Int, fichas:List[Int]): List[List[Int]]={
  	if (x<0) tablero
  	else rellenar(x-1, ly, rellenaColumnasOpt(x, ly, tablero, f, fichas), f, fichas)
  }
  
  def rellenarN1(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(7, 9, tablero, 4, fichas)
  }
  
  def rellenarN2(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(11, 17, tablero, 5, fichas)
  }
  
  def rellenarN3(tablero:List[List[Int]], fichas:List[Int]): List[List[Int]]={
  	rellenar(15, 27, tablero, 6, fichas)
  }
  
  val fichas1 = iniFichasN1()
  val tablero1 = iniTableroN1(fichas1)
  val fichas2 = iniFichasN2()
  val tablero2 = iniTableroN2(fichas2)
  val fichas3 = iniFichasN3()
  val tablero3 = iniTableroN3(fichas3)
  val act1 = iniActuarN1()
  val act2 = iniActuarN2()
  val act3 = iniActuarN3()
  val act12 = setPosicionBool(true, 3, 4, act1)
  val tablero12 = borradoTab(tablero1, act12)
  val tablero5 = iniTableroN1(List(4,4,4,4))
  imprimirTablero(tablero5)
  
  
  /**
   * le pasamos la posicion, comprueba si se puede borrar
   * y llama a una funcion auxiliar para comprobar todos los que tendra que borrar, que es la lista que devuelve
   * Esta configurado para que empiece por 0, pero si cambiamos ==0 por ==1 serian desde 1 y habria que cambiar seleccionarFicha
   */
   // funciona bien
  def getPosicion(x: Int, y: Int, tablero: List[List[Int]]): Int = {
    val laux: List[Int] = getPosicionX(x, tablero)
    getPosicionY(y, laux)
  }
 
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
  }

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
  }

  getPosicionX(1, tablero3)
  getPosicion(1, 7, tablero3)

  //igual pero de bool, funciona bien

  def getPosicionBool(x: Int, y: Int, tablero: List[List[Boolean]]): Boolean = {
    val laux: List[Boolean] = getPosicionXBool(x, tablero)
    getPosicionYBool(y, laux)
  }

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
  }

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
  }

  getPosicionX(1, tablero3)
  getPosicion(1, 7, tablero3)
  
  //PONE LA POSICON A LO QUE LE PASES, funciona guay

  def setPosicionBool(v:Boolean, x: Int, y: Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    setPosicionXBool(v, x, y, tablero)
  }
  
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
  }

  def setPosicionYBool(v:Boolean, y: Int, fila: List[Boolean]): List[Boolean] = {
      if (y == 0) {
        v :: fila.tail
      } else {
       fila.head :: setPosicionYBool(v, y - 1, fila.tail)
      }
    
  }
  
 
 
 //Bomba horizontal
 def setFilaBool(v:Boolean, x: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {
    setFilaBoolX(v, x, actuar)
  }
  
  def setFilaBoolX(v:Boolean, x: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {
    if (actuar.isEmpty) { //si no hay tablero
      null
    } else {
      if (x == 0) {
        setFilaBoolY(v, actuar.head) :: actuar.tail
      } else {
        actuar.head :: setFilaBoolX(v, x - 1, actuar.tail)
      }
    }
  }

  def setFilaBoolY(v:Boolean, fila: List[Boolean]): List[Boolean] = {
      if (fila.isEmpty) {
        Nil
      } else {
       v :: setFilaBoolY(v, fila.tail)
      }
    
  }
  
  //bomba vertical
  def setColumnaBool(v:Boolean, y: Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    setColumnaBoolX(v, y, tablero)
  }
  
  def setColumnaBoolX(v:Boolean, y:Int, tablero: List[List[Boolean]]): List[List[Boolean]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
        setColumnaBoolY(v, y, tablero.head) :: tablero.tail

    }
  }

  def setColumnaBoolY(v:Boolean, y: Int, fila: List[Boolean]): List[Boolean] = {
      if (y == 0) {
        v :: fila.tail
      } else {
       fila.head :: setColumnaBoolY(v, y - 1, fila.tail)
      }
    
  }
  
  //TNT
  
  def tnt(x: Int, y: Int, actuar: List[List[Boolean]]): List[List[Boolean]] = {
  
  	val aux1 = setPosicionBool(true, x+1, y, actuar)
  	val aux2 = setPosicionBool(true, x+1, y+1, aux1)
  	val aux3 = setPosicionBool(true, x+1, y-1, aux2)
  	val aux4 = setPosicionBool(true, x-1, y, aux3)
  	val aux5 = setPosicionBool(true, x-1, y+1, aux4)
  	val aux6 = setPosicionBool(true, x-1, y-1, aux5)
  	val aux7 = setPosicionBool(true, x, y+1, aux6)
  	val aux8 = setPosicionBool(true, x, y-1, aux7)
  	aux8
  
  }
  
   def rompecabezas(color: Int, tablero: List[List[Int]]): List[List[Boolean]] = {
    if (tablero.isEmpty) { //si no hay tablero
      null
    } else {
    	rompecabezasY(color, tablero.head )
    	rompecabezas(color, tablero.tail)
    }
  }

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
  }

  

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
 		}
 		
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
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero))) { //la y menor que la longitud de y
        //PONER A TRUEEE
        seleccionarFichaAux(x, y, tablero,  setPosicionBool(true, x, y, laux3))
      } else {
        laux3
      }

    laux4
  }

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
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero)) && !getPosicionBool(x, y+1, laux3)) { //la y menor que la longitud de y
        seleccionarFichaAux(x, y+1, tablero, laux3)
      } else {
        laux3
      }

    laux4
  }
  
 def imprimirTablero(tablero: List[List[Int]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	imprimirTableroY(tablero.head)
    	imprimirTablero(tablero.tail)
    }
  }

  def imprimirTableroY(fila: List[Int]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(fila.head + "  ")
    	imprimirTableroY(fila.tail)
    }
  }
  
   def imprimirTableroBool(tablero: List[List[Boolean]]): Int = {
    if (tablero.isEmpty) { //si no hay tablero
      -1
    } else {
    	imprimirTableroYBool(tablero.head)
    	imprimirTableroBool(tablero.tail)
    }
  }

  def imprimirTableroYBool(fila: List[Boolean]): Int ={
    if (fila.isEmpty) { //si no hay tablero
      println
      -1
    } else {
    	print(fila.head + "  ")
    	imprimirTableroYBool(fila.tail)
    }
  }
  
   def contarBorrar(l: List[List[Boolean]]): Int = {
    if (l.isEmpty) { //si no hay tablero
      0
    } else {
    	contarBorrarY(l.head) + contarBorrar(l.tail)
    }
  }

  def contarBorrarY(l: List[Boolean]): Int ={
    if (l.isEmpty) { //si no hay tablero
      0
    } else {
    val sumar =
    if(l.head == true) 1
    else 0
    
    	sumar + contarBorrarY(l.tail)
    }
  }
  
  //borramos, comprobamos esto que pone la bomba y rellenamos despues haciendo que baje
  def colocalBomba(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Int]] = {

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
  }
  
  imprimirTablero(tablero1)
  imprimirTableroBool(act1)
  
  // val salidaAux11 =setPosicionBool(true, 1, 1, act1)
   val salidaAux11 = seleccionarFicha(1,5,tablero1,act1)
   val tableroBomba = colocalBomba(1,5,tablero1,salidaAux11)

imprimirTableroBool(salidaAux11)

imprimirTablero(tableroBomba)

	print("numero a borrar " + contarBorrar(salidaAux11))

  def guardarPartida() = {
  }

  def identificarPatrones() = {
  }

  def estadisticas() = {
  }



}