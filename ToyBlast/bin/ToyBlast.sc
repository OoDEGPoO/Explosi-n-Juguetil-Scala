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
  val act3 = iniActuarN3()


//RAUL

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
        actuar
      }

    val laux3 =
      if ((((x + 1) < tablero.length) && getPosicion(x, y, tablero) == getPosicion(x + 1, y, tablero))) { //la xmenor que la longitud del tablero (x)
        //PONER A TRUEEE
        setPosicionBool(true, x, y, laux2)
        seleccionarFichaAux(x, y, tablero, laux2)
      } else {
        actuar
      }

    val laux4 =
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero))) { //la y menor que la longitud de y
        //PONER A TRUEEE
        setPosicionBool(true, x, y, laux3)
        seleccionarFichaAux(x, y, tablero, laux3)
      } else {
        actuar
      }

    laux4
  }

  def seleccionarFichaAux(x: Int, y: Int, tablero: List[List[Int]], actuar: List[List[Boolean]]): List[List[Boolean]] = {
    //TRUEEEEEEEE
    setPosicionBool(true, x, y, actuar)
    val laux1 =
      if ((((x - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x - 1, y, tablero)) && !getPosicionBool(x, y, actuar)) { //la x mayor o igual que 0
        seleccionarFichaAux(x, y, tablero, actuar)
      } else {
        actuar
      }

    val laux2 =
      if ((((y - 1) >= 0) && getPosicion(x, y, tablero) == getPosicion(x, y - 1, tablero)) && !getPosicionBool(x, y, laux1)) { //la y mayor o igual que 0
        seleccionarFichaAux(x, y, tablero, laux1)
      } else {
        actuar
      }

    val laux3 =
      if ((((x + 1) < tablero.length) && getPosicion(x, y, tablero) == getPosicion(x + 1, y, tablero)) && !getPosicionBool(x, y, laux2)) { //la xmenor que la longitud del tablero (x)
        seleccionarFichaAux(x, y, tablero, laux2)
      } else {
        actuar
      }

    val laux4 =
      if ((((y + 1) < getPosicionX(x, tablero).length) && getPosicion(x, y, tablero) == getPosicion(x, y + 1, tablero)) && !getPosicionBool(x, y, laux3)) { //la y menor que la longitud de y
        seleccionarFichaAux(x, y, tablero, laux3)
      } else {
        actuar
      }

    laux4
  }

  def guardarPartida() = {
  }

  def identificarPatrones() = {
  }

  def estadisticas() = {
  }



}