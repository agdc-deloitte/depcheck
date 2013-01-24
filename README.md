Verificador de dependencias
===========================

El programa de verificaci�n de dependencias, es un programa ABAP, que tiene como
objetivo obtener dependencias entre �rdenes de transporte y objetos de una orden.

1) Funcionamiento general
-------------------------

Su funcionamiento es el siguiente:
* Se indica una orden de transporte a analizar.
* Se obtienen los objetos de dicha orden de transporte.
* Para cada objeto se verifica en qu� ordenes de transporte existe y qu� objetos
  tiene como dependencias.
* Luego se toman dichas �rdenes y dichos objetos y se contin�a el an�lisis en 
  forma recursiva.
* Por �ltimo se muestra un arbol con todas las dependencias obtenidas.

Es importante hacer las siguientes aclaraciones:
* Un nodo tiene como dependientes a todos sus descendientes.
* Para tratar las dependencias cruzadas, lo que se hizo fue analizar cada objeto
  una �nica vez, de forma que, en el arbol las dependencias para un objeto dado 
  se muestran una �nica vez.
* Puede ocurrir que dos nodos A y B, no dependientes, tengan una misma 
  dependencia C. Tener en cuenta que las dependencias de C se mostrar�n una 
  �nica vez, en la rama de A o en la rama de B.

2) Opciones de ejecuci�n
------------------------

A continuaci�n se explica para que sirve cada uno de los par�metros de selecci�n:
* Orden/Tarea: Es la �rden de transporte a partir de la cu�l se va a comenzar a 
  realizar el an�lisis.
* Nombre de objetos en �rdenes: Filtro de los nombres de los objetos en �rdenes.
  Es recomendable no dejarlo vac�o, ya que sino el an�lisis se extender� a una 
  gran cantidad de objetos est�ndares, y causara una gran demora en la ejecuci�n
  del programa.
* Mostrar solo �rdenes: Si se encuentra tildado indica que en el arbol que se 
  obtenga como resultado del an�lisis, se muestren solo las �rdenes de transporte, 
  es decir, no se mostrar�n objetos como programas, tablas, funciones, etc.
* Mostrar solo ord. NO trans. a: Si se est� tildado indica que solo se deben 
  mostrar las �rdenes de transporte que NO se encuentren en el destino 
  especificado por el par�metro "Destino RFC". Tener en cuenta que al estar 
  tildado solo se mostrar�n ordenes de transporte en el arbol obtenido como 
  resultado.
* Destino RFC: Destino en el cual se verifica si una �rden de transporte existe 
  o NO existe en caso de estar tildado el par�metro "Mostrar solo ord. NO trans".

3) Entendiendo c�mo se obtienen las dependencias para cada objeto
-----------------------------------------------------------------

Ahora, valdr�a la pena preguntarse, �c�mo se obtienen las dependencias de cada 
objeto?
Bueno, justamente esa es la parte interesante de este programa. Lo que ofrece el
mismo es poder registrar para cada tipo de objeto una clase que se encargue de 
obtener sus dependencias.

Vamos a ver un ejemplo puntual, el del tipo de objeto Tabla.
Para empezar veamos la definici�n de la clase que se va a encargar de obtener 
las dependencias de las tablas:

    CLASS LCL_TABLA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.
    
      PUBLIC SECTION.
    
        METHODS:
    
          SELECT_DEPENDENCIAS REDEFINITION,
    
          GET_LINEA_TREE REDEFINITION.
    
    ENDCLASS.                    "LCL_TABLA DEFINITION

Analicemos un poco el c�digo. Nuestra clase se va a llamar LCL_TABLA y vemos que
hereda de la clas LCL_OBJETO_ORDEN.
Ac� tenemos el primer punto clave, toda clase que quiera obtener las dependencias
para un objeto dado debe extender LCL_OBJETO_ORDEN o alg�na sub clase de 
LCL_OBJETO_ORDEN.
** IMPORTANTE: Si redefine el constructor no cambiar la firma!!! La firma debe 
ser la misma que la definida en la clase LCL_OBJETO_ORDEN.**

Luego nos encontramos con dos m�todos:
* SELECT_DEPENDENCIAS: Este m�todo es el encargado de obtener las dependencias.
  No tiene par�metros.
* GET_LINEA_TREE: Este m�todo es el encargado de devolver los datos a ser 
  mostrados en el ALV tree. Tiene como �nico par�metro la l�nea a ser devuelta
  para ser mostrada por el ALV tree.

Veamos para empezar la implementaci�n del m�todo SELECT_DEPENDENCIAS para 
nuestra clase LCL_TABLA:

    METHOD SELECT_DEPENDENCIAS.
  
      TYPES:
        BEGIN OF TY_S_DD03L,
          ROLLNAME TYPE DD03L-ROLLNAME,
          DOMNAME TYPE DD03L-DOMNAME,
        END OF TY_S_DD03L.
  
      FIELD-SYMBOLS:
        <LS_DD03L> TYPE TY_S_DD03L.
  
      DATA:
        LT_DD03L TYPE STANDARD TABLE OF TY_S_DD03L.
  
      SUPER->SELECT_DEPENDENCIAS( ).
  
      " Tabla DD03L: A partir de TABNAME se pueden obtener ROLLNAME y DOMNAME
      SELECT ROLLNAME DOMNAME
      FROM DD03L
      INTO TABLE LT_DD03L
      WHERE
        TABNAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME AND
        ( ROLLNAME LIKE 'Z%' OR
        DOMNAME LIKE 'Z%' OR
        ROLLNAME LIKE 'Y%' OR
        DOMNAME LIKE 'Y%').
  
      LOOP AT LT_DD03L ASSIGNING <LS_DD03L>.
  
        IF <LS_DD03L>-ROLLNAME CP 'Z*' OR <LS_DD03L>-ROLLNAME CP 'Y*'.
  
          LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
            EXPORTING
              IV_CLASS_NAME = 'LCL_ELEMENTO_DATOS'
              IV_OBJ_NAME = <LS_DD03L>-ROLLNAME
            CHANGING
              CT_VERIFICABLES = ME->LT_DEPENDENCIAS
            ).
  
        ENDIF.
  
        IF <LS_DD03L>-DOMNAME CP 'Z*' OR <LS_DD03L>-DOMNAME CP 'Y*'.
  
          LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
            EXPORTING
              IV_CLASS_NAME = 'LCL_DOMINIO'
              IV_OBJ_NAME = <LS_DD03L>-DOMNAME
            CHANGING
              CT_VERIFICABLES = ME->LT_DEPENDENCIAS
            ).
  
        ENDIF.
  
      ENDLOOP.
  
    ENDMETHOD.                    "SELECT_DEPENDENCIAS

Varios puntos a tener en cuenta a la hora de realizar la implementaci�n del 
m�todo SELECT_DEPENDENCIAS:
* Siempre se deber�a llamar al principio del m�todo al m�todo padre:
  SUPER->SELECT_DEPENDENCIAS( ).
* Como se puede observar, hay una consulta donde se obtienen dominio y elementos 
  de datos de los depende la tabla en an�lisis, pero, �donde est� el nombre de
  la tabla analizada? Bueno, el nombre del objeto que se est� analizando siempre
  se podr� encontrar en ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
* Bien, una vez obtenidos dominios y elementos de datos, �donde indicamos que la
  tabla depende de estos? Eso se indica agregando una o varias entradas en la 
  tabla interna ME->LT_DEPENDENCIAS, pero se debe hacer de la siguiente forma:
  llamando al m�todo LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS que recibir� 
  los siguientes par�metros:
  * IV_CLASS_NAME: Nombre de la clase asociada al objeto que se tiene como 
    dependencia. En nuestro caso las clases son LCL_ELEMENTO_DATOS y LCL_DOMINIO.
    Dichas clases deben estar implementadas y deben extender a la clase 
    LCL_OBJETO_ORDEN o alguna sub clase de LCL_OBJETO_ORDEN.
  * IV_OBJ_NAME: Nombre del objeto. En nuestro caso ser� el nombre del dominio o
    nombre del elemento de datos.
  * CT_VERIFICABLES: Tabla donde se agregar�n las dependencias. Generalmente 
    siempre deber�a pasarse ME->LT_DEPENDENCIAS.

Y eso b�sicamente ser�a lo que debe tenerse en cuenta a la hora de implementar 
SELECT_DEPENDENCIAS. En resumen:
* Llamar a SUPER->SELECT_DEPENDENCIAS( )
* Agregar nuestra l�gica para obtener las dependencias del objeto 
  ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
* Por cada dependencia llamar al m�todo 
  LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS pasandole el nombre de la clase
  asociada con el tipo de objeto de la dependencia, el nombre de la dependencia 
  y la tabla interna donde se agregar�n las dependencias (ME->LT_DEPENDENCIAS).

Ahora veamos la implementaci�n del m�todo GET_LINEA_TREE para nuestra clase 
LCL_TABLA:

    METHOD GET_LINEA_TREE.
    
      LS_RESULT-TIPO = 'Tabla'.
  
    ENDMETHOD.

Bueno, como se ve es bastante simple. Lo �nico que se hace es indicar una 
descripci�n para el tipo de objeto.

4) Registrando nuestra clase
----------------------------

Est� claro que est� faltando algo, �c�mo puede ser que la aplicaci�n sepa que 
tiene que instanciar nuestra clase LCL_TABLA cuando encuentra una tabla en una 
orden de transporte?

Bueno, esto justamente se logra registrando nuestra clase para el tipo de objeto
que analizar� nuestra clase.
Es decir, lo que tenemos que hacer es indicar que la clase LCL_TABLA analizar� 
los siguientes tipos de objetos obtenidos de las �rdenes de transporte:
* Aquellos objetos que verifiquen PGMID = 'R3TR' y OBJECT = 'TABL'
* Aquellos objetos que verifiquen PGMID = 'LIMU' y OBJECT = 'TABD'

Esto se realiza de una forma muy simple. En el bloque INICIALIZATION del 
programa simplemente se obtiene el manejador de tipos y se registra la clase 
para dichos tipos de objetos:

    LV_MANEJADOR_TIPOS = LCL_MANEJADOR_TIPOS_OBJETO=>GET_INSTANCE( ).
  
    LV_MANEJADOR_TIPOS->REGISTRAR(
      IV_PGMID = 'R3TR'
      IV_OBJECT = 'TABL'
      IV_CLASE_ASOCIADA = 'LCL_TABLA'
      ).
  
    LV_MANEJADOR_TIPOS->REGISTRAR(
      IV_PGMID = 'LIMU'
      IV_OBJECT = 'TABD'
      IV_CLASE_ASOCIADA = 'LCL_TABLA'
      ).

Tan simple y tan dificil como eso.

5) Notas aclaratorias de los resultados obtenidos
-------------------------------------------------

Tener en cuenta que el arbol de dependencias mostrado est� furtemente ligado a
la implementaci�n de la obtenci�n de dependencias para cada tipo de objeto, 
por tanto la exactitud de los resultados obtenidos depender� de la exactitud
de la l�gica utilizada para la obtenci�n de dependencias para cada tipo de 
objeto.

