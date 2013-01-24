Verificador de dependencias
===========================

El programa de verificación de dependencias, es un programa ABAP, que tiene como
objetivo obtener dependencias entre órdenes de transporte y objetos de una orden.

1) Funcionamiento general
-------------------------

Su funcionamiento es el siguiente:
* Se indica una orden de transporte a analizar.
* Se obtienen los objetos de dicha orden de transporte.
* Para cada objeto se verifica en qué ordenes de transporte existe y qué objetos
  tiene como dependencias.
* Luego se toman dichas órdenes y dichos objetos y se continúa el análisis en 
  forma recursiva.
* Por último se muestra un arbol con todas las dependencias obtenidas.

Es importante hacer las siguientes aclaraciones:
* Un nodo tiene como dependientes a todos sus descendientes.
* Para tratar las dependencias cruzadas, lo que se hizo fue analizar cada objeto
  una única vez, de forma que, en el arbol las dependencias para un objeto dado 
  se muestran una única vez.
* Puede ocurrir que dos nodos A y B, no dependientes, tengan una misma 
  dependencia C. Tener en cuenta que las dependencias de C se mostrarán una 
  única vez, en la rama de A o en la rama de B.

2) Opciones de ejecución
------------------------

A continuación se explica para que sirve cada uno de los parámetros de selección:
* Orden/Tarea: Es la órden de transporte a partir de la cuál se va a comenzar a 
  realizar el análisis.
* Nombre de objetos en órdenes: Filtro de los nombres de los objetos en órdenes.
  Es recomendable no dejarlo vacío, ya que sino el análisis se extenderá a una 
  gran cantidad de objetos estándares, y causara una gran demora en la ejecución
  del programa.
* Mostrar solo órdenes: Si se encuentra tildado indica que en el arbol que se 
  obtenga como resultado del análisis, se muestren solo las órdenes de transporte, 
  es decir, no se mostrarán objetos como programas, tablas, funciones, etc.
* Mostrar solo ord. NO trans. a: Si se está tildado indica que solo se deben 
  mostrar las órdenes de transporte que NO se encuentren en el destino 
  especificado por el parámetro "Destino RFC". Tener en cuenta que al estar 
  tildado solo se mostrarán ordenes de transporte en el arbol obtenido como 
  resultado.
* Destino RFC: Destino en el cual se verifica si una órden de transporte existe 
  o NO existe en caso de estar tildado el parámetro "Mostrar solo ord. NO trans".

3) Entendiendo cómo se obtienen las dependencias para cada objeto
-----------------------------------------------------------------

Ahora, valdría la pena preguntarse, ¿cómo se obtienen las dependencias de cada 
objeto?
Bueno, justamente esa es la parte interesante de este programa. Lo que ofrece el
mismo es poder registrar para cada tipo de objeto una clase que se encargue de 
obtener sus dependencias.

Vamos a ver un ejemplo puntual, el del tipo de objeto Tabla.
Para empezar veamos la definición de la clase que se va a encargar de obtener 
las dependencias de las tablas:

    CLASS LCL_TABLA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.
    
      PUBLIC SECTION.
    
        METHODS:
    
          SELECT_DEPENDENCIAS REDEFINITION,
    
          GET_LINEA_TREE REDEFINITION.
    
    ENDCLASS.                    "LCL_TABLA DEFINITION

Analicemos un poco el código. Nuestra clase se va a llamar LCL_TABLA y vemos que
hereda de la clas LCL_OBJETO_ORDEN.
Acá tenemos el primer punto clave, toda clase que quiera obtener las dependencias
para un objeto dado debe extender LCL_OBJETO_ORDEN o algúna sub clase de 
LCL_OBJETO_ORDEN.
** IMPORTANTE: Si redefine el constructor no cambiar la firma!!! La firma debe 
ser la misma que la definida en la clase LCL_OBJETO_ORDEN.**

Luego nos encontramos con dos métodos:
* SELECT_DEPENDENCIAS: Este método es el encargado de obtener las dependencias.
  No tiene parámetros.
* GET_LINEA_TREE: Este método es el encargado de devolver los datos a ser 
  mostrados en el ALV tree. Tiene como único parámetro la línea a ser devuelta
  para ser mostrada por el ALV tree.

Veamos para empezar la implementación del método SELECT_DEPENDENCIAS para 
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

Varios puntos a tener en cuenta a la hora de realizar la implementación del 
método SELECT_DEPENDENCIAS:
* Siempre se debería llamar al principio del método al método padre:
  SUPER->SELECT_DEPENDENCIAS( ).
* Como se puede observar, hay una consulta donde se obtienen dominio y elementos 
  de datos de los depende la tabla en análisis, pero, ¿donde está el nombre de
  la tabla analizada? Bueno, el nombre del objeto que se está analizando siempre
  se podrá encontrar en ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
* Bien, una vez obtenidos dominios y elementos de datos, ¿donde indicamos que la
  tabla depende de estos? Eso se indica agregando una o varias entradas en la 
  tabla interna ME->LT_DEPENDENCIAS, pero se debe hacer de la siguiente forma:
  llamando al método LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS que recibirá 
  los siguientes parámetros:
  * IV_CLASS_NAME: Nombre de la clase asociada al objeto que se tiene como 
    dependencia. En nuestro caso las clases son LCL_ELEMENTO_DATOS y LCL_DOMINIO.
    Dichas clases deben estar implementadas y deben extender a la clase 
    LCL_OBJETO_ORDEN o alguna sub clase de LCL_OBJETO_ORDEN.
  * IV_OBJ_NAME: Nombre del objeto. En nuestro caso será el nombre del dominio o
    nombre del elemento de datos.
  * CT_VERIFICABLES: Tabla donde se agregarán las dependencias. Generalmente 
    siempre debería pasarse ME->LT_DEPENDENCIAS.

Y eso básicamente sería lo que debe tenerse en cuenta a la hora de implementar 
SELECT_DEPENDENCIAS. En resumen:
* Llamar a SUPER->SELECT_DEPENDENCIAS( )
* Agregar nuestra lógica para obtener las dependencias del objeto 
  ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
* Por cada dependencia llamar al método 
  LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS pasandole el nombre de la clase
  asociada con el tipo de objeto de la dependencia, el nombre de la dependencia 
  y la tabla interna donde se agregarán las dependencias (ME->LT_DEPENDENCIAS).

Ahora veamos la implementación del método GET_LINEA_TREE para nuestra clase 
LCL_TABLA:

    METHOD GET_LINEA_TREE.
    
      LS_RESULT-TIPO = 'Tabla'.
  
    ENDMETHOD.

Bueno, como se ve es bastante simple. Lo único que se hace es indicar una 
descripción para el tipo de objeto.

4) Registrando nuestra clase
----------------------------

Está claro que está faltando algo, ¿cómo puede ser que la aplicación sepa que 
tiene que instanciar nuestra clase LCL_TABLA cuando encuentra una tabla en una 
orden de transporte?

Bueno, esto justamente se logra registrando nuestra clase para el tipo de objeto
que analizará nuestra clase.
Es decir, lo que tenemos que hacer es indicar que la clase LCL_TABLA analizará 
los siguientes tipos de objetos obtenidos de las órdenes de transporte:
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

Tener en cuenta que el arbol de dependencias mostrado está furtemente ligado a
la implementación de la obtención de dependencias para cada tipo de objeto, 
por tanto la exactitud de los resultados obtenidos dependerá de la exactitud
de la lógica utilizada para la obtención de dependencias para cada tipo de 
objeto.

