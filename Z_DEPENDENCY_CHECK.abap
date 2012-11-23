*&---------------------------------------------------------------------*
*& Report  Z_DEPENDENCY_CHECK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

* @todo Detectar referencias cruzadas: Básicamente verificar si un
* verificable ya ha sido verificado.

* Ejemplos de uso
* ---------------
*
* Eejemplo 1
* ----------
* Entrada:
* Se ingresa como parámetro la orden de transporte O1.
* Se piden solo objetos Z.
* Se piden órdenes de transporte que no hayan sido importadas al
* ambiente A1.
*
* Salida: Se muestra arbol de dependencias, donde la raiz del arbol es
* la orden de transporte O1, para dicha orden se obtienen los objetos
* dependientes (hijos de nivel 1), para cada objeto se obtienen sus
* ordenes y objetos dependientes (hijos nivel 2), y se sigue en forma
* recursiva para cada uno hasta que no se presenten dependencias.
* En todos los casos se restringirá para que sean recuperados objetos
* Z.
* En todos los casos se restringirá para que sean recuperadas órdenes
* de transporte que no hayan sido transportadas al ambiente A1.

REPORT  Z_DEPENDENCY_CHECK.

TYPE-POOLS:
  ABAP.

***********************************************************************
* Definición de clases
***********************************************************************

CLASS LCL_VERIFICABLE DEFINITION DEFERRED.

***********************************************************************
* Clase contenedor de los parámetros de selección.
***********************************************************************
CLASS LCL_FILTROS DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      LR_OBJ_NAME TYPE RANGE OF E071-OBJ_NAME.

ENDCLASS.                    "LCL_FILTROS DEFINITION

***********************************************************************
* Clase encargada de mostrar las dependencias en un arbol.
***********************************************************************
CLASS lcl_alv_tree DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TY_S_LINEA_TREE,
        TIPO TYPE STRING,
        NOMBRE TYPE STRING,
      END OF TY_S_LINEA_TREE.


    CLASS-METHODS:

      ADD_VERIFICABLE
        IMPORTING
          IR_nodes type ref to cl_salv_nodes
          IV_CLAVE_NODO_PADRE type salv_de_node_key
          IR_VERIFICABLE TYPE REF TO LCL_VERIFICABLE,

      mostrar_dependencias
        IMPORTING
          IV_VERIFICABLE TYPE REF TO LCL_VERIFICABLE.

ENDCLASS.                    "lcl_alv_tree DEFINITION

***********************************************************************
* Clase abstracta de la cual heredaran todos los objetos a ser
* verificados.
* Patron Composite utilizado para modelar arbol de dependencias.
***********************************************************************
CLASS LCL_VERIFICABLE DEFINITION ABSTRACT FRIENDS LCL_ALV_TREE.

  PUBLIC SECTION.

    TYPES:
      TY_R_VERIFICABLE TYPE REF TO LCL_VERIFICABLE,
      TY_T_VERIFICABLES TYPE STANDARD TABLE OF TY_R_VERIFICABLE,
      TY_V_KEY(150) TYPE C.

    METHODS:

      VERIFICAR,

      VERIFICADO
        RETURNING VALUE(EV_RESULT) TYPE ABAP_BOOL,

      SELECT_DEPENDENCIAS ABSTRACT,

      GET_KEY
        RETURNING
          VALUE(EV_RESULT) TYPE LCL_VERIFICABLE=>TY_V_KEY,

      GET_LINEA_TREE ABSTRACT
        RETURNING
          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.


  PROTECTED SECTION.

    DATA:
          LT_DEPENDENCIAS TYPE TY_T_VERIFICABLES.

ENDCLASS.                    "LCL_VERIFICABLE DEFINITION

***********************************************************************
* Clase que modela una orden de transporte.
***********************************************************************
CLASS LCL_ORDEN_TRANSPORTE DEFINITION INHERITING FROM LCL_VERIFICABLE.

  PUBLIC SECTION.

    METHODS:

      CONSTRUCTOR
        IMPORTING IV_NUMERO_ORDEN TYPE E070-TRKORR,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_KEY REDEFINITION,

      GET_LINEA_TREE REDEFINITION.


  PROTECTED SECTION.

    DATA:
          LV_NUMERO_ORDEN TYPE E070-TRKORR.

ENDCLASS.                    "LCL_ORDEN_TRANSPORTE DEFINITION

***********************************************************************
* Clase que modela los objetos que pueden estar en una orden de
* transporte.
***********************************************************************
***********************************************************************
* @doc:
* Esta clase debe ser heredada en caso de querer agregar un nuevo tipo
* de objeto a ser verificado por este programa.
* Es tan simple como heredar de la clase LCL_OBJETO_ORDEN y redefinir
* el método SELECT_DEPENDENCIAS. En la redefinición de dicho método
* se debería llamar a super->SELECT_DEPENDENCIAS( ) y luego agregar al
* atributo me->LT_DEPENDENCIAS las dependencias que tenga el nuevo
* tipo de objeto. Recuerde que para identificar el objeto que está
* tratando cuenta con el atributo
* me->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
* Un ejemplo de clase heredera es la clase LCL_PROGRAMA que tal cual
* está aqui documentado redefine simplemente el método
* SELECT_DEPENDENCIAS.
* IMPORTANTE: Para que su clase sea instanciada y utilizada para
* analizar las dependencias de un objeto, debe registrar su
* instanciación en el método LCL_OBJETO_ORDEN=>FACTORY. En dicho
* método se explica y ejemplifica como realizar el registro.
***********************************************************************
CLASS LCL_OBJETO_ORDEN DEFINITION INHERITING FROM LCL_VERIFICABLE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TY_S_IDENTIFICACION_OBJETO,
        PGMID TYPE E071-PGMID,
        OBJECT TYPE E071-OBJECT,
        OBJ_NAME TYPE E071-OBJ_NAME,
      END OF TY_S_IDENTIFICACION_OBJETO,

      TY_T_IDENTIFICACION_OBJETOS TYPE STANDARD TABLE OF
        TY_S_IDENTIFICACION_OBJETO.

    METHODS:

      CONSTRUCTOR
        IMPORTING IS_IDENTIFICACION_OBJETO TYPE
          TY_S_IDENTIFICACION_OBJETO,

      GET_KEY REDEFINITION,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

    CLASS-METHODS:

      FACTORY
        IMPORTING IS_IDENTIFICACION_OBJETO TYPE
          TY_S_IDENTIFICACION_OBJETO
        RETURNING
          VALUE(EV_RESULT) TYPE REF TO LCL_OBJETO_ORDEN,

      CREAR_Y_AGREGAR_OBJETOS_TABLA
        IMPORTING
          IV_OBJ_NAME TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

      CREAR_Y_AGREGAR_OBJ_ELEM_DATOS
        IMPORTING
          IV_OBJ_NAME TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

      CREAR_Y_AGREGAR_OBJETO_DOMINIO
        IMPORTING
          IV_OBJ_NAME TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES.

  PROTECTED SECTION.

    DATA:
          LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

ENDCLASS.                    "LCL_OBJETO_ORDEN DEFINITION

***********************************************************************
* Clase que modela un programa ABAP.
***********************************************************************
CLASS LCL_PROGRAMA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.                    "LCL_PROGRAMA DEFINITION

***********************************************************************
* Clase que modela una tabla de diccionario.
***********************************************************************
CLASS LCL_TABLA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.                    "LCL_TABLA DEFINITION

***********************************************************************
* Clase utilizada para llevar un control de las dependencias que ya
* fueron verificadas, sobre todo para evitar ciclos infinitos por
* referencias cruzadas y redundancia en la respuesta.
***********************************************************************
CLASS LCL_MANEJADOR_DEPENDENCIAS DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      CLASS_CONSTRUCTOR,

      GET_INSTANCE
        RETURNING
          VALUE(EV_RESULT) TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.

    METHODS:

      VERIFICADO
        IMPORTING
          IV_VERIFICABLE TYPE REF TO LCL_VERIFICABLE
        RETURNING
          VALUE(EV_RETURN) TYPE ABAP_BOOL.

  PROTECTED SECTION.

    DATA:
          LT_VERIFICADOS TYPE SORTED TABLE OF LCL_VERIFICABLE=>TY_V_KEY WITH UNIQUE KEY TABLE_LINE.

  PRIVATE SECTION.

    CLASS-DATA:
      LV_INSTANCE TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.

ENDCLASS.                    "LCL_MANEJADOR_DEPENDENCIAS DEFINITION

***********************************************************************
* Implementación de clases
***********************************************************************

CLASS LCL_MANEJADOR_DEPENDENCIAS IMPLEMENTATION.

  METHOD CLASS_CONSTRUCTOR.
    CREATE OBJECT LCL_MANEJADOR_DEPENDENCIAS=>LV_INSTANCE.
  ENDMETHOD.                    "CLASS_CONSTRUCTOR

  METHOD GET_INSTANCE.
*    RETURNING
*      VALUE(EV_RESULT) TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.
    EV_RESULT = LCL_MANEJADOR_DEPENDENCIAS=>LV_INSTANCE.
  ENDMETHOD.                    "GET_INSTANCE

  METHOD VERIFICADO.
*    IMPORTING
*      IV_VERIFICABLE TYPE REF TO LCL_VERIFICABLE.
*    RETURNING
*      VALUE(EV_RETURN) TYPE ABAP_BOOL.

    DATA:
          LV_KEY TYPE LCL_VERIFICABLE=>TY_V_KEY.

    " Se obtiene la clave para el verificable pasado.
    LV_KEY = IV_VERIFICABLE->GET_KEY( ).

    " Se busca dicha clave en los objetos verificados.
    READ TABLE ME->LT_VERIFICADOS
    WITH KEY TABLE_LINE = LV_KEY
    TRANSPORTING NO FIELDS.

    " En caso de encontrarla...
    IF SY-SUBRC EQ 0.
      " ...el objeto ya se encuentra verificado.
      EV_RETURN = ABAP_TRUE.
    ELSE.
      " ...sino NO se encuentra verificado
      EV_RETURN = ABAP_FALSE.
      " ...y se lo agrega a los objetos verificados para que la próxima
      " vez aparezca como verificado.
      INSERT LV_KEY INTO TABLE ME->LT_VERIFICADOS.
      OPEN DATASET '/tmp/Z_DEPENDENCY_CHECK' FOR APPENDING ENCODING DEFAULT IN TEXT MODE.
      TRANSFER LV_KEY to '/tmp/Z_DEPENDENCY_CHECK'.
      CLOSE DATASET '/tmp/Z_DEPENDENCY_CHECK'.
    ENDIF.

  ENDMETHOD.                    "VERIFICADO

ENDCLASS.                    "LCL_MANEJADOR_DEPENDENCIAS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_tree IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_tree IMPLEMENTATION.

  METHOD ADD_VERIFICABLE.
*        IMPORTING
*          IR_nodes type ref to cl_salv_nodes
*          IV_CLAVE_NODO_PADRE type salv_de_node_key
*          IR_VERIFICABLE TYPE REF TO LCL_VERIFICABLE,

    FIELD-SYMBOLS:
      <LR_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      Lr_node TYPE REF TO cl_salv_node,
      LS_LINEA_TREE TYPE TY_S_LINEA_TREE,
      LV_CLAVE type salv_de_node_key.

    LR_node = IR_nodes->add_node(
      related_node = IV_CLAVE_NODO_PADRE
      relationship = cl_gui_column_tree=>relat_LAST_child
      ).

    LS_LINEA_TREE = IR_VERIFICABLE->GET_LINEA_TREE( ).

    LR_node->set_data_row( LS_LINEA_TREE ).

    LV_CLAVE = LR_node->get_key( ).

    LOOP AT IR_VERIFICABLE->LT_DEPENDENCIAS ASSIGNING <LR_VERIFICABLE>.

      ADD_VERIFICABLE(
        EXPORTING
          IR_nodes = IR_nodes
          IV_CLAVE_NODO_PADRE = LV_CLAVE
          IR_VERIFICABLE = <LR_VERIFICABLE>
        ).

    ENDLOOP.

  ENDMETHOD.

  METHOD mostrar_dependencias.
*        IMPORTING
*          IV_VERIFICABLE TYPE REF TO LCL_VERIFICABLE.

    data:
      Lr_tree type ref to cl_salv_tree,
      LT_SALIDA TYPE STANDARD TABLE OF TY_S_LINEA_TREE,
      LR_nodes type ref to cl_salv_nodes,
      LV_key type salv_de_node_key..

    cl_salv_tree=>factory(
      IMPORTING
        R_SALV_TREE = Lr_tree
      changing
        t_table      = LT_SALIDA
      ).

    LR_nodes = Lr_tree->get_nodes( ).

    ADD_VERIFICABLE(
      EXPORTING
        IR_nodes = LR_nodes
        IV_CLAVE_NODO_PADRE = LV_key
        IR_VERIFICABLE = IV_VERIFICABLE
      ).

    DATA:
      LR_columns type ref to cl_salv_columns.

    LR_columns = Lr_tree->get_columns( ).
    LR_columns->set_optimize( abap_true ).

    data:
      lR_funciones type ref to CL_SALV_FUNCTIONS_TREE.

*   Se setean todas las funciones del ALV
    lR_funciones = Lr_tree->get_functions( ).
    lR_funciones->set_all( ).

    Lr_tree->display( ).

  ENDMETHOD.                    "mostrar_dependencias

ENDCLASS.                    "lcl_alv_tree IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_VERIFICABLE IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_VERIFICABLE IMPLEMENTATION.

  METHOD VERIFICAR.

    FIELD-SYMBOLS:
                   <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE.

    CHECK ME->VERIFICADO( ) EQ ABAP_FALSE.

    ME->SELECT_DEPENDENCIAS( ).

    LOOP AT ME->LT_DEPENDENCIAS ASSIGNING <LV_DEPENDENCIA>.

      <LV_DEPENDENCIA>->VERIFICAR( ).

    ENDLOOP.

  ENDMETHOD.                    "VERIFICAR

  METHOD VERIFICADO.

    DATA:
          LV_MR TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.

    LV_MR = LCL_MANEJADOR_DEPENDENCIAS=>GET_INSTANCE( ).

    EV_RESULT = LV_MR->VERIFICADO( ME ).

  ENDMETHOD.                    "VERIFICADO

  METHOD GET_KEY.

    DATA:
          LV_CLASS_NAME TYPE ABAP_ABSTYPENAME.

    " Se obtiene el nombre de la clase del objeto llamador.
    " @todo verificar.
    LV_CLASS_NAME = cl_abap_classdescr=>get_class_name( ME ).

    EV_RESULT = LV_CLASS_NAME.

  ENDMETHOD.                    "GET_KEY

ENDCLASS.                    "LCL_VERIFICABLE IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ORDEN_TRANSPORTE IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ORDEN_TRANSPORTE IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*    IMPORTING IV_NUMERO_ORDEN TYPE E070-TRKORR,

    SUPER->CONSTRUCTOR( ).

    ME->LV_NUMERO_ORDEN = IV_NUMERO_ORDEN.

  ENDMETHOD.                    "CONSTRUCTOR

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_ORDEN> TYPE E070-TRKORR,
      <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE,
      <LS_OBJETO_ORDEN> TYPE
        LCL_OBJETO_ORDEN=>TY_S_IDENTIFICACION_OBJETO.

    DATA:
      LV_DEPENDENCIA TYPE REF TO LCL_VERIFICABLE,
      LT_ORDENES_HIJO TYPE STANDARD TABLE OF E070-TRKORR,
      LT_OBJETOS_ORDEN TYPE
        LCL_OBJETO_ORDEN=>TY_T_IDENTIFICACION_OBJETOS.

    " Se obtienen las ordenes hijo
    SELECT TRKORR
    FROM E070
    INTO TABLE LT_ORDENES_HIJO
    WHERE STRKORR EQ ME->LV_NUMERO_ORDEN.

    " En caso de tener alguna orden hijo se agrega a las
    " dependencias...
    LOOP AT LT_ORDENES_HIJO ASSIGNING <LV_ORDEN>.

      APPEND INITIAL LINE TO ME->LT_DEPENDENCIAS ASSIGNING
        <LV_DEPENDENCIA>.

      CREATE OBJECT <LV_DEPENDENCIA>
        TYPE
          LCL_ORDEN_TRANSPORTE
        EXPORTING
          IV_NUMERO_ORDEN      = <LV_ORDEN>.

    ENDLOOP.

    " Se obtienen los objetos dentro de la orden
    SELECT PGMID OBJECT OBJ_NAME
    FROM E071
    INTO TABLE LT_OBJETOS_ORDEN
    WHERE
      TRKORR EQ ME->LV_NUMERO_ORDEN AND
      OBJ_NAME IN LCL_FILTROS=>LR_OBJ_NAME.

    LOOP AT LT_OBJETOS_ORDEN ASSIGNING <LS_OBJETO_ORDEN>.

      LV_DEPENDENCIA = LCL_OBJETO_ORDEN=>FACTORY(
        <LS_OBJETO_ORDEN>
        ).

      IF LV_DEPENDENCIA IS NOT INITIAL.

        APPEND LV_DEPENDENCIA TO ME->LT_DEPENDENCIAS.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_KEY.

    EV_RESULT = SUPER->GET_KEY( ).

    CONCATENATE
      EV_RESULT
      '-'
      ME->LV_NUMERO_ORDEN
    INTO EV_RESULT.

  ENDMETHOD.                    "GET_KEY

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Orden de transporte'.
    LS_RESULT-NOMBRE = LV_NUMERO_ORDEN.
  ENDMETHOD.

ENDCLASS.                    "LCL_ORDEN_TRANSPORTE IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_OBJETO_ORDEN IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_OBJETO_ORDEN IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*    IMPORTING IS_IDENTIFICACION_OBJETO TYPE
*      TY_S_IDENTIFICACION_OBJETO,

    SUPER->CONSTRUCTOR( ).

    ME->LS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD GET_KEY.

    EV_RESULT = SUPER->GET_KEY( ).

    CONCATENATE
      EV_RESULT
      LS_IDENTIFICACION_OBJETO-PGMID
      LS_IDENTIFICACION_OBJETO-OBJECT
      LS_IDENTIFICACION_OBJETO-OBJ_NAME
    INTO
      EV_RESULT
    SEPARATED BY
      '-'.

  ENDMETHOD.                    "GET_KEY

  METHOD FACTORY.
*    IMPORTING IS_IDENTIFICACION_OBJETO TYPE
*      TY_S_IDENTIFICACION_OBJETO
*    RETURNING
*      VALUE(EV_RESULT) TYPE REF TO LCL_OBJETO_ORDEN.

***********************************************************************
* @doc:
* Cada TIPO de objeto en una orden de transporte se identifica
* por 2 campos:
* 1) IS_IDENTIFICACION_OBJETO-PGMID: Ejemplo de valores R3TR, LIMU,
*    etc.
* 2) IS_IDENTIFICACION_OBJETO-OBJECT: Ejemplos de valores PROG, TABL,
*    etc.
* Para agregar un nuevo tipo de objeto a ser verificado por este
* programa, se debe instanciar en este método la clase que modela dicho
* tipo en función de los campos IS_IDENTIFICACION_OBJETO-PGMID,
* IS_IDENTIFICACION_OBJETO-OBJECT.
* La clase de la cual se desea crear una instancia debe ser heredera
* de la clase LCL_OBJETO_ORDEN.
* Por ejemplo, si IS_IDENTIFICACION_OBJETO-PGMID eq 'R3T3' y
* IS_IDENTIFICACION_OBJETO-OBJECT eq 'PROG', entonces se instancia la
* clase LCL_PROGRAMA
***********************************************************************

    CLEAR EV_RESULT.

    CASE IS_IDENTIFICACION_OBJETO-PGMID.

      WHEN 'R3TR'.

        CASE IS_IDENTIFICACION_OBJETO-OBJECT.

          WHEN 'PROG'.
            CREATE OBJECT EV_RESULT
              TYPE
                LCL_PROGRAMA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN 'TABL'.
            CREATE OBJECT EV_RESULT
              TYPE
                LCL_TABLA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN 'DTEL' "Elemento de datos
            OR 'DOMA'. "Dominio

            CREATE OBJECT EV_RESULT
              TYPE
                LCL_OBJETO_ORDEN
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN OTHERS.

            RETURN.

        ENDCASE.

      WHEN 'LIMU'.

        CASE IS_IDENTIFICACION_OBJETO-OBJECT.

          WHEN 'REPS'.
            CREATE OBJECT EV_RESULT
              TYPE
                LCL_PROGRAMA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN 'TABT' OR 'TABD'.
            CREATE OBJECT EV_RESULT
              TYPE
                LCL_TABLA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN OTHERS.

            RETURN.

        ENDCASE.

      WHEN OTHERS.

        RETURN.

    ENDCASE.

  ENDMETHOD.                    "FACTORY

  METHOD CREAR_Y_AGREGAR_OBJETOS_TABLA.
*    IMPORTING
*      IV_OBJ_NAME TYPE CSEQUENCE
*    CHANGING
*      CT_VERIFICABLES TYPE LCL_VERIFICABLES=>TY_T_VERIFICABLES.

    FIELD-SYMBOLS:
      <LV_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

    LS_IDENTIFICACION_OBJETO-OBJ_NAME = IV_OBJ_NAME.

    LS_IDENTIFICACION_OBJETO-PGMID = 'R3TR'.
    LS_IDENTIFICACION_OBJETO-OBJECT = 'TABL'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( LS_IDENTIFICACION_OBJETO ).

    LS_IDENTIFICACION_OBJETO-PGMID = 'LIMU'.
    LS_IDENTIFICACION_OBJETO-OBJECT = 'TABT'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( LS_IDENTIFICACION_OBJETO ).
    .
    LS_IDENTIFICACION_OBJETO-OBJECT = 'TABD'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( LS_IDENTIFICACION_OBJETO ).

  ENDMETHOD.                    "CREAR_Y_AGREGAR_OBJETOS_TABLA

  METHOD CREAR_Y_AGREGAR_OBJ_ELEM_DATOS.
*          IMPORTING
*            IV_OBJ_NAME TYPE CSEQUENCE
*          CHANGING
*            CT_VERIFICABLES TYPE LCL_VERIFICABLES=>TY_T_VERIFICABLES,
    FIELD-SYMBOLS:
      <LV_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

    LS_IDENTIFICACION_OBJETO-OBJ_NAME = IV_OBJ_NAME.

    LS_IDENTIFICACION_OBJETO-PGMID = 'R3TR'.
    LS_IDENTIFICACION_OBJETO-OBJECT = 'DTEL'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( LS_IDENTIFICACION_OBJETO ).

  ENDMETHOD.                    "CREAR_Y_AGREGAR_OBJ_ELEM_DATOS

  METHOD CREAR_Y_AGREGAR_OBJETO_DOMINIO.
*          IMPORTING
*            IV_OBJ_NAME TYPE CSEQUENCE
*          CHANGING
*            CT_VERIFICABLES TYPE LCL_VERIFICABLES=>TY_T_VERIFICABLES.
    FIELD-SYMBOLS:
      <LV_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

    LS_IDENTIFICACION_OBJETO-OBJ_NAME = IV_OBJ_NAME.

    LS_IDENTIFICACION_OBJETO-PGMID = 'R3TR'.
    LS_IDENTIFICACION_OBJETO-OBJECT = 'DOMA'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( LS_IDENTIFICACION_OBJETO ).

  ENDMETHOD.                    "CREAR_Y_AGREGAR_OBJETO_DOMINIO

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_ORDEN> TYPE E070-TRKORR,
      <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LT_ORDENES TYPE STANDARD TABLE OF E070-TRKORR.

    " Se obtienen las ordenes de transporte en las que está incluido el
    " objeto
    SELECT TRKORR
    FROM E071
    INTO TABLE LT_ORDENES
    WHERE
      PGMID EQ ME->LS_IDENTIFICACION_OBJETO-PGMID AND
      OBJECT EQ ME->LS_IDENTIFICACION_OBJETO-OBJECT AND
      OBJ_NAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.

    IF SY-SUBRC NE 0.
      " @todo: Generar excepción al encontrar que un objeto no está
      " incluido en ninguna orden.
    ENDIF.

    " En caso de tener alguna orden hijo se agrega a las
    " dependencias...
    LOOP AT LT_ORDENES ASSIGNING <LV_ORDEN>.

      APPEND INITIAL LINE TO ME->LT_DEPENDENCIAS ASSIGNING
        <LV_DEPENDENCIA>.

      CREATE OBJECT <LV_DEPENDENCIA>
        TYPE
          LCL_ORDEN_TRANSPORTE
        EXPORTING
          IV_NUMERO_ORDEN      = <LV_ORDEN>.

    ENDLOOP.

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Objeto orden'.
    LS_RESULT-NOMBRE = LS_IDENTIFICACION_OBJETO-OBJ_NAME.

  ENDMETHOD.

ENDCLASS.                    "LCL_OBJETO_ORDEN IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_PROGRAMA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_PROGRAMA IMPLEMENTATION.

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_TABNAME> TYPE D010TAB-TABNAME.

    DATA:
      LT_TABNAME TYPE STANDARD TABLE OF D010TAB-TABNAME.

    SUPER->SELECT_DEPENDENCIAS( ).

    SELECT TABNAME
    FROM D010TAB
    INTO TABLE LT_TABNAME
    WHERE
      MASTER EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME AND
* @todo Verificar includes de grupos de funciones
      TABNAME LIKE 'Z%'.

    LOOP AT LT_TABNAME ASSIGNING <LV_TABNAME>.

      LCL_OBJETO_ORDEN=>CREAR_Y_AGREGAR_OBJETOS_TABLA(
        EXPORTING
          IV_OBJ_NAME = <LV_TABNAME>
        CHANGING
          CT_VERIFICABLES = ME->LT_DEPENDENCIAS
      ).

    ENDLOOP.

    " @todo: Agregar como dependencias a los includes:
    " Tabla D010INC: A partir de MASTER se obtiene el include.

    " @todo: Agregar como dependencias el programa padre, en caso
    " de ser un include.

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Programa'.
    LS_RESULT-NOMBRE = LS_IDENTIFICACION_OBJETO-OBJ_NAME.

  ENDMETHOD.

ENDCLASS.                    "LCL_PROGRAMA IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TABLA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TABLA IMPLEMENTATION.

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

    " @todo implementar obtencion de elementos de datos y dominios.

    " Tabla DD03L: A partir de TABNAME se pueden obtener ROLLNAME y DOMNAME
    SELECT ROLLNAME DOMNAME
    FROM DD03L
    INTO TABLE LT_DD03L
    WHERE
      TABNAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME AND
      ( ROLLNAME LIKE 'Z%' OR
      DOMNAME LIKE 'Z%' ).

    LOOP AT LT_DD03L ASSIGNING <LS_DD03L>.

      IF <LS_DD03L>-ROLLNAME IS NOT INITIAL.

        LCL_OBJETO_ORDEN=>CREAR_Y_AGREGAR_OBJ_ELEM_DATOS(
          EXPORTING
            IV_OBJ_NAME = <LS_DD03L>-ROLLNAME
          CHANGING
            CT_VERIFICABLES = ME->LT_DEPENDENCIAS
          ).

      ENDIF.

      IF <LS_DD03L>-DOMNAME IS NOT INITIAL.

        LCL_OBJETO_ORDEN=>CREAR_Y_AGREGAR_OBJETO_DOMINIO(
          EXPORTING
            IV_OBJ_NAME = <LS_DD03L>-ROLLNAME
          CHANGING
            CT_VERIFICABLES = ME->LT_DEPENDENCIAS
          ).

      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Tabla'.
    LS_RESULT-NOMBRE = LS_IDENTIFICACION_OBJETO-OBJ_NAME.

  ENDMETHOD.

ENDCLASS.                    "LCL_TABLA IMPLEMENTATION

* @todo: Agregar tipo de objeto dynpro
" Tabla D020S: A partir de PROG se obtienen todos los dynpros (DNUM).

TABLES:
  E071.

PARAMETERS:
  P_TRKORR TYPE E070-TRKORR.
SELECT-OPTIONS:
  S_OBJNAM FOR E071-OBJ_NAME.

DATA:
      LV_ORDEN_TRANSPORTE TYPE REF TO LCL_ORDEN_TRANSPORTE.

START-OF-SELECTION.

  DELETE DATASET '/tmp/Z_DEPENDENCY_CHECK'.

  LCL_FILTROS=>LR_OBJ_NAME = S_OBJNAM[].

  CREATE OBJECT LV_ORDEN_TRANSPORTE
    EXPORTING
      IV_NUMERO_ORDEN = P_TRKORR.

  LV_ORDEN_TRANSPORTE->VERIFICAR( ).

END-OF-SELECTION.

  lcl_alv_tree=>mostrar_dependencias( LV_ORDEN_TRANSPORTE ).
