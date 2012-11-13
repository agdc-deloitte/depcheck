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
* Clase abstracta de la cual heredaran todos los objetos a ser
* verificados.
* Patron Composite utilizado para modelar arbol de dependencias.
***********************************************************************
CLASS LCL_VERIFICABLE DEFINITION ABSTRACT.

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
          VALUE(EV_RESULT) TYPE LCL_VERIFICABLE=>TY_V_KEY.

  PROTECTED SECTION.

    DATA:
          LT_DEPENDENCIAS TYPE TY_T_VERIFICABLES.

ENDCLASS.

***********************************************************************
* Clase que modela una orden de transporte.
***********************************************************************
CLASS LCL_ORDEN_TRANSPORTE DEFINITION INHERITING FROM LCL_VERIFICABLE.

  PUBLIC SECTION.

    METHODS:

      CONSTRUCTOR
        IMPORTING IV_NUMERO_ORDEN TYPE E070-TRKORR,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_KEY REDEFINITION.

  PROTECTED SECTION.

    DATA:
          LV_NUMERO_ORDEN TYPE E070-TRKORR.

ENDCLASS.

***********************************************************************
* Clase que modela los objetos que pueden estar en una orden de
* transporte.
***********************************************************************
CLASS LCL_OBJETO_ORDEN DEFINITION INHERITING FROM LCL_VERIFICABLE
  ABSTRACT.

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

      SELECT_DEPENDENCIAS REDEFINITION.

    CLASS-METHODS:

      FACTORY
        IMPORTING IS_IDENTIFICACION_OBJETO TYPE
          TY_S_IDENTIFICACION_OBJETO
        RETURNING
          VALUE(EV_RESULT) TYPE REF TO LCL_OBJETO_ORDEN,
      
      CREAR_Y_AGREGAR_OBJETOS_TABLA
        IMPORTING
          IV_OBJ_NAME TYPE E071-OBJ_NAME
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLES=>TY_T_VERIFICABLES.

  PROTECTED SECTION.

    DATA:
          LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

ENDCLASS.

***********************************************************************
* Clase que modela un programa ABAP.
***********************************************************************
CLASS LCL_PROGRAMA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION.

ENDCLASS.

***********************************************************************
* Clase que modela una tabla de diccionario.
***********************************************************************
CLASS LCL_TABLA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION.

ENDCLASS.

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

ENDCLASS.

***********************************************************************
* Implementación de clases
***********************************************************************

CLASS LCL_MANEJADOR_DEPENDENCIAS IMPLEMENTATION.

  METHOD CLASS_CONSTRUCTOR.
    CREATE OBJECT LCL_MANEJADOR_DEPENDENCIAS=>LV_INSTANCE.
  ENDMETHOD.

  METHOD GET_INSTANCE.
*    RETURNING
*      VALUE(EV_RESULT) TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.
    EV_RESULT = LCL_MANEJADOR_DEPENDENCIAS=>LV_INSTANCE.
  ENDMETHOD.

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
      INSERT LV_KEY INTO ME->LT_VERIFICADOS.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_VERIFICABLE IMPLEMENTATION.

  METHOD VERIFICAR.

    FIELD-SYMBOLS:
                   <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE.

    CHECK ME->VERIFICADO( ) EQ ABAP_FALSE.

    ME->SELECT_DEPENDENCIAS( ).

    LOOP AT ME->LT_DEPENDENCIAS ASSIGNING <LV_DEPENDENCIA>.

      <LV_DEPENDENCIA>->VERIFICAR( ).

    ENDLOOP.

  ENDMETHOD.

  METHOD VERIFICADO.

    DATA:
          LV_MR TYPE REF TO LCL_MANEJADOR_DEPENDENCIAS.

    LV_MR = LCL_MANEJADOR_DEPENDENCIAS=>GET_INSTANCE( ).

    EV_RESULT = LV_MR->VERIFICADO( ME ).

  ENDMETHOD.

  METHOD GET_KEY.

    DATA:
          LV_CLASS_NAME TYPE ABAP_ABSTYPENAME.

    " Se obtiene el nombre de la clase del objeto llamador.
    " @todo verificar.
    LV_CLASS_NAME = cl_abap_classdescr=>get_class_name( ME ).

    EV_RESULT = LV_CLASS_NAME.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_ORDEN_TRANSPORTE IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*    IMPORTING IV_NUMERO_ORDEN TYPE E070-TRKORR,

    SUPER->CONSTRUCTOR( ).

    ME->LV_NUMERO_ORDEN = IV_NUMERO_ORDEN.

  ENDMETHOD.

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_ORDEN> TYPE E070-TRKORR,
      <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE,
      <LS_OBJETO_ORDEN> TYPE
        LCL_OBJETO_ORDEN=>TY_S_IDENTIFICACION_OBJETO.

    DATA:
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

      CREATE OBJECT <LV_DEPENDENCIA> TYPE LCL_ORDEN_TRANSPORTE
        EXPORTING
          IV_NUMERO_ORDEN = <LV_ORDEN>.

    ENDLOOP.

    " Se obtienen los objetos dentro de la orden
    SELECT PGMID OBJECT OBJ_NAME
    FROM E071
    INTO TABLE LT_OBJETOS_ORDEN
    WHERE TRKORR EQ ME->LV_NUMERO_ORDEN.

    LOOP AT LT_OBJETOS_ORDEN ASSIGNING <LS_OBJETO_ORDEN>.

      APPEND INITIAL LINE TO ME->LT_DEPENDENCIAS ASSIGNING
        <LV_DEPENDENCIA>.

      <LV_DEPENDENCIA> = LCL_OBJETO_ORDEN=>FACTORY(
        <LS_OBJETO_ORDEN>
        ).

    ENDLOOP.

  ENDMETHOD.

  METHOD GET_KEY.

    EV_RESULT = SUPER->GET_KEY( ).

    CONCATENATE
      EV_RESULT
      '-'
      ME->LV_NUMERO_ORDEN
    INTO EV_RESULT.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_OBJETO_ORDEN IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*    IMPORTING IS_IDENTIFICACION_OBJETO TYPE
*      TY_S_IDENTIFICACION_OBJETO,

    SUPER->CONSTRUCTOR( ).

    ME->LS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.
  ENDMETHOD.

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

  ENDMETHOD.

  METHOD FACTORY.
*    IMPORTING IS_IDENTIFICACION_OBJETO TYPE
*      TY_S_IDENTIFICACION_OBJETO
*    RETURNING
*      VALUE(EV_RESULT) TYPE REF TO LCL_OBJETO_ORDEN.

    " @todo Implementar: Crear instancia según la identificación del
    " objeto de la orden.
    CLEAR EV_RESULT.

    CASE IS_IDENTIFICACION_OBJETO-PGMID.

      WHEN 'R3TR'.

        CASE IS_IDENTIFICACION_OBJETO-OBJECT.

          WHEN 'PROG'.
            CREATE OBJECT EV_RESULT TYPE LCL_PROGRAMA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.
            
          WHEN 'TABL'.
            CREATE OBJECT EV_RESULT TYPE LCL_TABLA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN OTHERS.
            RETURN.

        ENDCASE.

      WHEN 'LIMU'.

        CASE IS_IDENTIFICACION_OBJETO-OBJECT.

          WHEN 'REPS'.
            CREATE OBJECT EV_RESULT TYPE LCL_PROGRAMA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN 'TABT' OR 'TABD'.
            CREATE OBJECT EV_RESULT TYPE LCL_TABLA
              EXPORTING
                IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO.

          WHEN OTHERS.
            RETURN.

        ENDCASE.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

  ENDMETHOD.

  METHOD CREAR_Y_AGREGAR_OBJETOS_TABLA.
*    IMPORTING
*      IV_OBJ_NAME TYPE E071-OBJ_NAME
*    CHANGING
*      CT_VERIFICABLES TYPE LCL_VERIFICABLES=>TY_T_VERIFICABLES.
    
    FIELD-SYMBOLS:
      <LV_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.
    
    DATA:
      LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.
    
    LS_IDENTIFICACION_OBJETO-OBJ_NAME = IV_OBJ_NAME.
    
    IS_IDENTIFICACION_OBJETO-PGMID = 'R3TR'.
    IS_IDENTIFICACION_OBJETO-OBJECT = 'TABL'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( IS_IDENTIFICACION_OBJETO ).
    
    IS_IDENTIFICACION_OBJETO-PGMID = 'LIMU'.
    IS_IDENTIFICACION_OBJETO-OBJECT = 'TABT'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( IS_IDENTIFICACION_OBJETO ).
.
    IS_IDENTIFICACION_OBJETO-OBJECT = 'TABD'.
    APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.
    <LV_VERIFICABLE> = FACTORY( IS_IDENTIFICACION_OBJETO ).

  ENDMETHOD.

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

      CREATE OBJECT <LV_DEPENDENCIA> TYPE LCL_ORDEN_TRANSPORTE
        EXPORTING
          IV_NUMERO_ORDEN = <LV_ORDEN>.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

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
    WHERE MASTER EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
      
    LOOP AT LT_TABNAME ASSIGNING <LV_TABNAME>.

      LCL_OBJETO_ORDEN=>CREAR_Y_AGREGAR_OBJETOS_TABLA( 
        EXPORTING
          IV_OBJ_NAME = <LV_TABNAME>
        CHANGING
          CT_VERIFICABLES = ME->LT_DEPENDENCIAS
      ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_TABLA IMPLEMENTATION.

  METHOD SELECT_DEPENDENCIAS.
    
    SUPER->SELECT_DEPENDENCIAS( ).
    
    " @todo implementar obtencion de elementos de datos y dominios.

  ENDMETHOD.

ENDCLASS.
