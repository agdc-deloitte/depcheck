*&---------------------------------------------------------------------*
*& Report  Z_DEPENDENCY_CHECK
*& Autor: Juan Sebastian Goldberg
*&---------------------------------------------------------------------*
*& Documenatción: Ver README en https://github.com/agdc-deloitte/depcheck
*&
*&---------------------------------------------------------------------*

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
      LR_OBJ_NAME TYPE RANGE OF E071-OBJ_NAME,
      LV_OBTENER_TAREAS_ORDENES TYPE ABAP_BOOL,
      LV_LISTAR_SOLO_ORDENES TYPE ABAP_BOOL,
      LV_MOSTRAR_SOLO_ORD_NO_TRANS TYPE ABAP_BOOL,
      LV_DESTINO_RFC TYPE EDI_LOGDES,
      LV_NO_SELECCIONAR_ORDENES TYPE ABAP_BOOL.

ENDCLASS.                    "LCL_FILTROS DEFINITION

***********************************************************************
* Clase encargada de mostrar las dependencias en un arbol.
***********************************************************************
CLASS lcl_alv_tree DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF TY_S_LINEA_TREE,
        TIPO TYPE STRING,
*        NOMBRE TYPE STRING,
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
          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE,

      GET_TEXT ABSTRACT
        RETURNING
          VALUE(EV_RESULT) TYPE LVC_VALUE,

      SELECT_ORDENES.

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

      GET_LINEA_TREE REDEFINITION,

      GET_TEXT REDEFINITION.

    CLASS-METHODS:

      ORDEN_EXISTE_EN_DESTINO
        IMPORTING
          IV_ORDEN TYPE E070-TRKORR
          IV_DESTINO_RFC TYPE EDI_LOGDES
        RETURNING
          VALUE(LV_RESULT) TYPE ABAP_BOOL.

    DATA:
          LV_NUMERO_ORDEN TYPE E070-TRKORR.

ENDCLASS.                    "LCL_ORDEN_TRANSPORTE DEFINITION

***********************************************************************
* Clase que modela los objetos que pueden estar en una orden de
* transporte.
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
        IMPORTING
          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO,

      GET_KEY REDEFINITION,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION,

      GET_TEXT REDEFINITION,

      SELECT_ORDENES_DONDE_ESTOY,

      SELECT_ORDENES REDEFINITION.

    CLASS-METHODS:

      FACTORY_COLLECTION_BY_CLASS
        IMPORTING
          IV_CLASS_NAME TYPE CSEQUENCE
          IV_OBJ_NAME TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

      FACTORY_COLLECTION
        IMPORTING IS_IDENTIFICACION_OBJETO TYPE
          TY_S_IDENTIFICACION_OBJETO
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES.

  PROTECTED SECTION.

    DATA:
          LS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO.

ENDCLASS.                    "LCL_OBJETO_ORDEN DEFINITION

CLASS LCL_MANEJADOR_TIPOS_OBJETO DEFINITION.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF TY_S_CLASE_REGISTRADA,
        PGMID TYPE E071-PGMID,
        OBJECT TYPE E071-OBJECT,
        CLASS_NAME TYPE STRING,
      END OF TY_S_CLASE_REGISTRADA,

      " @todo crear tipo con tres niveles de busqueda de forma de hacer las búsquedas mucho más performantes.
      TY_T_CLASES_REGISTRADAS TYPE STANDARD TABLE OF TY_S_CLASE_REGISTRADA.

    METHODS:

      REGISTRAR
        IMPORTING
          IV_PGMID TYPE E071-PGMID
          IV_OBJECT TYPE E071-OBJECT
          IV_CLASE_ASOCIADA TYPE CSEQUENCE,

      FACTORY_COLLECTION_BY_CLASS
        IMPORTING
          IV_CLASS_NAME TYPE CSEQUENCE
          IV_OBJ_NAME TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

      FACTORY_COLLECTION
        IMPORTING
          IS_IDENTIFICACION_OBJETO TYPE LCL_OBJETO_ORDEN=>TY_S_IDENTIFICACION_OBJETO
        CHANGING
          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES.

    CLASS-METHODS:

      GET_INSTANCE
        RETURNING
          VALUE(RV_RESULT) TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.

  PRIVATE SECTION.

    CLASS-DATA:
      LV_INSTANCE TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.

    DATA:
      LT_CLASES_REGISTRADAS TYPE TY_T_CLASES_REGISTRADAS.

ENDCLASS.

***********************************************************************
* Clase que modela un programa ABAP.
***********************************************************************
CLASS LCL_PROGRAMA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      CONSTRUCTOR
        IMPORTING
          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

  PROTECTED SECTION.

    DATA:
      LV_NOMBRE_PROGRAMA TYPE E071-OBJ_NAME.

ENDCLASS.                    "LCL_PROGRAMA DEFINITION

***********************************************************************
* Clase que modela un grupo de funciones.
***********************************************************************
CLASS LCL_GRUPO_FUNCIONES DEFINITION INHERITING FROM LCL_PROGRAMA.

  PUBLIC SECTION.

    METHODS:

      CONSTRUCTOR
        IMPORTING
          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO,

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.                    "LCL_PROGRAMA DEFINITION

CLASS LCL_FUNCION DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.

CLASS LCL_DOMINIO DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.

CLASS LCL_ELEMENTO_DATOS DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.
***********************************************************************
* Clase que modela una tabla de diccionario.
***********************************************************************
CLASS LCL_TABLA DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    CLASS-METHODS:

      FILTER_BY_MASTER
        IMPORTING
          IV_MASTER TYPE CSEQUENCE
        CHANGING
          CT_VERIFICABLES TYPE TY_T_VERIFICABLES.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.                    "LCL_TABLA DEFINITION

***********************************************************************
* Clase que modela una clase.
***********************************************************************
CLASS LCL_CLASE DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      SELECT_DEPENDENCIAS REDEFINITION,

      GET_LINEA_TREE REDEFINITION.

ENDCLASS.

CLASS LCL_METODO DEFINITION INHERITING FROM LCL_OBJETO_ORDEN.

  PUBLIC SECTION.

    METHODS:

      GET_LINEA_TREE REDEFINITION.

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
    TRANSPORTING NO FIELDS
    BINARY SEARCH.

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
      LV_TEXT TYPE LVC_VALUE,
      LV_CLAVE type salv_de_node_key,
      LV_AGREGAR TYPE ABAP_BOOL,
      LR_ORDEN TYPE REF TO LCL_ORDEN_TRANSPORTE.

    LV_AGREGAR = ABAP_TRUE.

    " En caso que haya que listar solo ordenes...
    IF LCL_FILTROS=>LV_LISTAR_SOLO_ORDENES EQ ABAP_TRUE.

      " En caso de tratarse de una orden...
      IF CL_LCR_UTIL=>INSTANCEOF(
        OBJECT = IR_VERIFICABLE
        CLASS = 'LCL_ORDEN_TRANSPORTE' ) NE ABAP_TRUE.

        LV_AGREGAR = ABAP_FALSE.

      ENDIF.

    ENDIF.

    " Si solo se desean listar las órdenes que no se encuentran en
    " un cierto destino...
    IF LV_AGREGAR EQ ABAP_TRUE AND
      LCL_FILTROS=>LV_MOSTRAR_SOLO_ORD_NO_TRANS EQ ABAP_TRUE .

      " En caso de tratarse de una orden...
      IF CL_LCR_UTIL=>INSTANCEOF(
        OBJECT = IR_VERIFICABLE
        CLASS = 'LCL_ORDEN_TRANSPORTE' ) EQ ABAP_TRUE.

        LR_ORDEN ?= IR_VERIFICABLE.

        " Si la orden ya se encuentra en dicho destino...
        IF LCL_ORDEN_TRANSPORTE=>ORDEN_EXISTE_EN_DESTINO(
          IV_ORDEN = LR_ORDEN->LV_NUMERO_ORDEN
          IV_DESTINO_RFC = LCL_FILTROS=>LV_DESTINO_RFC
          ) NE ABAP_TRUE.

          LV_AGREGAR = ABAP_TRUE.

        ENDIF.

      ENDIF.

    ENDIF.

    IF LV_AGREGAR EQ ABAP_TRUE.

      LR_node = IR_nodes->add_node(
        related_node = IV_CLAVE_NODO_PADRE
        relationship = cl_gui_column_tree=>relat_LAST_child
        ).

      LS_LINEA_TREE = IR_VERIFICABLE->GET_LINEA_TREE( ).

      LR_node->set_data_row( LS_LINEA_TREE ).

      LV_TEXT = IR_VERIFICABLE->GET_TEXT( ).

      LR_node->SET_TEXT( LV_TEXT ).

      LV_CLAVE = LR_node->get_key( ).

    ELSE.

      LV_CLAVE = IV_CLAVE_NODO_PADRE.

    ENDIF.

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

*    CHECK ME->VERIFICADO( ) EQ ABAP_FALSE.

    ME->SELECT_DEPENDENCIAS( ).

    LOOP AT ME->LT_DEPENDENCIAS ASSIGNING <LV_DEPENDENCIA>.

      IF <LV_DEPENDENCIA>->VERIFICADO( ) EQ ABAP_TRUE.

        DELETE ME->LT_DEPENDENCIAS.

      ELSE.

        <LV_DEPENDENCIA>->VERIFICAR( ).

      ENDIF.

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
    LV_CLASS_NAME = cl_abap_classdescr=>get_class_name( ME ).

    EV_RESULT = LV_CLASS_NAME.

  ENDMETHOD.                    "GET_KEY

**********************************************************************
* Se obtienen las órdenes de transporte para cada verificable del
* arbol.
**********************************************************************
  METHOD SELECT_ORDENES.

    FIELD-SYMBOLS:
      <LR_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    LOOP AT ME->LT_DEPENDENCIAS ASSIGNING <LR_VERIFICABLE>.

      <LR_VERIFICABLE>->SELECT_ORDENES( ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.                    "LCL_VERIFICABLE IMPLEMENTATION

CLASS LCL_MANEJADOR_TIPOS_OBJETO IMPLEMENTATION.

  METHOD REGISTRAR.
*        IMPORTING
*          IV_PGMID TYPE E071-PGMID
*          IV_OBJECT TYPE E071-OBJECT
*          IV_CLASE_ASOCIADA TYPE CSEQUENCE,
    FIELD-SYMBOLS:
      <LS_CLASE_REGISTRADA> TYPE TY_S_CLASE_REGISTRADA.

    APPEND INITIAL LINE TO ME->LT_CLASES_REGISTRADAS ASSIGNING
      <LS_CLASE_REGISTRADA>.

    <LS_CLASE_REGISTRADA>-PGMID = IV_PGMID.
    <LS_CLASE_REGISTRADA>-OBJECT = IV_OBJECT.
    <LS_CLASE_REGISTRADA>-CLASS_NAME = IV_CLASE_ASOCIADA.

  ENDMETHOD.

  METHOD FACTORY_COLLECTION_BY_CLASS.
*        IMPORTING
*          IV_CLASS_NAME TYPE CSEQUENCE
*          IV_OBJ_NAME TYPE CSEQUENCE
*        CHANGING
*          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES.

    FIELD-SYMBOLS:
      <LS_CLASE_REGISTRADA> TYPE TY_S_CLASE_REGISTRADA,
      <LV_VERIFICABLE> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LS_IDENTIFICACION_OBJETO TYPE LCL_OBJETO_ORDEN=>TY_S_IDENTIFICACION_OBJETO,
      LV_CLASS_NAME TYPE TY_S_CLASE_REGISTRADA-CLASS_NAME.

    LS_IDENTIFICACION_OBJETO-OBJ_NAME = IV_OBJ_NAME.

    " @todo remplazar por búsqueda en tablas anidadas de 3 niveles.
    LOOP AT ME->LT_CLASES_REGISTRADAS ASSIGNING <LS_CLASE_REGISTRADA>
      WHERE CLASS_NAME EQ IV_CLASS_NAME.

      LS_IDENTIFICACION_OBJETO-PGMID = <LS_CLASE_REGISTRADA>-PGMID.
      LS_IDENTIFICACION_OBJETO-OBJECT = <LS_CLASE_REGISTRADA>-OBJECT.

      APPEND INITIAL LINE TO CT_VERIFICABLES ASSIGNING <LV_VERIFICABLE>.

      CREATE OBJECT <LV_VERIFICABLE>
        TYPE
          (<LS_CLASE_REGISTRADA>-CLASS_NAME)
        EXPORTING
          IS_IDENTIFICACION_OBJETO = LS_IDENTIFICACION_OBJETO.

    ENDLOOP.

  ENDMETHOD.

  METHOD FACTORY_COLLECTION.
*        IMPORTING
*          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO
*        CHANGING
*          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES.

    FIELD-SYMBOLS:
      <LS_CLASE_REGISTRADA> TYPE TY_S_CLASE_REGISTRADA.

    " @todo Hacer búsqueda binaria
    READ TABLE ME->LT_CLASES_REGISTRADAS
    WITH KEY
      PGMID = IS_IDENTIFICACION_OBJETO-PGMID
      OBJECT = IS_IDENTIFICACION_OBJETO-OBJECT
    ASSIGNING <LS_CLASE_REGISTRADA>.

    CHECK SY-SUBRC EQ 0.

    ME->FACTORY_COLLECTION_BY_CLASS(
      EXPORTING
        IV_CLASS_NAME = <LS_CLASE_REGISTRADA>-CLASS_NAME
        IV_OBJ_NAME = IS_IDENTIFICACION_OBJETO-OBJ_NAME
      CHANGING
        CT_VERIFICABLES = CT_VERIFICABLES
      ).

  ENDMETHOD.

  METHOD GET_INSTANCE.
*        RETURNING
*          VALUE(RV_RESULT) TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.
    IF LCL_MANEJADOR_TIPOS_OBJETO=>LV_INSTANCE IS INITIAL.
      CREATE OBJECT LCL_MANEJADOR_TIPOS_OBJETO=>LV_INSTANCE.
    ENDIF.

    RV_RESULT = LV_INSTANCE.

  ENDMETHOD.

ENDCLASS.

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

    IF LCL_FILTROS=>LV_OBTENER_TAREAS_ORDENES EQ ABAP_TRUE.

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

    ENDIF.

    " Se obtienen los objetos dentro de la orden
    SELECT PGMID OBJECT OBJ_NAME
    FROM E071
    INTO TABLE LT_OBJETOS_ORDEN
    WHERE
      TRKORR EQ ME->LV_NUMERO_ORDEN AND
      OBJ_NAME IN LCL_FILTROS=>LR_OBJ_NAME.

    " Se obtienen los objetos dentro de las tareas de la orden.
    SELECT PGMID OBJECT OBJ_NAME
    FROM E071 INNER JOIN E070
    ON E071~TRKORR EQ E070~TRKORR
    APPENDING TABLE LT_OBJETOS_ORDEN
    WHERE
      STRKORR EQ ME->LV_NUMERO_ORDEN AND
      OBJ_NAME IN LCL_FILTROS=>LR_OBJ_NAME.

    SORT LT_OBJETOS_ORDEN.
    DELETE ADJACENT DUPLICATES FROM LT_OBJETOS_ORDEN.

    LOOP AT LT_OBJETOS_ORDEN ASSIGNING <LS_OBJETO_ORDEN>.

      LCL_OBJETO_ORDEN=>FACTORY_COLLECTION(
        EXPORTING
          IS_IDENTIFICACION_OBJETO = <LS_OBJETO_ORDEN>
        CHANGING
          CT_VERIFICABLES = LT_DEPENDENCIAS
        ).

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
  ENDMETHOD.

  METHOD GET_TEXT.
*        RETURNING
*          VALUE(EV_RESULT) TYPE LVC_VALUE.

    EV_RESULT = LV_NUMERO_ORDEN.
  ENDMETHOD.

  METHOD ORDEN_EXISTE_EN_DESTINO.
*    IMPORTING
*      IV_ORDEN TYPE E070-TRKORR
*      IV_DESTINO_RFC TYPE EDI_LOGDES
*    RETURNING
*      VALUE(LV_RESULT) TYPE ABAP_BOOL.

    STATICS:
      LT_EXISTENTES_EN_DESTINO TYPE SORTED TABLE OF E070-TRKORR WITH UNIQUE KEY TABLE_LINE,
      LT_INEXISTENTES_EN_DESTINO TYPE SORTED TABLE OF E070-TRKORR WITH UNIQUE KEY TABLE_LINE.

    READ TABLE LT_EXISTENTES_EN_DESTINO
    WITH KEY TABLE_LINE = IV_ORDEN
    TRANSPORTING NO FIELDS
    BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      LV_RESULT = ABAP_TRUE.
      RETURN.
    ENDIF.

    READ TABLE LT_INEXISTENTES_EN_DESTINO
    WITH KEY TABLE_LINE = IV_ORDEN
    TRANSPORTING NO FIELDS
    BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      LV_RESULT = ABAP_FALSE.
      RETURN.
    ENDIF.

    DATA:
      LT_BAO6163 TYPE STANDARD TABLE OF TMW_6163,
      LT_E071K TYPE STANDARD TABLE OF TMW_E071K.

    CALL FUNCTION 'TMW_GET_REQUEST_INFO' DESTINATION IV_DESTINO_RFC
      EXPORTING
        IP_TRKORR           = IV_ORDEN
*     IMPORTING
*       EMPTY_REQUEST       =
      TABLES
        TT_BAO6163          = LT_BAO6163
        TT_E071K            = LT_E071K
      EXCEPTIONS
        FAILURE             = 1
        OTHERS              = 2
              .
    CASE SY-SUBRC.
      WHEN 0.
        LV_RESULT = ABAP_TRUE.
        INSERT IV_ORDEN INTO TABLE LT_EXISTENTES_EN_DESTINO.
      WHEN 1.
        LV_RESULT = ABAP_FALSE.
        INSERT IV_ORDEN INTO TABLE LT_INEXISTENTES_EN_DESTINO.
      WHEN OTHERS.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDCASE.

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

  METHOD FACTORY_COLLECTION_BY_CLASS.
*        IMPORTING
*          IV_CLASS_NAME TYPE CSEQUENCE
*          IV_OBJ_NAME TYPE CSEQUENCE
*        CHANGING
*          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

    DATA:
      LV_MANEJADOR_TIPOS_OBJETO TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.

    LV_MANEJADOR_TIPOS_OBJETO =
      LCL_MANEJADOR_TIPOS_OBJETO=>GET_INSTANCE( ).

    LV_MANEJADOR_TIPOS_OBJETO->FACTORY_COLLECTION_BY_CLASS(
      EXPORTING
        IV_CLASS_NAME = IV_CLASS_NAME
        IV_OBJ_NAME = IV_OBJ_NAME
      CHANGING
        CT_VERIFICABLES = CT_VERIFICABLES
    ).

  ENDMETHOD.

***********************************************************************
* Este método es el responsable de devolver todos los tipos de
* objeto posibles para un objeto dado, necesario para poder
* hacer la verificación total. Esto es necesario porque un mismo
* objeto puede tener más de un tipo. Por ejemplo, un programa,
* puede encontrarse en una orden de transporte identificado de
* tres formas distintas.
***********************************************************************
  METHOD FACTORY_COLLECTION.
*        IMPORTING IS_IDENTIFICACION_OBJETO TYPE
*          TY_S_IDENTIFICACION_OBJETO
*        CHANGING
*          CT_VERIFICABLES TYPE LCL_VERIFICABLE=>TY_T_VERIFICABLES,

    DATA:
      LV_MANEJADOR_TIPOS_OBJETO TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.

    LV_MANEJADOR_TIPOS_OBJETO =
      LCL_MANEJADOR_TIPOS_OBJETO=>GET_INSTANCE( ).

    LV_MANEJADOR_TIPOS_OBJETO->FACTORY_COLLECTION(
      EXPORTING
        IS_IDENTIFICACION_OBJETO = IS_IDENTIFICACION_OBJETO
      CHANGING
        CT_VERIFICABLES = CT_VERIFICABLES
    ).

  ENDMETHOD.

  METHOD SELECT_ORDENES_DONDE_ESTOY.

    FIELD-SYMBOLS:
      <LV_ORDEN> TYPE E070-TRKORR,
      <LV_DEPENDENCIA> TYPE REF TO LCL_VERIFICABLE.

    DATA:
      LT_ORDENES TYPE STANDARD TABLE OF E070-TRKORR.

    " Se obtienen las ordenes de transporte en las que está incluido el
    " objeto
    SELECT E071~TRKORR
    FROM E071
    INTO TABLE LT_ORDENES
    WHERE
      PGMID EQ ME->LS_IDENTIFICACION_OBJETO-PGMID AND
      OBJECT EQ ME->LS_IDENTIFICACION_OBJETO-OBJECT AND
      OBJ_NAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME AND
      " Se verifica que no sea una tarea
      " @todo Ver si esto debería estar condicionado a algún parámetro.
      E071~TRKORR IN (
        SELECT E070~STRKORR
        FROM E070
        WHERE E070~STRKORR EQ E071~TRKORR
        ).

    " Se obtienen las ordenes de transporte de las tareas en las que
    " está incluido el objeto
    SELECT E070~STRKORR
    FROM E071 INNER JOIN E070
    ON
      E071~TRKORR EQ E070~TRKORR AND
      E070~STRKORR NE SPACE
    APPENDING TABLE LT_ORDENES
    WHERE
      PGMID EQ ME->LS_IDENTIFICACION_OBJETO-PGMID AND
      OBJECT EQ ME->LS_IDENTIFICACION_OBJETO-OBJECT AND
      OBJ_NAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.

    SORT LT_ORDENES.
    DELETE ADJACENT DUPLICATES FROM LT_ORDENES.

    IF LT_ORDENES IS INITIAL.
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

  ENDMETHOD.

  METHOD SELECT_DEPENDENCIAS.

    " En caso de tener que seleccionar órdenes para analizar
    " dependencias...
    IF LCL_FILTROS=>LV_NO_SELECCIONAR_ORDENES EQ ABAP_FALSE.

      ME->SELECT_ORDENES_DONDE_ESTOY( ).

    ENDIF.

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD SELECT_ORDENES.

    SUPER->SELECT_ORDENES( ).

    ME->SELECT_ORDENES_DONDE_ESTOY( ).

  ENDMETHOD.

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Objeto orden'.

  ENDMETHOD.

  METHOD GET_TEXT.
*        RETURNING
*          VALUE(EV_RESULT) TYPE LVC_VALUE.

    EV_RESULT = LS_IDENTIFICACION_OBJETO-OBJ_NAME.
  ENDMETHOD.


ENDCLASS.                    "LCL_OBJETO_ORDEN IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_PROGRAMA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_PROGRAMA IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*        IMPORTING
*          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO,
    SUPER->CONSTRUCTOR( IS_IDENTIFICACION_OBJETO ).

    ME->LV_NOMBRE_PROGRAMA = ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
  ENDMETHOD.

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_INCLUDE> TYPE D010INC-INCLUDE.

    DATA:
      LT_INCLUDE TYPE STANDARD TABLE OF D010INC-INCLUDE.

    SUPER->SELECT_DEPENDENCIAS( ).

    LCL_TABLA=>FILTER_BY_MASTER(
      EXPORTING
        IV_MASTER = ME->LV_NOMBRE_PROGRAMA
      CHANGING
        CT_VERIFICABLES = ME->LT_DEPENDENCIAS
    ).

    " Agrega como dependencias a los includes.
    SELECT INCLUDE
    FROM D010INC
    INTO TABLE LT_INCLUDE
    WHERE
      MASTER EQ ME->LV_NOMBRE_PROGRAMA AND
      ( INCLUDE LIKE 'Z%' OR
      INCLUDE LIKE 'Y%' OR
      INCLUDE LIKE 'LZ%' ).

    " En las pruebas se detectó la obtención de duplicados, por tanto
    " se eliminan.
    SORT LT_INCLUDE.
    DELETE ADJACENT DUPLICATES FROM LT_INCLUDE.

    LOOP AT LT_INCLUDE ASSIGNING <LV_INCLUDE>.

* Se excluyen includes correspondientes a módulos de función.
      IF <LV_INCLUDE> CP 'LZ*U++' OR <LV_INCLUDE> CP 'LY*U++'.
        CONTINUE.
      ENDIF.

      LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
        EXPORTING
          IV_CLASS_NAME = 'LCL_PROGRAMA'
          IV_OBJ_NAME = <LV_INCLUDE>
        CHANGING
          CT_VERIFICABLES = ME->LT_DEPENDENCIAS
        ).

    ENDLOOP.

    " @todo: Agregar tipo de objeto dynpro
    " Tabla D020S: A partir de PROG se obtienen todos los dynpros (DNUM).

    " @todo: Agregar como dependencias el programa padre, en caso
    " de ser un include. Esto sería necesario porque las dependencias
    " solo se pueden obtener a partir del programa principal y NO a
    " partir de include

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Programa'.

  ENDMETHOD.

ENDCLASS.                    "LCL_PROGRAMA IMPLEMENTATION

CLASS LCL_GRUPO_FUNCIONES IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*        IMPORTING
*          IS_IDENTIFICACION_OBJETO TYPE TY_S_IDENTIFICACION_OBJETO,
    SUPER->CONSTRUCTOR( IS_IDENTIFICACION_OBJETO ).

    CONCATENATE
      'SAPL'
      ME->LV_NOMBRE_PROGRAMA
    INTO
      ME->LV_NOMBRE_PROGRAMA.

  ENDMETHOD.

  METHOD SELECT_DEPENDENCIAS.

    FIELD-SYMBOLS:
      <LV_FUNCNAME> TYPE TFDIR-FUNCNAME.

    DATA:
      LT_FUNCNAME TYPE STANDARD TABLE OF TFDIR-FUNCNAME.

    SUPER->SELECT_DEPENDENCIAS( ).

    " Se agrega las funciones del grupo de función.
    SELECT FUNCNAME
    FROM TFDIR
    INTO TABLE LT_FUNCNAME
    WHERE
      PNAME EQ ME->LV_NOMBRE_PROGRAMA AND
      ( FUNCNAME LIKE 'Z%' OR
      FUNCNAME LIKE 'Y%' ).

    LOOP AT LT_FUNCNAME ASSIGNING <LV_FUNCNAME>.

      LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
        EXPORTING
          IV_CLASS_NAME = 'LCL_FUNCION'
          IV_OBJ_NAME = <LV_FUNCNAME>
        CHANGING
          CT_VERIFICABLES = ME->LT_DEPENDENCIAS
        ).

    ENDLOOP.

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Grupo de funciones'.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_TABLA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_TABLA IMPLEMENTATION.

  METHOD FILTER_BY_MASTER.
*    IMPORTING
*      IV_MASTER TYPE CSEQUENCE
*    CHANGING
*      CT_VERIFICABLES TYPE TY_T_VERIFICABLES.

    FIELD-SYMBOLS:
      <LV_TABNAME> TYPE D010TAB-TABNAME.

    DATA:
      LT_TABNAME TYPE STANDARD TABLE OF D010TAB-TABNAME.

    SELECT TABNAME
    FROM D010TAB
    INTO TABLE LT_TABNAME
    WHERE
      MASTER EQ IV_MASTER AND
      ( TABNAME LIKE 'Z%' OR
      TABNAME LIKE 'Y%' ).

    LOOP AT LT_TABNAME ASSIGNING <LV_TABNAME>.

      LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
        EXPORTING
          IV_CLASS_NAME = 'LCL_TABLA'
          IV_OBJ_NAME = <LV_TABNAME>
        CHANGING
          CT_VERIFICABLES = CT_VERIFICABLES
        ).

    ENDLOOP.

  ENDMETHOD.

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

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Tabla'.

  ENDMETHOD.

ENDCLASS.                    "LCL_TABLA IMPLEMENTATION

CLASS LCL_CLASE IMPLEMENTATION.

  METHOD SELECT_DEPENDENCIAS.

    TYPES:
      BEGIN OF TY_S_SEOCOMPO,
        CMPNAME TYPE SEOCOMPO-CMPNAME,
      END OF TY_S_SEOCOMPO.


    CONSTANTS:
      C_METODO TYPE SEOCOMPO-CMPTYPE VALUE '1'.

    FIELD-SYMBOLS:
      <LS_SEOCOMPO> TYPE TY_S_SEOCOMPO.

    DATA:
      LT_SEOCOMPO TYPE STANDARD TABLE OF TY_S_SEOCOMPO,
      LV_MASTER TYPE D010TAB-MASTER,
      LV_OBJ_NAME TYPE E071-OBJ_NAME.

    SUPER->SELECT_DEPENDENCIAS( ).

    " Se obtienen los métodos para la clase analizada
    SELECT CMPNAME
    FROM SEOCOMPO
    INTO TABLE LT_SEOCOMPO
    WHERE
      CLSNAME EQ ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME AND
      CMPTYPE EQ C_METODO.

    LOOP AT LT_SEOCOMPO ASSIGNING <LS_SEOCOMPO>.

      LV_OBJ_NAME = ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
      LV_OBJ_NAME+30 = <LS_SEOCOMPO>-CMPNAME.

      LCL_OBJETO_ORDEN=>FACTORY_COLLECTION_BY_CLASS(
        EXPORTING
          IV_CLASS_NAME = 'LCL_METODO'
          IV_OBJ_NAME = LV_OBJ_NAME
        CHANGING
          CT_VERIFICABLES = ME->LT_DEPENDENCIAS
        ).

    ENDLOOP.

    " Se genera el nombre para el campo MASTER a partir del nombre de
    " la clase.
    LV_MASTER = ME->LS_IDENTIFICACION_OBJETO-OBJ_NAME.
    TRANSLATE LV_MASTER(30) USING ' ='.
    LV_MASTER+30(2) = 'CP'.

    LCL_TABLA=>FILTER_BY_MASTER(
      EXPORTING
        IV_MASTER = LV_MASTER
      CHANGING
        CT_VERIFICABLES = ME->LT_DEPENDENCIAS
    ).

  ENDMETHOD.                    "SELECT_DEPENDENCIAS

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Clase'.

  ENDMETHOD.

ENDCLASS.                    "LCL_CLASE IMPLEMENTATION

CLASS LCL_FUNCION IMPLEMENTATION.

* @todo Ver si se debería agregar al grupo de función como dependencia,
* ya que las dependencias de tablas e includes solo se pueden obtener a
* partir del grupo de función.

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Función'.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_DOMINIO IMPLEMENTATION.

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Dominio'.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_ELEMENTO_DATOS IMPLEMENTATION.

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Elemento de datos'.

  ENDMETHOD.

ENDCLASS.

CLASS LCL_METODO IMPLEMENTATION.

  METHOD GET_LINEA_TREE.
*        RETURNING
*          VALUE(LS_RESULT) TYPE LCL_ALV_TREE=>TY_S_LINEA_TREE.
    LS_RESULT-TIPO = 'Método'.

  ENDMETHOD.

ENDCLASS.

TABLES:
  E071.

SELECTION-SCREEN BEGIN OF BLOCK FILTROS WITH FRAME TITLE TEXT-B01.
  PARAMETERS:
    P_TRKORR TYPE E070-TRKORR.  "Orden de transporte
  SELECT-OPTIONS:
    S_OBJNAM FOR E071-OBJ_NAME. "Nombre de objeto en órdenes de transporte
SELECTION-SCREEN END OF BLOCK FILTROS.

SELECTION-SCREEN BEGIN OF BLOCK EJECUCION WITH FRAME TITLE TEXT-B02.
  PARAMETERS:
    P_NOSELO  TYPE ABAP_BOOL AS CHECKBOX DEFAULT ABAP_TRUE, " No seleccionar órdens para verificación de dependencias
    P_LISORD TYPE ABAP_BOOL AS CHECKBOX DEFAULT ABAP_FALSE, "Mostrar solo órdenes
*    P_VALEXT TYPE ABAP_BOOL AS CHECKBOX DEFAULT ABAP_FALSE, "Mostrar solo órdenes NO transp. a:
    P_MNOTRA TYPE ABAP_BOOL AS CHECKBOX DEFAULT ABAP_TRUE, "Mostrar solo ord. NO trans. a:
    P_RFCDST TYPE EDI_LOGDES, "Destino RFC (del destino)
    P_GETTAR TYPE ABAP_BOOL NO-DISPLAY DEFAULT ABAP_FALSE. "Obtener tareas
SELECTION-SCREEN END OF BLOCK EJECUCION.

DATA:
      LV_ORDEN_TRANSPORTE TYPE REF TO LCL_ORDEN_TRANSPORTE,
      LV_MANEJADOR_TIPOS TYPE REF TO LCL_MANEJADOR_TIPOS_OBJETO.

AT SELECTION-SCREEN.

  IF P_MNOTRA EQ ABAP_TRUE AND P_RFCDST IS INITIAL.
    MESSAGE 'Debe indicar el destino RFC al estar tildado "Mostrar solo ord. NO trans. a:"'(002) TYPE 'E'.
  ENDIF.
*
**  CHECK P_VALEXT EQ ABAP_FALSE.
*  CHECK P_MNOTRA EQ ABAP_TRUE.
*
*  IF LCL_ORDEN_TRANSPORTE=>ORDEN_EXISTE_EN_DESTINO(
*    IV_ORDEN = P_TRKORR
*    IV_DESTINO_RFC = P_RFCDST
*    ) EQ ABAP_TRUE.
*
*    MESSAGE 'La orden de transporte ya existe en el destino seleccionado'(001) TYPE 'E'.
*
*  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_RFCDST.

  DATA:
    LV_DESTINO_RFC TYPE EDI_LOGDES.

  perform values_for_logdes(SAPMSEDPORT)
    using
      '1'
      SPACE
      LV_DESTINO_RFC.

  IF LV_DESTINO_RFC IS NOT INITIAL.
    P_RFCDST = LV_DESTINO_RFC.
  ENDIF.

INITIALIZATION.

  TYPES:
    TY_R_OBJ_NAME TYPE RANGE OF E071-OBJ_NAME.

  FIELD-SYMBOLS:
    <LS_OBJNAM> TYPE LINE OF TY_R_OBJ_NAME.

  APPEND INITIAL LINE TO S_OBJNAM ASSIGNING <LS_OBJNAM>.
  <LS_OBJNAM>-SIGN = 'I'.
  <LS_OBJNAM>-OPTION = 'CP'.
  <LS_OBJNAM>-LOW = 'Z*'.

  LV_MANEJADOR_TIPOS = LCL_MANEJADOR_TIPOS_OBJETO=>GET_INSTANCE( ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'PROG'
    IV_CLASE_ASOCIADA = 'LCL_PROGRAMA'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'TABL'
    IV_CLASE_ASOCIADA = 'LCL_TABLA'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'DTEL'
    IV_CLASE_ASOCIADA = 'LCL_ELEMENTO_DATOS'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'DOMA'
    IV_CLASE_ASOCIADA = 'LCL_DOMINIO'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'CLAS'
    IV_CLASE_ASOCIADA = 'LCL_CLASE'
    ).

* @todo Verificar si aplica para registro de clases
  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'CLSD'
    IV_CLASE_ASOCIADA = 'LCL_CLASE'
    ).

* @todo Verificar si aplica para registro de clases
  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'CPUB'
    IV_CLASE_ASOCIADA = 'LCL_CLASE'
    ).

* @todo Verificar si aplica para registro de clases
  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'CPRI'
    IV_CLASE_ASOCIADA = 'LCL_CLASE'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'METH'
    IV_CLASE_ASOCIADA = 'LCL_METODO'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'REPS'
    IV_CLASE_ASOCIADA = 'LCL_PROGRAMA'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'TABT'
    IV_CLASE_ASOCIADA = 'LCL_PROGRAMA'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'TABD'
    IV_CLASE_ASOCIADA = 'LCL_TABLA'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'R3TR'
    IV_OBJECT = 'FUGR'
    IV_CLASE_ASOCIADA = 'LCL_GRUPO_FUNCIONES'
    ).

  LV_MANEJADOR_TIPOS->REGISTRAR(
    IV_PGMID = 'LIMU'
    IV_OBJECT = 'FUNC'
    IV_CLASE_ASOCIADA = 'LCL_FUNCION'
    ).

START-OF-SELECTION.

  LCL_FILTROS=>LR_OBJ_NAME = S_OBJNAM[].
  LCL_FILTROS=>LV_OBTENER_TAREAS_ORDENES = P_GETTAR.
  LCL_FILTROS=>LV_LISTAR_SOLO_ORDENES = P_LISORD.
  LCL_FILTROS=>LV_MOSTRAR_SOLO_ORD_NO_TRANS = P_MNOTRA.
  LCL_FILTROS=>LV_DESTINO_RFC = P_RFCDST.
  LCL_FILTROS=>LV_NO_SELECCIONAR_ORDENES = P_NOSELO.

  CREATE OBJECT LV_ORDEN_TRANSPORTE
    EXPORTING
      IV_NUMERO_ORDEN = P_TRKORR.

  LV_ORDEN_TRANSPORTE->VERIFICADO( ).
  LV_ORDEN_TRANSPORTE->VERIFICAR( ).

  " En caso de no haber seleccionado ordenes para cada objeto, de forma
  " que no se expanda demasiado el arbol de dependencias...
  IF LCL_FILTROS=>LV_NO_SELECCIONAR_ORDENES EQ ABAP_TRUE.
    " Se obtienen las órdenes de cada objeto del arbol.
    LV_ORDEN_TRANSPORTE->SELECT_ORDENES( ).
  ENDIF.

END-OF-SELECTION.

  lcl_alv_tree=>mostrar_dependencias( LV_ORDEN_TRANSPORTE ).
