*&---------------------------------------------------------------------*
*& Include ZPPRE_CEJA_01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Programa   : ZRE_CEJA                                                *
* Descripción: Impresión de Cejas_PP                                   *
* Solicitante: Jessica Priscilla Rojas Romero                          *
* Programador: David Navoa Acevedo                                     *
* Fecha      : 06.07.23                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     F_ORDER_SUGESTION2                                               *
*----------------------------------------------------------------------*
FORM f_order_sugestion2.
  DATA: lt_return TYPE TABLE OF ddshretval.

  REFRESH lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '2 Botes de 4 Lts.'.
  APPEND ls_envases TO lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '10 Botes'.
  APPEND ls_envases TO lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '4 Botes de 1 L'.
  APPEND ls_envases TO lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '6 Botes de  .500 L'.
  APPEND ls_envases TO lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '6 Botes'.
  APPEND ls_envases TO lt_envases.

  CLEAR ls_envases.
  ls_envases-env = '12 Cajas'.
  APPEND ls_envases TO lt_envases.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ENV'
      "dynprog         = sy-repid
      "dynpnr          = '1000'
      "dynprofield     = 'S_AUFNR'
      window_title    = 'Envases'
      value_org       = 'S'
    TABLES
      value_tab       = lt_envases
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
    IF sy-subrc EQ 0.

      s_env-low = ls_ret-fieldval.

      CASE ls_ret-fieldval.
        WHEN '2 Botes de 4 Lts.'.
          gv_env = '2B 4L'.
        WHEN '10 Botes'.
          gv_env = '10B'.
        WHEN '4 Botes de 1 L'.
          gv_env = '4B 1L'.
        WHEN '6 Botes de  .500 L'.
          gv_env = '6B .5L'.
        WHEN '6 Botes'.
          gv_env = '6B'.
        WHEN '12 Cajas'.
          gv_env = '12C'.
      ENDCASE.

    ENDIF.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*     F_MOD_SCREEN                                                     *
*----------------------------------------------------------------------*
FORM f_mod_screen.
  IF
   screen-name = '%_S_ENV_%_APP_%-OPTI_PUSH' OR
   screen-name = '%C003018_1000' OR
   screen-name = 'S_ENV-LOW'.

    screen-invisible = 1.
    screen-active = 0.

  ENDIF.

  IF p_grand EQ 'X' AND
    ( screen-name = '%_S_ENV_%_APP_%-OPTI_PUSH' OR
      screen-name = '%C003018_1000' OR
      screen-name = 'S_ENV-LOW' ).

    screen-invisible = 0.
    screen-active = 1.

  ENDIF.

  IF  screen-name = '%BH02022_BLOCK_1000' OR
       screen-name = '%C007024_1000' OR
       screen-name = '%C008025_1000' OR
       screen-name = '%C009026_1000' OR
       screen-name = '%C010027_1000' OR
       screen-name = '%C011030_1000' OR
       screen-name = '%C012031_1000'.

    screen-invisible = 1.
    screen-active = 0.

  ENDIF.

  "El siguiente campo se le debe cambiar el valor
  "Este debe ser el Numero del material
  "- %C008025_1000
  "Este debe ser la descripcion del material
  "- %C010027_1000
  "Este debe ser el codigo EAN
  "- %C012031_1000

  IF gs_ordenes IS NOT INITIAL AND
     ( screen-name = '%BH02022_BLOCK_1000' OR
       screen-name = '%C007024_1000' OR
       screen-name = '%C008025_1000' OR
       screen-name = '%C009026_1000' OR
       screen-name = '%C010027_1000' OR
       screen-name = '%C011030_1000' OR
       screen-name = '%C012031_1000')
    .

    screen-invisible = 0.
    screen-active = 1.

    gv_matnr      = gs_ordenes-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_ordenes-matnr
      IMPORTING
        output = gv_matnr.

    %c008025_1000 = gv_matnr.
    %c010027_1000 = gs_ordenes-maktx.
    %c012031_1000 = gs_ordenes-ean.

  ENDIF.



ENDFORM.

*----------------------------------------------------------------------*
*     F_ORDER_SUGESTION                                                *
*----------------------------------------------------------------------*
FORM f_order_sugestion.

  DATA: lt_return TYPE TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUFNR'
      "dynprog         = sy-repid
      "dynpnr          = '1000'
      "dynprofield     = 'S_AUFNR'
      window_title    = 'Ordenes'
      value_org       = 'S'
    TABLES
      value_tab       = lt_Ordenes
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
    IF sy-subrc EQ 0.
      s_aufnr-low = ls_ret-fieldval.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*     F_USER_PRE_VALIDATION                                                *
*----------------------------------------------------------------------*
FORM f_user_pre_validation.

  DATA: lv_centro       TYPE werks,
        lv_dauat        TYPE afpo-dauat,
        lv_aufnr        TYPE afpo-aufnr,
        lv_msj          TYPE string,
        lv_matnr        TYPE mara-matnr,
        lv_flag_e       TYPE c,
        lt_descripcion  TYPE TABLE OF makt,
        lt_centros_aux1 TYPE TABLE OF ty_centros,
        lt_aufk_jest    TYPE TABLE OF ty_aufk_jest,
        lt_ordenes_aux1 TYPE TABLE OF zespp_orden_ceja,
        lt_ordenes_aux2 TYPE TABLE OF zespp_orden_ceja,
        lt_centros      TYPE TABLE OF ty_centros.

  FIELD-SYMBOLS: <wa_mara>    TYPE zespp_orden_ceja,
                 <fs_ordenes> TYPE zespp_orden_ceja.

  CLEAR: lv_aufnr, lv_dauat, lv_centro, lv_matnr, lt_Ordenes[], ls_ordenes, lv_flag_e, lt_ordenes_aux2[], lt_ordenes_aux2[], lt_centros_aux1[].

  "Llenado y validaciones para el matchcode

  "Campo Ordenes de la estructura
  SELECT werks
         name1
    FROM t001w
    INTO TABLE lt_centros.
  IF sy-subrc EQ 0.
    LOOP AT lt_centros INTO DATA(ls_centros_aux1).
      AUTHORITY-CHECK OBJECT 'C_AFKO_AWK' FOR USER sy-uname
      ID 'WERKS'  FIELD ls_centros_aux1-werks.
      IF sy-subrc EQ 0.
        APPEND ls_centros_aux1 TO lt_centros_aux1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Tabla a llenar lt_ordenes, estructura a llenar ls_ordenes

  SELECT aufnr matnr FROM afpo                     "#EC CI_NO_TRANSFORM
    INTO TABLE lt_ordenes
    FOR ALL ENTRIES IN lt_centros_aux1
    WHERE pwerk = lt_centros_aux1-werks
    AND dauat = 'YBM2'.
  IF sy-subrc EQ 0.

    "Campo Numero de Material de la estructura
    SELECT a~aufnr, b~matnr ##SHADOW
      FROM mara AS b
      JOIN @lt_ordenes AS a
      ON  b~matnr EQ a~matnr
      AND b~mtart EQ 'ZFER'
      INTO CORRESPONDING FIELDS OF TABLE @lt_ordenes_aux1.
    IF sy-subrc EQ 0.

      LOOP AT lt_ordenes_aux1 ASSIGNING  <wa_mara>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <wa_mara>-matnr
          IMPORTING
            output = lv_matnr.

        DATA(lv_tabix) = sy-tabix.
        DATA(lv_leng) = strlen( lv_matnr ).
        lv_leng = lv_leng - 2.
        lv_matnr = lv_matnr+lv_leng(2).

        IF lv_matnr EQ '00'.
          DELETE lt_ordenes_aux1 INDEX lv_tabix.
        ENDIF.

      ENDLOOP.

      UNASSIGN <wa_mara>.
    ELSE.
      lv_flag_e = 'X'.
    ENDIF.

    SELECT c~aufnr, d~objnr ##SHADOW
      FROM aufk AS c
      JOIN jest AS d
      ON  c~objnr EQ d~objnr
      JOIN @lt_ordenes_aux1 AS e
      ON c~aufnr  EQ e~aufnr
      AND d~stat  EQ 'I0002'
      AND d~inact EQ ' '
      INTO CORRESPONDING FIELDS OF TABLE @lt_aufk_jest.
    IF sy-subrc EQ 0.

      LOOP AT lt_aufk_jest INTO DATA(ls_aufk_jest).
        READ TABLE lt_ordenes_aux1 INTO DATA(ls_ordenes_aux1) WITH KEY aufnr = ls_aufk_jest-aufnr.
        IF sy-subrc EQ 0.
          APPEND ls_ordenes_aux1 TO lt_ordenes_aux2.
        ENDIF.
      ENDLOOP.

    ELSE.
      lv_flag_e = 'X'.
    ENDIF.

    lt_Ordenes[] = lt_ordenes_aux2[].
  ELSE.
    lv_flag_e = 'X'.
  ENDIF.

  IF lv_flag_e = 'X'.
    MESSAGE TEXT-e04 TYPE 'E'.
  ENDIF.

  REFRESH lt_ordenes_aux2.
  "Descripcion del material
  SELECT matnr
         maktx
    FROM makt
    INTO CORRESPONDING FIELDS OF TABLE lt_ordenes_aux2
    FOR ALL ENTRIES IN lt_ordenes
    WHERE matnr = lt_ordenes-matnr
    AND   spras = 'S'.
  IF sy-subrc EQ 0.
    LOOP AT lt_ordenes ASSIGNING <fs_ordenes>.
      READ TABLE lt_ordenes_aux2 INTO DATA(ls_ordenes_aux2) WITH KEY matnr = <fs_ordenes>-matnr.
      IF sy-subrc EQ 0.
        <fs_ordenes>-maktx = ls_ordenes_aux2-maktx.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*     F_USER_VALIDATION                                                *
*----------------------------------------------------------------------*
FORM f_user_validation USING pv_flag_ucomm.

  DATA: lv_centro TYPE werks,
        lv_dauat  TYPE afpo-dauat,
        lv_aufnr  TYPE afpo-aufnr,
        lv_msj    TYPE string,
        lv_flag_e TYPE c,
        lv_matnr  TYPE mara-matnr.

  CLEAR: lv_aufnr, lv_dauat, lv_centro, lv_matnr, lv_flag_e, gs_ordenes.

  SELECT SINGLE * FROM afpo INTO @DATA(ls_afpo) WHERE aufnr EQ @s_aufnr-low.
  IF sy-subrc EQ 0.

    lv_centro = ls_afpo-pwerk.
    lv_dauat  = ls_afpo-dauat.

    "Autority check para el centro y la clase de orden
    AUTHORITY-CHECK OBJECT 'C_AFKO_AWK' FOR USER sy-uname
    ID 'WERKS'  FIELD lv_centro
    ID 'AUFART' FIELD lv_DAUAT.
    IF sy-subrc NE 0 AND pv_flag_ucomm EQ 'X'.
      MESSAGE TEXT-e01 TYPE 'E'.
    ENDIF.

    CLEAR lv_msj.
    CONCATENATE TEXT-e02 s_aufnr-low TEXT-e21 INTO lv_msj SEPARATED BY space.

    "Validaciones Por material
    SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr EQ @ls_afpo-matnr.
    IF sy-subrc EQ 0 AND ls_mara-mtart EQ 'ZFER'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_mara-matnr
        IMPORTING
          output = lv_matnr.
      IF sy-subrc NE 0 AND pv_flag_ucomm EQ 'X'.
        lv_flag_e = 'X'.
      ENDIF.

      DATA(lv_leng) = strlen( lv_matnr ).
      lv_leng = lv_leng - 2.
      lv_matnr = lv_matnr+lv_leng(2).

      IF lv_matnr EQ '00' AND pv_flag_ucomm EQ 'X'.
        lv_flag_e = 'X'.
      ENDIF.

    ELSEIF pv_flag_ucomm EQ 'X'.
      lv_flag_e = 'X'.
    ENDIF.

    CLEAR lv_msj.
    CONCATENATE TEXT-e02 s_aufnr-low TEXT-e22 INTO lv_msj SEPARATED BY space.

    "Validacion del estatus de la Orden
    SELECT SINGLE * FROM jest INTO @DATA(ls_jest) WHERE objnr EQ ( SELECT objnr FROM aufk WHERE aufnr IN @s_aufnr )
      AND stat  EQ 'I0002'
      AND inact EQ ' '.
    IF sy-subrc EQ 0.

    ELSEIF pv_flag_ucomm EQ 'X'.
      lv_flag_e = 'X'.
    ENDIF.
  ELSEIF pv_flag_ucomm EQ 'X'.
    MESSAGE TEXT-e03 TYPE 'E'.
  ENDIF.

  IF lv_flag_e = 'X' AND pv_flag_ucomm EQ 'X'.
    MESSAGE lv_msj TYPE 'E'.
  ENDIF.

  IF ls_jest IS NOT INITIAL.

    gs_ordenes-aufnr = ls_afpo-aufnr.
    gs_ordenes-matnr = ls_mara-matnr.
    gs_ordenes-ean   = ls_mara-ean11.

    SELECT SINGLE * FROM makt INTO @DATA(ls_makt) WHERE matnr = @ls_mara-matnr.
    IF sy-subrc EQ 0.
      gs_ordenes-maktx = ls_makt-maktx.
    ENDIF.

    gs_ordenes-objnr = ls_jest-objnr.

  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*     F_USER_VALIDATION                                                *
*----------------------------------------------------------------------*
FORM f_call_adobeform.

  DATA: lv_funcname     TYPE funcname,
        ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_formoutput   TYPE fpformoutput,
        lv_pia          TYPE char32,
        lv_proj         TYPE char32.

  DATA: lv_name TYPE fpname VALUE 'ZPPAF_ETIQUETAS'.

  ls_outputparams-nodialog = space.
  "ls_outputparams-device    = 'PRINTER'.
  ls_outputparams-preview = abap_true.
  "ls_outputparams-getpdf = abap_true.
  "ls_outputparams-assemble   = abap_true.

  IF p_chica = abap_true.
    gs_ordenes-etiqueta = 'C'.
  ELSEIF p_media = abap_true.
    gs_ordenes-etiqueta = 'M'.
    lv_name = 'ZPPAF_ETIQUETAS_M'.
  ELSEIF p_grand = abap_true.
    gs_ordenes-etiqueta = 'G'.
    lv_name = 'ZPPAF_ETIQUETAS_G'.
    gs_ordenes-envases = gv_env.
  ENDIF.

  " DO s_int-low TIMES.
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE 'Error en Impresion' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.
  " ENDDO.




  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = lv_name
    IMPORTING
      e_funcname = lv_funcname
*     E_INTERFACE_TYPE           =
*     EV_FUNCNAME_INBOUND        =
    .

  ls_docparams-langu   = 'S'.
  ls_docparams-country = 'MX'.

  DO s_int-low TIMES.

    CALL FUNCTION lv_funcname
      EXPORTING
        /1bcdwb/docparams = ls_docparams
        es_ordenes        = gs_ordenes
* IMPORTING
*       /1BCDWB/FORMOUTPUT       =
      EXCEPTIONS
        usage_error       = 1
        system_error      = 2
        internal_error    = 3
        OTHERS            = 4.
    IF sy-subrc EQ 0.

    ENDIF.

  ENDDO.

  CALL FUNCTION 'FP_JOB_CLOSE'
*   IMPORTING
*     E_RESULT             =
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.
