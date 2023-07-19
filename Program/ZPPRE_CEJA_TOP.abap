*&---------------------------------------------------------------------*
*& Include ZPPRE_CEJA_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Programa   : ZRE_CEJA                                                *
* Descripción: Impresión de Cejas_PP                                   *
* Solicitante: Jessica Priscilla Rojas Romero                          *
* Programador: David Navoa Acevedo                                     *
* Fecha      : 06.07.23                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                      I N F O T I P O S                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                        T A B L A S                                   *
*----------------------------------------------------------------------*
TABLES: afpo.
*----------------------------------------------------------------------*
*                         T I P O S                                    *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_centros,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END OF ty_centros,

  BEGIN OF ty_aufk_jest,
    aufnr TYPE aufk-aufnr,
    objnr TYPE jest-objnr,
  END OF ty_aufk_jest,

  BEGIN OF ty_ordenes,
    aufnr TYPE aufk-aufnr,
    matnr TYPE mara-matnr,
    ean   TYPE mara-ean11,
    maktx TYPE makt-maktx,
    objnr TYPE jest-objnr,
  END OF ty_ordenes,

  BEGIN OF ty_envases,
    env TYPE zppde_t_envase,
  END OF ty_envases
  .
*----------------------------------------------------------------------*
*                      C O N S T A N T E S                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                      E S T R U C T U R A S                           *
*----------------------------------------------------------------------*
DATA: ls_ordenes TYPE zespp_orden_ceja,
      gs_ordenes TYPE zppes_ordenes,
      ls_envases TYPE ty_envases.
*----------------------------------------------------------------------*
*                  T A B L A S  I N T E R N A S                        *
*----------------------------------------------------------------------*
DATA: lt_ordenes TYPE TABLE OF zespp_orden_ceja,
      lt_envases TYPE TABLE OF ty_envases.
*----------------------------------------------------------------------*
*                          R A N G O S                                 *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                      F I E L D - S Y M B O L S                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                      V A R I A B L E S                               *
*----------------------------------------------------------------------*
DATA: lv_envases   TYPE char15,
      n(4)         TYPE n,
      lv_int(5)    TYPE n,
      gv_env       TYPE zppde_envases,
      gv_flag_ucom TYPE c,
      gv_matnr     TYPE mara-matnr.

*----------------------------------------------------------------------*
*                       C L A S E S                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                     P A R A M E T R O S                              *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(10) TEXT-001.
  SELECT-OPTIONS: s_aufnr FOR afpo-aufnr NO INTERVALS NO-EXTENSION OBLIGATORY.

  SELECTION-SCREEN COMMENT 50(19) TEXT-002.
  SELECT-OPTIONS: s_int FOR lv_int NO INTERVALS NO-EXTENSION OBLIGATORY.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 20(16) TEXT-005.
    PARAMETERS: p_chica RADIOBUTTON GROUP a DEFAULT 'X' USER-COMMAND d. "Etiqueta Chica
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 20(16) TEXT-006.
    PARAMETERS: p_media RADIOBUTTON GROUP a.                            "Etiqueta Mediana
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 20(16) TEXT-004.
    PARAMETERS: p_grand RADIOBUTTON GROUP a.                           "Etiqueta Grande

    SELECTION-SCREEN COMMENT 50(19) TEXT-003.
    SELECT-OPTIONS: s_env FOR lv_envases  NO INTERVALS NO-EXTENSION.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-h02.
  SELECTION-SCREEN BEGIN OF LINE.
    "Material
    SELECTION-SCREEN COMMENT 10(12) TEXT-007.
    SELECTION-SCREEN COMMENT 22(13) TEXT-008.

    "Descripcion
    SELECTION-SCREEN COMMENT 40(13) TEXT-009.
    SELECTION-SCREEN COMMENT 53(40) TEXT-010.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    "EAN
    SELECTION-SCREEN COMMENT 10(12) TEXT-011.
    SELECTION-SCREEN COMMENT 22(16) TEXT-012.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.
