*&---------------------------------------------------------------------*
*& Report ZRE_CEJA
*----------------------------------------------------------------------*
* Programa   : ZRE_CEJA                                                *
* Descripción: Impresión de Cejas_PP                                   *
* Solicitante: Jessica Priscilla Rojas Romero                          *
* Programador: David Navoa Acevedo                                     *
* Fecha      : 06.07.23                                                *
*----------------------------------------------------------------------*
REPORT zre_ceja.

INCLUDE zppre_ceja_top. "Declaraciones
INCLUDE zppre_ceja_01.  "Rutinas

*----------------------------------------------------------------------*
*                    I N I T I A L I Z A T I O N                       *
*----------------------------------------------------------------------*
INITIALIZATION.

  PERFORM f_user_pre_validation.

*----------------------------------------------------------------------*
*              A T   S E L E C T I O N   S C R E E N                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_aufnr-low.

  PERFORM f_order_sugestion.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_env-low.

  PERFORM f_order_sugestion2.

AT SELECTION-SCREEN OUTPUT.

  CLEAR gv_flag_ucom. "agregado para no mostrar errores en este proceso.

  PERFORM f_user_validation USING gv_flag_ucom.

  LOOP AT SCREEN.

    PERFORM f_mod_screen.

    MODIFY SCREEN.

  ENDLOOP.




*----------------------------------------------------------------------*
*                 S T A R T   OF   S E L E C T I O N                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  gv_flag_ucom = 'X'.
  PERFORM f_user_validation USING gv_flag_ucom.

  PERFORM f_call_adobeform.
