FUNCTION ZI_RISHI_SET_PO_STATUS_OPEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_PO_TAB) TYPE  ZRSH_IF_RAP_BATCH1=>TT_PO_STATUS OPTIONAL
*"  EXPORTING
*"     VALUE(LT_MESSAGES) TYPE  BAPIRETTAB
*"----------------------------------------------------------------------
SELECT * FROM zrishi_podoc INTO TABLE @DATA(lt_podocs) FOR ALL ENTRIES IN @it_po_tab
WHERE po_document EQ @it_po_tab-purchase_doc.
IF sy-subrc EQ 0.
LOOP AT lt_podocs ASSIGNING FIELD-SYMBOL(<lfs_podoc>).
<lfs_podoc>-po_status = '3'.
ENDLOOP.
"update status
UPDATE zrishi_podoc FROM TABLE @lt_podocs.
ENDIF.






ENDFUNCTION.
