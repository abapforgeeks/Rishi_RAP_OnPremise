CLASS lcl_trbuffer DEFINITION.
  PUBLIC SECTION.

    TYPES:BEGIN OF lty_po_doc,
           purchase_doc TYPE zebeln,
           END OF lty_po_doc.

    CLASS-DATA: mt_create_purchasedoc TYPE TABLE OF zrishi_podoc.
    CLASS-DATA: mt_delete_purchasedoc TYPE TABLE OF zrishi_podoc.
    CLASS-DATA: mt_update_purchasedoc TYPE TABLE OF zrishi_podoc.
    CLASS-DATA: mt_create_po_items TYPE TABLE OF zrishi_poitem.
    CLASS-DATA: mt_po_status TYPE TABLE OF lty_po_doc.
ENDCLASS.


CLASS lhc_ZI_RISHI_PURCHASEDOC_U DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create_purchaseorder FOR MODIFY
      IMPORTING it_purchase_create FOR CREATE PurchaseHeader.

    METHODS delete_purchaseorder FOR MODIFY
      IMPORTING it_delete_po FOR DELETE PurchaseHeader.

    METHODS update_purchaseorder FOR MODIFY
      IMPORTING it_update_po FOR UPDATE PurchaseHeader.

    METHODS read FOR READ
      IMPORTING it_purchase_doc FOR READ PurchaseHeader RESULT result.

    METHODS set_status_close FOR MODIFY IMPORTING it_purchase_header FOR ACTION PurchaseHeader~set_status_close
       RESULT et_purchase_data.

    METHODS create_header_item FOR MODIFY
      IMPORTING it_header_item FOR CREATE PurchaseHeader\_PurchaseItems.




ENDCLASS.

CLASS lhc_ZI_RISHI_PURCHASEDOC_U IMPLEMENTATION.

  METHOD create_purchaseorder.

    DATA: lt_messages TYPE bapirettab.
    DATA: lt_purchase_buffer TYPE TABLE OF zi_rishi_purchasedoc_u.
    DATA: ls_buffer TYPE zrishi_podoc.

    DATA(lv_cid) = it_purchase_create[ 1 ]-%cid.
    "Step1: Prepare Buffer table for purchase document based on the input data.
    "Prepare Buffer table
    CALL FUNCTION 'ZRISHI_PREPARE_PURCHASEDOC'
      EXPORTING
        it_purchase_docs = it_purchase_create
      IMPORTING
        et_purchase_docs = lt_purchase_buffer
        et_messages      = lt_messages.
    "Step 2: Message Handling.
    IF NOT line_exists( lt_messages[ type = 'E' ] ).
      DATA(lv_purchase_doc) = lt_purchase_buffer[ 1 ]-PurchaseDocument.
      "Step 3: Buffer Insert
      "mapping CDS View data to DB table.
      lcl_trbuffer=>mt_create_purchasedoc = CORRESPONDING #( lt_purchase_buffer MAPPING FROM ENTITY ).
      APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchase_doc ) TO mapped-purchaseheader.
      DATA(lref_message) = NEW cl_abap_behv( )->new_message(
                                                       id       =  'ZRISHI_MSG'
                                                       number   = '001'
                                                       severity = if_abap_behv_message=>severity-success ).
      APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchase_doc  %msg = lref_message ) TO reported-purchaseheader.

    ELSE.
      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<lfs_msg>).
        APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchase_doc ) TO failed-purchaseheader.

        lref_message = NEW cl_abap_behv( )->new_message(
                                                         id       =  <lfs_msg>-id
                                                         number   = <lfs_msg>-number
                                                         severity = CONV #( <lfs_msg>-type )
                                                         v1 = <lfs_msg>-message_v1 ).
        APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchase_doc ) TO reported-purchaseheader.
      ENDLOOP.
    ENDIF.



  ENDMETHOD.

  METHOD delete_purchaseorder.
    IF it_delete_po IS NOT INITIAL.
*Get purchase doc number to be deleted.(single instance)
      LOOP AT it_delete_po ASSIGNING FIELD-SYMBOL(<lfs_delete>).
        DATA(lv_purchasedoc) = <lfs_delete>-PurchaseDocument.
        DATA(lv_cid) = <lfs_delete>-%cid_ref.
      ENDLOOP.
      APPEND VALUE #(  po_document = lv_purchasedoc ) TO lcl_trbuffer=>mt_delete_purchasedoc.
      APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchasedoc ) TO mapped-purchaseheader.
      DATA(lref_message) = NEW cl_abap_behv( )->new_message(
                                                       id       =  'ZRISHI_MSG'
                                                       number   = '003'
                                                       severity = if_abap_behv_message=>severity-success
                                                       v1 = lv_purchasedoc ).
      APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchasedoc  %msg = lref_message ) TO reported-purchaseheader.

    ENDIF.
  ENDMETHOD.

  METHOD update_purchaseorder.

    DATA: lt_update_purchase TYPE zrsh_if_rap_batch1=>tt_db_purchase.
    DATA: lt_purchase_control TYPE  zrsh_if_rap_batch1=>tt_purchase_control.

    DATA: lt_messages TYPE bapirettab.
    DATA: lt_update_purchase_final TYPE TABLE OF zrishi_podoc.
    DATA: ls_purchase_control TYPE zrsh_if_rap_batch1=>ts_purchase_control,
          ls_purchase_update  TYPE zrishi_podoc.


    LOOP AT it_update_po ASSIGNING FIELD-SYMBOL(<lfs_update_po>).
      DATA(lv_cid) = <lfs_update_po>-%cid_ref.
      ls_purchase_control-purchasedocument = <lfs_update_po>-PurchaseDocument.
      ls_purchase_control-purchasedesc = xsdbool( <lfs_update_po>-%control-PurchaseDesc = cl_abap_behv=>flag_changed ).
      ls_purchase_control-currency = xsdbool( <lfs_update_po>-%control-Currency =  cl_abap_behv=>flag_changed ).
      ls_purchase_control-purchaseprio = xsdbool( <lfs_update_po>-%control-PurchasePrio = cl_abap_behv=>flag_changed ).
      ls_purchase_control-purchasestatus = xsdbool( <lfs_update_po>-%control-PurchaseStatus = cl_abap_behv=>flag_changed ).

      ls_purchase_update = CORRESPONDING #( <lfs_update_po> MAPPING FROM ENTITY ).
      APPEND ls_purchase_update TO lt_update_purchase.
      APPEND ls_purchase_control TO lt_purchase_control.

    ENDLOOP.


    CALL FUNCTION 'ZRISHI_PURCHASE_UPDATE'
      EXPORTING
        it_purchase_doc     = lt_update_purchase
        it_purchase_control = lt_purchase_control
      IMPORTING
        et_purchase_update  = lcl_trbuffer=>mt_update_purchasedoc
        et_messages         = lt_messages.
    APPEND VALUE #( purchasedocument = ls_purchase_update-po_document  ) TO mapped-purchaseheader.

    DATA(lref_message) = NEW cl_abap_behv( )->new_message(
                                                     id       =  'ZRISHI_MSG'
                                                     number   = '007'
                                                     severity = if_abap_behv_message=>severity-success
                                                     v1 =  ls_purchase_update-po_document ).
    APPEND VALUE #(
                     purchasedocument =  ls_purchase_update-po_document
                      %msg = lref_message ) TO reported-purchaseheader.
  ENDMETHOD.

  METHOD read.

  DATA(lv_purchase) = it_purchase_doc[ 1 ]-PurchaseDocument.
  SELECT * FROM zrishi_podoc INTO TABLE @DATA(lt_purchase_doc) WHERE po_document = @lv_purchase.
  IF sy-subrc EQ 0.
  LOOP AT lt_purchase_doc ASSIGNING FIELD-SYMBOL(<lfs_po>).

  APPEND VALUE #(  purchasedocument  = <lfs_po>-po_document
 purchasedesc  = <lfs_po>-po_desc
 purchasestatus   = <lfs_po>-po_status
 purchaseprio  = <lfs_po>-po_desc
 createdby    = <lfs_po>-create_by
 createdon    = <lfs_po>-changed_date_time
 changedby   = <lfs_po>-changed_date_time
  ) TO result.
  ENDLOOP.
  ENDIF.
  ENDMETHOD.

  METHOD create_header_item.
    DATA: lv_item_no TYPE zpo_item.

    DATA(lv_purchasedoc) = it_header_item[ 1 ]-PurchaseDocument.

    DATA: ls_purchase_items TYPE zrishi_poitem.

    "Fetch existing max item no.
    SELECT MAX( po_item ) FROM zrishi_poitem
    WHERE po_document EQ @lv_purchasedoc
    INTO @lv_item_no.
    GET TIME STAMP FIELD DATA(lv_change_dtime).

    LOOP AT it_header_item ASSIGNING FIELD-SYMBOL(<lfs_header_item>).
      LOOP AT <lfs_header_item>-%target ASSIGNING FIELD-SYMBOL(<lfs_items>).
        DATA(lv_cid) = <lfs_items>-%cid.
        lv_item_no = lv_item_no + 10.
        ls_purchase_items = CORRESPONDING #(  <lfs_items> MAPPING FROM ENTITY ).
        ls_purchase_items-po_item = lv_item_no.
        ls_purchase_items-change_date_time = lv_change_dtime.
        ls_purchase_items-po_document = <lfs_header_item>-PurchaseDocument.
        APPEND ls_purchase_items TO lcl_trbuffer=>mt_create_po_items.

      ENDLOOP.

    ENDLOOP.
    "Message Handling as exercise.
    APPEND VALUE #( %cid = lv_cid purchasedocument = lv_purchasedoc PurchaseItem = lv_item_no ) TO mapped-purchaseitems.
    DATA(lref_message) = NEW cl_abap_behv( )->new_message(
                                                     id       =  'ZRISHI_MSG'
                                                     number   = '006'
                                                     severity = if_abap_behv_message=>severity-success
                                                     v1 = lv_purchasedoc ).
    APPEND VALUE #( %cid = lv_cid
                     purchasedocument = lv_purchasedoc
                      %msg = lref_message
                      PurchaseItem = lv_item_no ) TO reported-purchaseitems.


  ENDMETHOD.

  METHOD set_status_close.
  "The row type of the import_parameter table contains the following data:
*%CID_REF
*ID fields (here, PurchaseDocument)
DATA(lv_cid) = it_purchase_header[ 1 ]-%cid_ref.
    LOOP AT it_purchase_header ASSIGNING FIELD-SYMBOL(<lfs_header>).
    APPEND VALUE #( purchase_doc = <lfs_header>-PurchaseDocument ) TO lcl_trbuffer=>mt_po_status.
    APPEND VALUE #( %cid_ref = lv_cid
                    purchasedocument = <lfs_header>-PurchaseDocument
                 %param-purchasedocument = <lfs_header>-PurchaseDocument ) TO et_purchase_data.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_RISHI_PURCHASEDOC_U DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize          REDEFINITION.

    METHODS save              REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_RISHI_PURCHASEDOC_U IMPLEMENTATION.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD finalize.
  ENDMETHOD.

  METHOD save.

    "Purchase Document Create
    CALL FUNCTION 'ZRISHI_PURCHASE_DATA_SAVE'
      EXPORTING
        it_purchase_create = lcl_trbuffer=>mt_create_purchasedoc
        it_purchase_delete = lcl_trbuffer=>mt_delete_purchasedoc
        it_purchase_update = lcl_trbuffer=>mt_update_purchasedoc
        it_purchase_items  = lcl_trbuffer=>mt_create_po_items.

    IF lcl_trbuffer=>mt_po_status IS NOT INITIAL.
        CALL FUNCTION 'ZRISHI_SET_PO_STATUS_CLOSE'
      EXPORTING
        it_po_tab   = lcl_trbuffer=>mt_po_status.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
