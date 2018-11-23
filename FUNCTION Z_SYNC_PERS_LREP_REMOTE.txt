function Z_SYNC_PERS_LREP_REMOTE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_CONTENT_ID) TYPE  /UIF/LREP_CONT_ID OPTIONAL
*"     VALUE(IV_PACKAGE) TYPE  DEVCLASS OPTIONAL
*"     VALUE(IV_TRKORR) TYPE  TRKORR OPTIONAL
*"     VALUE(IV_CONTENT) TYPE  /UIF/LREP_CONTENT OPTIONAL
*"     VALUE(IS_FILE_ID) TYPE  /UIF/LREP_FILE_ID OPTIONAL
*"     VALUE(IV_LAYER_TYPE) TYPE  /UIF/LREP_LAYER_TYPE OPTIONAL
*"  EXCEPTIONS
*"      ERROR_OCCURRED
*"----------------------------------------------------------------------
*    Deletes or creates/updates LREP personalization in target system
*"--------------------------------------------------------------------

  data: lv_result_content type /uif/lrep_content.
  data: eo_lrep_api type ref to /uif/cl_lrep_api.

  create object eo_lrep_api type /uif/cl_lrep_api.

  IF IS_FILE_ID IS NOT INITIAL.
*DELETE
    eo_lrep_api->delete_content(
    exporting
      is_file_id            = is_file_id
      iv_layer_type         = iv_layer_type
      iv_trkorr             = iv_trkorr
      iv_synchronous_update = abap_true ).

  ELSE.
*CREATE, UPDATE
    eo_lrep_api->write_content(
        exporting
          is_cont_id            = is_content_id
          iv_package            = iv_package
          iv_trkorr             = iv_trkorr
          iv_content            = iv_content
          iv_synchronous_update = abap_true
        importing
          ev_content            = lv_result_content ).

  ENDIF.


  eo_lrep_api->commit_work( ).


endfunction.