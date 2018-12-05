FUNCTION Z_SYNC_PERS_LREP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_CONTENT_ID) TYPE  /UIF/LREP_CONT_ID OPTIONAL
*"     REFERENCE(IV_PACKAGE) TYPE  DEVCLASS OPTIONAL
*"     REFERENCE(IV_TRKORR) TYPE  TRKORR OPTIONAL
*"     REFERENCE(IV_CONTENT) TYPE  /UIF/LREP_CONTENT OPTIONAL
*"     REFERENCE(IS_FILE_ID) TYPE  /UIF/LREP_FILE_ID OPTIONAL
*"     REFERENCE(IV_LAYER_TYPE) TYPE  /UIF/LREP_LAYER_TYPE OPTIONAL
*"----------------------------------------------------------------------

    DATA: lv_destination TYPE rfcdest.
    DATA: lv_sync_cust TYPE zsyncflag.
    DATA: lv_log_sys     TYPE logsys.
    DATA: lv_sync_check  TYPE boolean.
    DATA: lv_caller_client TYPE RFCDISPLAY-RFCCLIENT.
    DATA: lv_caller_sysid TYPE SY-SYSID.

    DATA:
    my_message1 TYPE STRING,
    my_message2 TYPE STRING,
    my_message3 TYPE STRING,
    lt_msg      TYPE balmi_tab,
    ls_msg      TYPE balmi.

*   Abort if this is an RFC call to avoid infinite loops
    CALL FUNCTION 'RFC_GET_ATTRIBUTES'
      IMPORTING
        CALLER_SYSTEM_ID = lv_caller_sysid
        CALLER_CLIENT = lv_caller_client
      EXCEPTIONS
        system_call_not_supported = 1
        no_rfc_communication      = 2
        internal_error            = 3
        OTHERS                    = 4.
    IF sy-subrc = 0.
*  Log successful sync in target system
        CONCATENATE 'FLP LREP Personalization for <' sy-uname '>' INTO my_message1.
        CONCATENATE 'successfully retrieved' '' INTO my_message2.
        CONCATENATE 'from <' lv_caller_sysid 'CLNT' lv_caller_client '>' INTO my_message3.
        ls_msg-msgty = 'I'.
        ls_msg-msgid = '00'.
        ls_msg-msgno = '398'.
        ls_msg-msgv1 = my_message1.
        ls_msg-msgv2 = my_message2.
        ls_msg-msgv3 = my_message3.
        APPEND ls_msg TO lt_msg.
*EXIT.
    ENDIF.

IF sy-subrc <> 0.

    lv_sync_check = ABAP_FALSE.

    SELECT SINGLE * INTO lv_sync_cust FROM zsyncflag.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    lv_destination = lv_sync_cust-destination.
    lv_sync_check = lv_sync_cust-sync.

    IF lv_sync_check = ABAP_TRUE.

*         Do magic...
          call function 'Z_SYNC_PERS_LREP_REMOTE' DESTINATION lv_destination
            exporting
              is_content_id        =  is_content_id
              iv_package           =  iv_package
              iv_trkorr            =  iv_trkorr
              iv_content           =  iv_content
              is_file_id           =  is_file_id
              iv_layer_type        =  iv_layer_type
           EXCEPTIONS
              ERROR_OCCURRED       = 1
              OTHERS               = 2.
          IF sy-subrc <> 0.
*        Implement suitable error handling here
*  Log unsuccessful sync in source system
            CONCATENATE 'FLP LREP Personalization for <' sy-uname '>' INTO my_message1.
            CONCATENATE 'could not be syncronized' '' INTO my_message2.
            CONCATENATE 'to <' lv_destination '>' INTO my_message3.
            ls_msg-msgty = 'E'.
            ls_msg-msgid = '00'.
            ls_msg-msgno = '398'.
            ls_msg-msgv1 = my_message1.
            ls_msg-msgv2 = my_message2.
            ls_msg-msgv3 = my_message3.
          ELSE.
*         Implement suitable logging here
*  Log successful sync in source system
            CONCATENATE 'FLP LREP Personalization for <' sy-uname '>' INTO my_message1.
            CONCATENATE 'successfully syncronized' '' INTO my_message2.
            CONCATENATE 'to <' lv_destination '>' INTO my_message3.
            ls_msg-msgty = 'I'.
            ls_msg-msgid = '00'.
            ls_msg-msgno = '398'.
            ls_msg-msgv1 = my_message1.
            ls_msg-msgv2 = my_message2.
            ls_msg-msgv3 = my_message3.
          ENDIF.
          APPEND ls_msg TO lt_msg.

    ENDIF.
ENDIF.

*&---------------------------------------------------------------------*
*& These code snippets will generate the Application log = output only
*&---------------------------------------------------------------------*

IF lt_msg IS NOT INITIAL.

DATA: lf_obj        TYPE balobj_d,
      lf_subobj     TYPE balsubobj,
      ls_header     TYPE balhdri,
      lf_log_handle TYPE balloghndl,
      lf_log_number TYPE balognr,
      lt_lognum     TYPE TABLE OF balnri,
      ls_lognum     TYPE balnri.

* Application Log object & Subobject
  lf_obj     = '/UI2/BE'.
  lf_subobj  = '/UI2/LAUNCHPAD'.

* Header information for the log
  ls_header-object     = lf_obj.
  ls_header-subobject  = lf_subobj.
  ls_header-aldate     = sy-datum.
  ls_header-altime     = sy-uzeit.
  ls_header-aluser     = sy-uname.
  ls_header-aldate_del = sy-datum + 1.

* Get the Log handle using the header
  CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
    EXPORTING
      header              = ls_header
    IMPORTING
      e_log_handle        = lf_log_handle
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      error               = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Get the next avaliable Log number
  CALL FUNCTION 'BAL_DB_LOGNUMBER_GET'
    EXPORTING
      i_client                 = sy-mandt
      i_log_handle             = lf_log_handle
    IMPORTING
      e_lognumber              = lf_log_number
    EXCEPTIONS
      log_not_found            = 1
      lognumber_already_exists = 2
      numbering_error          = 3
      OTHERS                   = 4.

* Write the Log mesages to the memory
  CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
    EXPORTING
      object              = lf_obj
      subobject           = lf_subobj
      log_handle          = lf_log_handle
    TABLES
      messages            = lt_msg
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* write the log message to Database which can be later analyzed from transaction SLG1

  MOVE-CORRESPONDING ls_header TO ls_lognum.
  ls_lognum-lognumber = lf_log_number.
  APPEND ls_lognum TO lt_lognum.

  CALL FUNCTION 'APPL_LOG_WRITE_DB'
    EXPORTING
      object                = lf_obj
      subobject             = lf_subobj
      log_handle            = lf_log_handle
    TABLES
      object_with_lognumber = lt_lognum
    EXCEPTIONS
      object_not_found      = 1
      subobject_not_found   = 2
      internal_error        = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDIF.

ENDFUNCTION.