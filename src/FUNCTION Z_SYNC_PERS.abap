FUNCTION Z_SYNC_PERS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ACTION) TYPE  I
*"     REFERENCE(CONFIG_KEY) TYPE  WDY_CONFIG_KEY
*"     REFERENCE(DEVCLASS) TYPE  DEVCLASS
*"     REFERENCE(ENVIRONMENT) TYPE  I
*"     VALUE(IS_COMPONENT) TYPE  FLAG
*"     REFERENCE(OBJECT_NAME) TYPE  WDY_COMPONENT_NAME
*"     REFERENCE(PERS_SCOPE) TYPE  WDR_PERS_SCOPE
*"     REFERENCE(TRANSPORT) TYPE  TRKORR
*"     REFERENCE(UNAME) TYPE  SY-UNAME
*"     REFERENCE(CONFIG_DATA) TYPE  WDY_CONFIG_DATA
*"     REFERENCE(SOURCE_CONFIG_KEY) TYPE  WDY_CONFIG_KEY
*"----------------------------------------------------------------------

    DATA: lv_destination TYPE rfcdest.
    DATA: lv_sync_cust TYPE zsyncflag.
    DATA: lv_log_sys     TYPE logsys.
    DATA: lv_conf_user   TYPE wdy_conf_user.
    DATA: lv_conf_usert  TYPE wdy_conf_usert.
    DATA: lv_conf_usert2  TYPE wdy_conf_usert2.
    DATA: lv_sync_check  TYPE boolean.
    DATA: lv_caller_client TYPE RFCDISPLAY-RFCCLIENT.
    DATA: lv_caller_sysid TYPE SY-SYSID.
    DATA: lv_logical_system TYPE LOGSYS.

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
        CONCATENATE 'FLP Personalization for <' uname '>' INTO my_message1.
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

IF ( pers_scope = /ui2/if_wd_personalization=>co_scope_user OR pers_scope = /ui2/if_wd_personalization=>co_scope_all ) AND sy-subrc <> 0.

*   Retrieve RFC Destination from Logsys
*    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
*      IMPORTING
*        own_logical_system             = lv_log_sys
*      EXCEPTIONS
*        own_logical_system_not_defined = 1
*        OTHERS                         = 2.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.

    lv_sync_check = ABAP_FALSE.

*         Get Logsys...
    call function 'OWN_LOGICAL_SYSTEM_GET'
     IMPORTING
       OWN_LOGICAL_SYSTEM                   = lv_logical_system
*     EXCEPTIONS
*       OWN_LOGICAL_SYSTEM_NOT_DEFINED       = 1
*       OTHERS                               = 2
              .
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

*         Get Sync Flag...
    SELECT SINGLE * INTO lv_sync_cust FROM zsyncflag WHERE source = lv_logical_system.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    lv_destination = lv_sync_cust-destination.
    lv_sync_check = lv_sync_cust-sync.

    IF lv_sync_check = ABAP_TRUE.

*         Get pers data...
          SELECT SINGLE * FROM wdy_conf_user INTO lv_conf_user
            WHERE wduser      = uname
              AND user_id     = /ui2/if_wd_cfg_constants=>c_config_user_user
              AND config_id   = config_key-config_id
              AND config_type = config_key-config_type
              AND config_var  = config_key-config_var.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          SELECT SINGLE * FROM wdy_conf_usert INTO lv_conf_usert
            WHERE wduser      = lv_conf_user-wduser
              AND user_id     = lv_conf_user-user_id
              AND config_id   = lv_conf_user-config_id
              AND config_type = lv_conf_user-config_type
              AND config_var  = lv_conf_user-config_var
              AND config_view = lv_conf_user-config_view
              AND langu       = sy-langu.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          SELECT SINGLE * FROM wdy_conf_usert2 INTO lv_conf_usert2
            WHERE wduser      = lv_conf_user-wduser
              AND user_id     = lv_conf_user-user_id
              AND config_id   = lv_conf_user-config_id
              AND config_type = lv_conf_user-config_type
              AND config_var  = lv_conf_user-config_var
              AND config_view = lv_conf_user-config_view
              AND langu       = sy-langu.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

*         Do magic...
          CALL FUNCTION 'Z_SYNC_PERS_REMOTE' DESTINATION lv_destination
            EXPORTING
              iv_user              = uname
              iv_action            = action
              iv_config_key        = config_key
              iv_devclass          = devclass
              iv_environment       = environment
              iv_is_component      = is_component
              iv_object_name       = object_name
              iv_pers_scope        = pers_scope
              iv_transport         = transport
              iv_config_data       = config_data
              iv_source_config_key = source_config_key
              iv_conf_user         = lv_conf_user
              iv_conf_usert        = lv_conf_usert
              iv_conf_usert2        = lv_conf_usert2
            EXCEPTIONS
              error_occurred       = 1
              OTHERS               = 2.
          IF sy-subrc <> 0.
*        Implement suitable error handling here
*  Log unsuccessful sync in source system
            CONCATENATE 'FLP Personalization for <' uname '>' INTO my_message1.
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
            CONCATENATE 'FLP Personalization for <' uname '>' INTO my_message1.
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