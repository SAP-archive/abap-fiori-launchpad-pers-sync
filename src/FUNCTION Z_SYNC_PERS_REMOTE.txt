function Z_SYNC_PERS_REMOTE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_USER) TYPE  SY-UNAME
*"     VALUE(IV_ACTION) TYPE  I
*"     VALUE(IV_CONFIG_KEY) TYPE  WDY_CONFIG_KEY
*"     VALUE(IV_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(IV_ENVIRONMENT) TYPE  I
*"     VALUE(IV_IS_COMPONENT) TYPE  FLAG
*"     VALUE(IV_OBJECT_NAME) TYPE  WDY_COMPONENT_NAME
*"     VALUE(IV_PERS_SCOPE) TYPE  WDR_PERS_SCOPE
*"     VALUE(IV_TRANSPORT) TYPE  TRKORR
*"     VALUE(IV_CONFIG_DATA) TYPE  WDY_CONFIG_DATA
*"     VALUE(IV_SOURCE_CONFIG_KEY) TYPE  WDY_CONFIG_KEY
*"     VALUE(IV_CONF_USER) TYPE  WDY_CONF_USER
*"     VALUE(IV_CONF_USERT) TYPE  WDY_CONF_USERT
*"     VALUE(IV_CONF_USERT2) TYPE  WDY_CONF_USERT2
*"  EXCEPTIONS
*"      ERROR_OCCURRED
*"----------------------------------------------------------------------
*    Deletes personalization in target system
*    Re-creates updated personalization data in target system
*"--------------------------------------------------------------------

  data: lv_rc           type sysubrc.
  data: lr_translator   type ref to /ui2/cl_wdr_cfg_otr.
  data: ls_pers_key     type wdy_pers_key.

*Delete a Personalization...
  call method /ui2/cl_wdr_cfg_pers_utils=>delete_personalization
    exporting
      config_key = iv_config_key
      user       = iv_user
    receiving
      subrc      = lv_rc.

*Build a Translator...
  move-corresponding iv_conf_user to ls_pers_key.

  create object lr_translator
    exporting
      modification_language = sy-langu
      pers_key              = ls_pers_key
      is_pers               = abap_true.

*Store the Personalization...
  iv_conf_user-mandt = iv_conf_usert-mandt = iv_conf_usert2-mandt = sy-mandt.

  if iv_conf_usert2 is not initial.
    modify wdy_conf_usert2 from iv_conf_usert2.
  endif.

  call method /ui2/cl_wdr_cfg_pers_utils=>save_comp_cust_pers_to_db
    exporting
      conf_user  = iv_conf_user
      conf_usert = iv_conf_usert
      translator = lr_translator.

*Follow up...
  call method /ui2/cl_wdr_cfg_pers_utils=>config_changed
    exporting
      action            = iv_action
      config_key        = iv_config_key
      devclass          = iv_devclass
      environment       = iv_environment
      is_component      = iv_is_component
      object_name       = iv_object_name
      pers_scope        = iv_pers_scope
      transport         = iv_transport
      uname             = iv_user
      config_data       = iv_config_data
      source_config_key = iv_source_config_key.

commit work.

endfunction.