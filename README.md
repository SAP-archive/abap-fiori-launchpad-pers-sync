# Purpose
Sample code to synchronize SAP Fiori launchpad user personalization among different Fiori Frontend Servers.
We hereby distinguish two types of personalization

1) SAP Fiori launchpad personalization "UI2"
e.g. add/delete/move tile(s), add/delete group(s), ...

2) In-App personalization "LREP"
e.g. filter variants, table variants, ...

# Motivation
xxx

Usecases:
* Mirrored Fiori Frontend Server within one region for high availability
* Global Landscape Setup with different Fiori Frontend Server per region for high availability and best performance

# Prerequisites
* Either
** two or more identical Fiori Frontend Server (same software stack, base customizing, roles, user etc.)
or
** two or more identical clients on same server/system
tyically fed by same DEV/QAS system(s)!
* Trusted RFC connection
* NW ABAP, SAP_UI >= 7.51

# Deployment

RFC Connection (SM59)
sm59 trusted RFC, Current User

Role (PFCG)
S_RFC / S_RFCACL

Customizing Table ZSYNCFLAG (SE11/SM30)
Type/Delivery Class: Transparent Table / "C" (Cust. Table), Display/Maintenance Allowed
Data Class: APPL0
Size Category: 0
Transparent Table
Storage Type: Column Store
Create SM30 view -> "Utilities" -> "Table Maintenance Generator"

Function Groups (SE37)
Z_SYNC_PERS (UI2)
Z_SYNC_PERS_REMOTE (UI2)
Z_SYNC_PERS_LREP (LREP)
Z_SYNC_PERS_LREP_REMOTE (LREP)

Enhancement Spots (SE80)

UI2
/ui2/cl_wdr_cfg_pers_utils
=>config_changed

LREP
/UIF/CL_LREP_REST_FILE_RES
=> DELETE_CONTENT_AND_FILL_RESP
=> DO_WRITE_CONTENT_AND_FILL_RESP


# Testing

Application Log
SLG1
"/UI2/BE"
"/UI2/LAUNCHPAD"

# FAQ

* What are the performance implications?

* Which system serves as master?

* What happens, if any of the destination server(s) are down? How to restore personalization once systems are up again?

UI2
LREP

* What to keep in mind during system upgrades?
