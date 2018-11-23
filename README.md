# Purpose
Sample code to synchronize SAP Fiori launchpad user personalization among different Fiori Frontend Servers.
We hereby distinguish two types of personalization

1) SAP Fiori launchpad personalization -> UI2
e.g. add/delete/move tile(s), add/delete group(s), ...

2) In-App personalization -> LREP
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
* Trusted RFC connection
* NW ABAP, SAP_UI >= 7.51

# Deployment

RFC Connection
sm59 trusted RFC, Current User

Role
S_RFC / S_RFCACL

Customizing Table

Function Groups
Z_SYNC_PERS (UI2)
Z_SYNC_PERS_REMOTE (UI2)
Z_SYNC_PERS_LREP (LREP)
Z_SYNC_PERS_LREP_REMOTE (LREP)

Enhancement Spots
UI2

LREP

# Testing

Application Log
