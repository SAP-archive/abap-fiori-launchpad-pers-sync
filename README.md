# Purpose
Today, there is no built-in functionality to syncronize any SAP Fiori launchpad user personalization across multiple distributed Fiori frontend servers. This typically is an issue in larger system landscapes:
* Mirrored Fiori Frontend Server within one region for high availability
* Global Landscape Setup with different Fiori Frontend Server per region for high availability and best performance

This sample code offers an easy approach of closing this gap.
In SAP Fiori, one distinguishes two types of user personalization:

1. SAP Fiori launchpad personalization "UI2"
e.g. add/delete/move tile(s), add/delete group(s), ...

2. In-App personalization "LREP"
e.g. filter variants, table variants, ...

# Architecture

The architecture of the solution is depicted below. Please note, that there is a small difference between UI2 and LREP, when it comes to the transmitted data. Whereas for UI2, a full sync can be performed (means old data is purged and replaced by all existing personalization data), for LREP, only changes = delta will be taken into account.

<IMG>

# Prerequisites
- Either
  - two or more identical Fiori Frontend Server (same software stack, base customizing, roles, user etc.)
  - or two or more identical clients on same server/system, both fed by same DEV/QAS system(s)!
- Trusted RFC connection
- NW ABAP, SAP_UI >= 7.51

# Deployment

## RFC Connection (SM59)
Create trusted RFC connection for current user, as depicted below:

<IMG>

## Roles (PFCG)
As the solution requires trusted RFC communication, respective roles on target / destinations servers need to be created.
Auth. Objects S_RFC (Source) / S_RFCACL (Destination)

## Customizing Table (SE11/SM30)
Name e.g. ZYSYNCFLAG
Type/Delivery Class: Transparent Table / "C" (Cust. Table), Display/Maintenance Allowed
Data Class: APPL0
Size Category: 0
Transparent Table
Storage Type: Column Store
Customizing (SM30) View: Create view. In SE11, use menu "Utilities" -> "Table Maintenance Generator"

## Function Groups (SE37)
### Z_SYNC_PERS (UI2)
### Z_SYNC_PERS_REMOTE (UI2)
### Z_SYNC_PERS_LREP (LREP)
### Z_SYNC_PERS_LREP_REMOTE (LREP)

## Enhancement Spots (SE80)

### UI2
/ui2/cl_wdr_cfg_pers_utils
=>config_changed

### LREP
/UIF/CL_LREP_REST_FILE_RES
=> DELETE_CONTENT_AND_FILL_RESP
=> DO_WRITE_CONTENT_AND_FILL_RESP


# Testing

- Assign roles to user(s)
- Perform personalization on source system.
- Check destination system
- Check Application Log, transaction SLG1 - Object "/UI2/BE", Sub-Object "/UI2/LAUNCHPAD"

# FAQ

* What are the performance implications?

* Which system serves as master?

* What happens, if any of the destination server(s) are down? How to restore personalization once systems are up again?
  - UI2
  - LREP

* What to keep in mind during system upgrades?
