![](https://img.shields.io/badge/STATUS-NOT%20CURRENTLY%20MAINTAINED-red.svg?longCache=true&style=flat)

# Important Notice
This public repository is read-only and no longer maintained.

# Description
If you are operating multiple Fiori Frontend servers for high availability or failover reasons and have issues around the consistency of user personalization for the SAP Fiori launchpad, you can use this described technique and sample coding to effectively syncronize launchpad as well as in-app personalization configurations across your landscape.

Today, there is no built-in functionality to syncronize any SAP Fiori launchpad user personalization across multiple distributed Fiori frontend servers. This typically is an issue in larger system landscapes:
* Mirrored Fiori Frontend Server within one region for high availability
* Global Landscape Setup with different Fiori Frontend Server per region for high availability and best performance

This sample code offers an easy approach of closing this gap.

### Personalization

In SAP Fiori, one distinguishes two types of user personalization:

**SAP Fiori launchpad personalization "UI2"**
> e.g. add/delete/move tile(s), add/delete group(s), ...

**In-App personalization "LREP"** 
> e.g. filter variants, table variants, ...

# Demo

Check out this short [demo video](https://sapvideoa35699dc5.hana.ondemand.com/?entry_id=0_dhbsyiqd) on the provided functionality. In this example, two different clients in a single system are used.

# Requirements
* **SAP Netweaver ABAP >= 7.51 incl. SAP_UI** environment e.g. tested on [SAP S/4HANA 1709/1809](https://blogs.sap.com/?p=745947)
- Either
  - two or more **identical** Fiori Frontend Server
  - or two or more **identical** clients on same server/system
- **identical** means = same software stack, base customizing, roles, user etc., typically both fed by same DEV/QAS system(s)!
- Trusted RFC connections ("current user")
- [Logical System](https://help.sap.com/viewer/61af834e09164854993e81aa39be576d/1809.000/en-US/b4b0b13bb3acef3ce10000000a11402f.html) maintained in each source system / client

# Architecture

The architecture of the solution is depicted below. Please note, that there is a small difference between UI2 and LREP, when it comes to the transmitted data. Whereas for UI2, a full sync can be performed (means old data is purged and replaced by all existing personalization data), for LREP, only changes = delta will be taken into account. See also section FAQ.

## Overview

![architecture](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/AR_SUMMARY.png)

## SAP Fiori launchpad personalization "UI2"

![architecture_ui2](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/AR_UI2.png)

## In-App personalization "LREP"

![architecture_lrep](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/AR_LREP.png)

# Download & Installation

It is actually not necessary to download any files, you can easily copy and paste the required code fragments directly, as mentioned below. If you still like to have a local copy, you can follow this [guide](https://help.github.com/articles/cloning-a-repository/).

### RFC Connection (SM59)
Create a trusted RFC connection with setting "current user" in each source system, as depicted below:

![SM59](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/SM59.png)

### Roles (PFCG)
As the solution requires trusted RFC communication, respective roles on source & destinations servers need to be created.
Auth. Objects S_RFC (Source) / S_RFCACL (Destination)

![PFCG](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/PFCG.png)

### Customizing Table (SE11/SM30)
Create a cross-client customizing table as follows

Name: e.g. ZYSYNCFLAG  
Type/Delivery Class: Transparent Table / "C" (Cust. Table), Display/Maintenance Allowed  
Data Class: APPL0  
Size Category: 0   
Storage Type: Column Store  

Create customizing view: In SE11 for the given table ZYSYNCFLAG, use menu "Utilities" -> "Table Maintenance Generator"  

![ZSYNCFLAG](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/ZSYNCFLAG.png)

![ZSYNCFLAG MAINTENANCE](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/ZSYNCFLAG_MAIN.png)

### Function Groups (SE37)

Create a function group e.g. Z_FLP_SYNC and the following function modules below.

#### Z_SYNC_PERS (UI2)

Purpose: Fetches UI2 personalization data and performs RFC call to destination system

Flow: 
- Triggered in the UI2 Enhancement Spot (see below)
- Checks if local call (If RFC call = destination system, we have to stop processing to avoid infinite loops!)
- Reads current logical system
- Checks if sync enabled
- Fetches UI2 data for the given uer
- Performs RFC call Z_SYNC_PERS_REMOTE

[Copy & paste code](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/src/FUNCTION%20Z_SYNC_PERS.abap)

![Z_SYNC_PERS](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/Z_SYNC_PERS.png)

#### Z_SYNC_PERS_REMOTE (UI2)

Purpose: Stores UI2 personalization data in destination system

Flow: 
- Triggered via RFC call
- Purges all personalization data for given user
- Stores all personalization data for given user
- Triggers UI2 API

[Copy & paste code](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/src/FUNCTION%20Z_SYNC_PERS_REMOTE.abap)

![Z_SYNC_PERS_REMOTE](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/Z_SYNC_PERS_REMOTE.png)

### Z_SYNC_PERS_LREP (LREP)

Purpose: Fetches LREP personalization data and performs RFC call to destination system

Flow: 
- Triggered in the LREP Enhancement Spot (see below)
- Checks if local call (If RFC call = destination system, we have to stop processing to avoid infinite loops!)
- Reads current logical system
- Checks if sync enabled
- Fetches LREP data for the given uer
- Performs RFC call Z_SYNC_PERS_LREP_REMOTE

[Copy & paste code](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/src/FUNCTION%20Z_SYNC_PERS_LREP.abap)

![Z_SYNC_PERS_LREP](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/Z_SYNC_PERS_LREP.png)

#### Z_SYNC_PERS_LREP_REMOTE (LREP)

Purpose: Stores/Purges LREP personalization data in destination system

Flow: 
- Triggered via RFC call
- Upserts/Purges given personalization data entry for given user
- Triggers LREP API

[Copy & paste code](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/src/FUNCTION%20Z_SYNC_PERS_LREP_REMOTE.abap)

![Z_SYNC_PERS_LREP_REMOTE](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/Z_SYNC_PERS_LREP_REMOTE.png)

### Enhancement Spots (SE80)

#### UI2
/ui2/cl_wdr_cfg_pers_utils  
=>config_changed  

![enhancement_ui2](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/ENH_UI2.png)

#### LREP
/UIF/CL_LREP_REST_FILE_RES  
=> DELETE_CONTENT_AND_FILL_RESP  
=> DO_WRITE_CONTENT_AND_FILL_RESP  

![enhancement_lrep1](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/ENH_LREP1.png)

![enhancement_lrep2](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/ENH_LREP2.png)

# Testing

- Maintain customizing table ZSYNCFLAG via SM30
- Assign roles to user(s)
- Perform SAP Fiori launchpad personalization on source system
- Check SAP Fiori launchpad personalization on destination system
- Check Application Log, transaction SLG1 - Object "/UI2/BE", Sub-Object "/UI2/LAUNCHPAD"

![SLG1](https://github.com/SAP/abap-fiori-launchpad-pers-sync/blob/master/docs/img/SLG1_OUTPUT.png)

# FAQ

* What are the performance implications?

The proposed coding is light-weight. Also, the frequency of SAP Fiori launchpad user personalization is expected to be low. Nevertheless, if you expect very high usage of personalization, the respective load of the additional RFC calls should be monitored and, in worst case, system should be sized accordingly.

* Which system serves as master?

There is no master system by definition. For a given user, the master will be the system, where the last personalization occured.

* What happens, if any of the destination server(s) are down? How to ensure consistency? How to restore personalization once systems are up again?

For reliable failover handling, switching to an asyncronous processing would make sense. This could for instance be done via queued RFCs or a temporary persistance for outbound processing via batch job. Nevertheless, there should be a mechanism to empty the queue before allowing users entering the restored system.
Note, whereas for UI2, we can easily always do a full sync, which ensures consistency, LREP data needs to be processed in sequence one by one!

* What to keep in mind during system upgrades?

During an upgrade/maintenance window, it is recommended to disable SAP Fiori launchpad user personalization, to ensure data consistency. This can be done via [launchpad parameters](https://blogs.sap.com/2018/08/14/options-to-disable-fiori-personalization/).
  
# SUPPORT

This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

# License

Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.

The content of this repository is licensed under the SAP SAMPLE CODE LICENSE AGREEMENT as noted in the [LICENSE](https://github.com/SAP/abap-odata-smoke-test/blob/master/LICENSE) file.
