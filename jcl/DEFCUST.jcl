//MATEDGI  JOB (ACCT),'DEFINE CUSTOMER KSDS',
//             CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* JOB TO DELETE AND DEFINE VSAM KSDS FOR CUSTOMER FILE
//* BASED ON RECORD LAYOUT FROM CICS/KSDSC2.CBL
//*
//* RECORD LAYOUT:
//*   WS-CUSTOMER-ID      PIC X(10)  - PRIMARY KEY
//*   WS-CUSTOMER-NAME    PIC X(30)
//*   WS-CUSTOMER-ADDRESS PIC X(50)
//*   WS-CUSTOMER-PHONE   PIC X(15)
//*   WS-CUSTOMER-EMAIL   PIC X(40)
//*   TOTAL RECORD LENGTH: 145 BYTES
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 /*                                              */
 /* DELETE EXISTING CUSTOMER VSAM CLUSTER       */
 /*                                              */
 DELETE MATEDG.CUSTFILE CLUSTER PURGE
 
 IF LASTCC LE 8 THEN -
   SET MAXCC = 0
 
 /*                                              */
 /* DEFINE CUSTOMER VSAM KSDS CLUSTER           */
 /*                                              */
 DEFINE CLUSTER -
   (NAME(MATEDG.CUSTFILE) -
    VOLUMES(DEVHD3) -
    RECORDSIZE(145 145) -
    KEYS(10 0) -
    FREESPACE(20 10) -
    INDEXED -
    REUSE -
    CISZ(4096) -
    UNIQUE) -
 DATA -
   (NAME(MATEDG.CUSTFILE.DATA) -
    CONTROLINTERVALSIZE(4096) -
    BUFFERSPACE(20480)) -
 INDEX -
   (NAME(MATEDG.CUSTFILE.INDEX) -
    CONTROLINTERVALSIZE(1024) -
    BUFFERSPACE(2048))
 
 /*                                              */
 /* LIST CATALOG TO VERIFY DEFINITION           */
 /*                                              */
 LISTCAT ENTRIES(MATEDG.CUSTFILE) ALL
/*
//
