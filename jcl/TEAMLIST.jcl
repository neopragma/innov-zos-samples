//MATEDGT JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//*  RUN THE SAMPLE TEAMLIST PROGRAM
//*
//LIST     EXEC PGM=IKJEFT01,
//             DYNAMNBR=20,REGION=4096K
//STEPLIB  DD  DISP=SHR,DSN=DSNA10.DBAG.SDSNEXIT
//         DD  DISP=SHR,DSN=DSNA10.SDSNLOAD
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SYSIN    DD  *
MORTICIANS
SAILORS
REVENANT ARACHNOIDS
NECROMANCERS
/*
//SYSTSIN  DD  *
DSN SYSTEM (DBAG   )
  RUN PROGRAM   (TEAMLIST)  -
      PLAN      (MATEDGA ) -
      LIBRARY   ('MATEDG.LOADLIB')
END
/*
