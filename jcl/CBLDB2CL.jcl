//DBAD03E JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//*  COBOL + DB2 PRECOMPILE AND LINKEDIT
//*
//DBPREC  EXEC DB2COBCL,
//             COPYLIB=DBAD03.COPYLIB,         <= COPYBOOK LIBRARY
//             DCLGLIB=DBAD03.COBOL.DCLGEN,    <= DCLGEN LIBRARY
//             DBRMLIB=DBAD03.DBRMLIB,         <= DBRM LIBRARY
//             LOADLIB=DBAD03.LOADLIB,         <= LOAD LIBRARY
//             SRCLIB=DBAD03.COBOL.SOURCE,     <= SOURCE LIBRARY
//             MEMBER=LSTCON                   <= SOURCE MEMBER
