//DBAD03K JOB (123),'KSDS',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//DEFLOAD  EXEC PGM=IDCAMS
//INDATA   DD *
0001AAAAAAAAAABBBBBCCCCCCCCCCCCCCCDDDDDDEEEEE
0002FFFFFFFFFFGGGGGHHHHHHHHHHHHHHHIIIIIIJJJJJ
0003KKKKKKKKKKLLLLLMMMMMMMMMMMMMMMNNNNNNOOOOO
/*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD  *
  DELETE DBAD03.TEST.KSDS.CLUSTER CLUSTER PURGE
  IF LASTCC < 9 THEN
    DEFINE CLUSTER(NAME(DBAD03.TEST.KSDS.CLUSTER)  -
    RECORDSIZE(80,80)    -
    CYLINDERS(2,1)       -
    FREESPACE(10,20)     -
    KEYS(4,0)            -
    CISZ(4096)           -
    VOLUMES(DBADM1)      -
    INDEXED)             -
    INDEX(NAME(DBAD03.TEST.KSDS.INDEX)) -
    DATA(NAME(DBAD03.TEST.KSDS.DATA))
    IF LASTCC = 0 THEN
      REPRO INFILE(INDATA) -
            OUTDATASET(DBAD03.TEST.KSDS.CLUSTER)
/*