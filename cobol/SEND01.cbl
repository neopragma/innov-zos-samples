       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SEND01.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT.
         05 WS-TRAN-ID       PIC X(4).
         05 WS-MESSAGE-I     PIC X(70).

       01 WS-OUTPUT.
         05 WS-TEXT          PIC X(8).
         05 WS-MESSAGE-O     PIC X(70).

       01 WS-MSG-LENGTH      PIC 9(4) COMP.
      *
       PROCEDURE DIVISION.
           MOVE 74     TO WS-MSG-LENGTH.
           EXEC CICS RECEIVE
                INTO (WS-INPUT)
                LENGTH (WS-MSG-LENGTH)
           END-EXEC.
      *
           MOVE WS-MESSAGE-I    TO  WS-MESSAGE-O.
           MOVE 'OUTPUT: '      TO  WS-TEXT.
           MOVE 78              TO  WS-MSG-LENGTH.
           EXEC CICS SEND
              FROM(WS-OUTPUT)
              LENGTH(WS-MSG-LENGTH)
              ERASE
           END-EXEC.
      *
           EXEC CICS
              RETURN
           END-EXEC.
      *
           GOBACK.
