       IDENTIFICATION DIVISION.
       PROGRAM-ID. KSDSC2.
      *
      * CICS COBOL CRUD Program for VSAM KSDS - Template 2
      * Uses pseudoconversational design with containers and channels
      * User interaction via PF keys
      *
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       
       01  Ws-Container-Names.
           05  Ws-Data-Container          PIC X(16) VALUE 'CUSTOMER-DATA'.
           05  Ws-State-Container         PIC X(16) VALUE 'PROGRAM-STATE'.
           
       01  Ws-Channel-Name               PIC X(16) VALUE 'CRUD-CHANNEL'.
       
       01  Ws-Customer-Record.
           05  Ws-Customer-Id             PIC X(10).
           05  Ws-Customer-Name           PIC X(30).
           05  Ws-Customer-Address        PIC X(50).
           05  Ws-Customer-Phone          PIC X(15).
           05  Ws-Customer-Email          PIC X(40).
           
       01  Ws-Program-State.
           05  Ws-Current-Key             PIC X(10).
           05  Ws-Operation-Mode          PIC X(10).
           05  Ws-Record-Found-Flag       PIC X(01) VALUE 'N'.
           05  Ws-Changes-Made-Flag       PIC X(01) VALUE 'N'.
           
       01  Ws-Response-Codes.
           05  Ws-Response                PIC S9(8) COMP.
           05  Ws-Response2               PIC S9(8) COMP.
           
       01  Ws-File-Control.
           05  Ws-Ksds-File               PIC X(08) VALUE 'CUSTFILE'.
           
       01  Ws-Work-Fields.
           05  Ws-Message-Text            PIC X(78) VALUE SPACES.
           05  Ws-Record-Count            PIC 9(05) VALUE ZERO.
           05  Ws-Container-Length        PIC S9(8) COMP VALUE ZERO.
           
       COPY CRUDM.
       
       LINKAGE SECTION.
       
       PROCEDURE DIVISION.
       
       0100-Main-Processing.
           PERFORM 7100-Retrieve-Containers
           PERFORM 1100-Process-User-Action
           PERFORM 7200-Store-Containers
           PERFORM 9100-Return-To-Cics
           .
           
       1100-Process-User-Action.
           EVALUATE EIBAID
               WHEN DFHPF1
                   PERFORM 2100-Display-Help
               WHEN DFHPF2
                   PERFORM 2200-Save-Changes
               WHEN DFHPF3
                   PERFORM 2200-Save-Changes
                   PERFORM 9200-Exit-Program
               WHEN DFHPF4
                   PERFORM 2300-Add-Record
               WHEN DFHPF5
                   PERFORM 2400-Delete-Record
               WHEN DFHPF7
                   PERFORM 2500-Browse-Previous
               WHEN DFHPF8
                   PERFORM 2600-Browse-Next
               WHEN DFHPF11
                   PERFORM 2700-Cancel-Changes
               WHEN DFHPF12
                   PERFORM 9200-Exit-Program
               WHEN DFHENTER
                   PERFORM 3100-Process-Enter-Key
               WHEN OTHER
                   PERFORM 3200-Invalid-Key
           END-EVALUATE
           .
           
       2100-Display-Help.
           MOVE SPACES TO Ws-Message-Text
           STRING 'PF1=Help PF2=Save PF3=Save+Exit PF4=Add PF5=Delete '
                  'PF7=Previous PF8=Next PF11=Cancel PF12=Exit'
                  DELIMITED BY SIZE INTO Ws-Message-Text
           END-STRING
           PERFORM 5100-Send-Map
           .
           
       2200-Save-Changes.
           IF Ws-Changes-Made-Flag = 'Y'
               PERFORM 6200-Write-Customer-Record
               IF Ws-Response = DFHRESP(NORMAL)
                   MOVE 'Record saved successfully' TO Ws-Message-Text
                   MOVE 'N' TO Ws-Changes-Made-Flag
               ELSE
                   MOVE 'Error saving record' TO Ws-Message-Text
               END-IF
           ELSE
               MOVE 'No changes to save' TO Ws-Message-Text
           END-IF
           PERFORM 5100-Send-Map
           .
           
       2300-Add-Record.
           MOVE SPACES TO Ws-Customer-Record
           MOVE 'ADD' TO Ws-Operation-Mode
           MOVE 'Enter new customer data and press PF2 to save'
                TO Ws-Message-Text
           PERFORM 5100-Send-Map
           .
           
       2400-Delete-Record.
           IF Ws-Record-Found-Flag = 'Y'
               PERFORM 6400-Delete-Customer-Record
               IF Ws-Response = DFHRESP(NORMAL)
                   MOVE 'Record deleted successfully' TO Ws-Message-Text
                   MOVE SPACES TO Ws-Customer-Record
                   MOVE 'N' TO Ws-Record-Found-Flag
               ELSE
                   MOVE 'Error deleting record' TO Ws-Message-Text
               END-IF
           ELSE
               MOVE 'No record to delete' TO Ws-Message-Text
           END-IF
           PERFORM 5100-Send-Map
           .
           
       2500-Browse-Previous.
           PERFORM 6500-Start-Browse-Previous
           IF Ws-Response = DFHRESP(NORMAL)
               PERFORM 6600-Read-Previous-Record
               IF Ws-Response = DFHRESP(NORMAL)
                   MOVE 'Y' TO Ws-Record-Found-Flag
                   MOVE 'Previous record displayed' TO Ws-Message-Text
               ELSE
                   MOVE 'No previous record found' TO Ws-Message-Text
               END-IF
           ELSE
               MOVE 'Browse error' TO Ws-Message-Text
           END-IF
           PERFORM 6700-End-Browse
           PERFORM 5100-Send-Map
           .
           
       2600-Browse-Next.
           PERFORM 6800-Start-Browse-Next
           IF Ws-Response = DFHRESP(NORMAL)
               PERFORM 6900-Read-Next-Record
               IF Ws-Response = DFHRESP(NORMAL)
                   MOVE 'Y' TO Ws-Record-Found-Flag
                   MOVE 'Next record displayed' TO Ws-Message-Text
               ELSE
                   MOVE 'No next record found' TO Ws-Message-Text
               END-IF
           ELSE
               MOVE 'Browse error' TO Ws-Message-Text
           END-IF
           PERFORM 6700-End-Browse
           PERFORM 5100-Send-Map
           .
           
       2700-Cancel-Changes.
           MOVE 'N' TO Ws-Changes-Made-Flag
           IF Ws-Record-Found-Flag = 'Y'
               PERFORM 6100-Read-Customer-Record
           END-IF
           MOVE 'Changes cancelled' TO Ws-Message-Text
           PERFORM 5100-Send-Map
           .
           
       3100-Process-Enter-Key.
           IF Ws-Current-Key NOT = SPACES
               MOVE Ws-Current-Key TO Ws-Customer-Id
               PERFORM 6100-Read-Customer-Record
               IF Ws-Response = DFHRESP(NORMAL)
                   MOVE 'Y' TO Ws-Record-Found-Flag
                   MOVE 'Record found' TO Ws-Message-Text
               ELSE
                   MOVE 'N' TO Ws-Record-Found-Flag
                   MOVE 'Record not found' TO Ws-Message-Text
               END-IF
           ELSE
               MOVE 'Enter customer ID and press Enter' TO Ws-Message-Text
           END-IF
           PERFORM 5100-Send-Map
           .
           
       3200-Invalid-Key.
           MOVE 'Invalid key pressed - use PF1 for help' TO Ws-Message-Text
           PERFORM 5100-Send-Map
           .
           
       5100-Send-Map.
           MOVE LOW-VALUES TO Crudmo
           MOVE Ws-Customer-Id TO Keyo
           MOVE Ws-Customer-Name TO Nameo
           MOVE Ws-Customer-Address TO Addro
           MOVE Ws-Customer-Phone TO Phoneo
           MOVE Ws-Customer-Email TO Emailo
           MOVE Ws-Message-Text TO Msgo
           MOVE Ws-Record-Count TO Counto
           
           EXEC CICS SEND MAP('CRUDM')
                MAPSET('CRUDMS')
                ERASE
                FREEKB
           END-EXEC
           .
           
       6100-Read-Customer-Record.
           EXEC CICS READ
                FILE(Ws-Ksds-File)
                INTO(Ws-Customer-Record)
                RIDFLD(Ws-Customer-Id)
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       6200-Write-Customer-Record.
           IF Ws-Operation-Mode = 'ADD'
               EXEC CICS WRITE
                    FILE(Ws-Ksds-File)
                    FROM(Ws-Customer-Record)
                    RIDFLD(Ws-Customer-Id)
                    RESP(Ws-Response)
                    RESP2(Ws-Response2)
               END-EXEC
           ELSE
               EXEC CICS REWRITE
                    FILE(Ws-Ksds-File)
                    FROM(Ws-Customer-Record)
                    RESP(Ws-Response)
                    RESP2(Ws-Response2)
               END-EXEC
           END-IF
           .
           
       6400-Delete-Customer-Record.
           EXEC CICS DELETE
                FILE(Ws-Ksds-File)
                RIDFLD(Ws-Customer-Id)
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       6500-Start-Browse-Previous.
           EXEC CICS STARTBR
                FILE(Ws-Ksds-File)
                RIDFLD(Ws-Customer-Id)
                GTEQ
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       6600-Read-Previous-Record.
           EXEC CICS READPREV
                FILE(Ws-Ksds-File)
                INTO(Ws-Customer-Record)
                RIDFLD(Ws-Customer-Id)
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       6700-End-Browse.
           EXEC CICS ENDBR
                FILE(Ws-Ksds-File)
           END-EXEC
           .
           
       6800-Start-Browse-Next.
           EXEC CICS STARTBR
                FILE(Ws-Ksds-File)
                RIDFLD(Ws-Customer-Id)
                GTEQ
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       6900-Read-Next-Record.
           EXEC CICS READNEXT
                FILE(Ws-Ksds-File)
                INTO(Ws-Customer-Record)
                RIDFLD(Ws-Customer-Id)
                RESP(Ws-Response)
                RESP2(Ws-Response2)
           END-EXEC
           .
           
       7100-Retrieve-Containers.
           EXEC CICS GET CONTAINER(Ws-Data-Container)
                CHANNEL(Ws-Channel-Name)
                INTO(Ws-Customer-Record)
                FLENGTH(LENGTH OF Ws-Customer-Record)
                RESP(Ws-Response)
           END-EXEC
           
           IF Ws-Response NOT = DFHRESP(NORMAL)
               MOVE SPACES TO Ws-Customer-Record
           END-IF
           
           EXEC CICS GET CONTAINER(Ws-State-Container)
                CHANNEL(Ws-Channel-Name)
                INTO(Ws-Program-State)
                FLENGTH(LENGTH OF Ws-Program-State)
                RESP(Ws-Response)
           END-EXEC
           
           IF Ws-Response NOT = DFHRESP(NORMAL)
               MOVE SPACES TO Ws-Program-State
           END-IF
           
           EXEC CICS RECEIVE MAP('CRUDM')
                MAPSET('CRUDMS')
                INTO(Crudmi)
                RESP(Ws-Response)
           END-EXEC
           
           IF Ws-Response = DFHRESP(NORMAL)
               MOVE Keyi TO Ws-Current-Key
               IF Namei NOT = SPACES
                   MOVE Namei TO Ws-Customer-Name
                   MOVE 'Y' TO Ws-Changes-Made-Flag
               END-IF
               IF Addri NOT = SPACES
                   MOVE Addri TO Ws-Customer-Address
                   MOVE 'Y' TO Ws-Changes-Made-Flag
               END-IF
               IF Phonei NOT = SPACES
                   MOVE Phonei TO Ws-Customer-Phone
                   MOVE 'Y' TO Ws-Changes-Made-Flag
               END-IF
               IF Emaili NOT = SPACES
                   MOVE Emaili TO Ws-Customer-Email
                   MOVE 'Y' TO Ws-Changes-Made-Flag
               END-IF
           END-IF
           .
           
       7200-Store-Containers.
           EXEC CICS PUT CONTAINER(Ws-Data-Container)
                CHANNEL(Ws-Channel-Name)
                FROM(Ws-Customer-Record)
                FLENGTH(LENGTH OF Ws-Customer-Record)
           END-EXEC
           
           EXEC CICS PUT CONTAINER(Ws-State-Container)
                CHANNEL(Ws-Channel-Name)
                FROM(Ws-Program-State)
                FLENGTH(LENGTH OF Ws-Program-State)
           END-EXEC
           .
           
       9100-Return-To-Cics.
           EXEC CICS RETURN
                TRANSID(EIBTRNID)
                CHANNEL(Ws-Channel-Name)
           END-EXEC
           .
           
       9200-Exit-Program.
           EXEC CICS RETURN
           END-EXEC
           .
