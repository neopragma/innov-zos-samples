       Identification Division.
       Program-Id. BSKEL1.
      *****************************************************************
      * Batch COBOL skeleton.
      * Read sequential input, write sequential output. 
      *****************************************************************
       Environment Division.
       Input-Output Section.
       File Control.
           Select Sequential-Input-File
                  Assign to "SQINPUT"
                  Organization Sequential
                  Access Sequential
                  File Status Sequential-Input-File-Status.
           Select Sequential-Output-File
                  Assign to "SQOUTPUT"
                  Organization Sequential
                  Access Sequential
                  File Status Sequential-Output-File-Status.
       Data Division.
       File Section.
       FD  Sequential-Input-File
           Recording Mode F
           Record Contains 80 Characters 
           Block Contains 0 Records
           Data Record Sequential-Input-Record.
       01  Sequential-Input-Record           pic x(80).    
       FD  Sequential-Output-File
           Recording Mode F
           Record Contains 80 Characters 
           Block Contains 0 Records
           Data Record Sequential-Output-Record.
       01  Sequential-Output-Record          pic x(80).    
       Working-Storage Section.
       01  Work-X.
           05  filler                        pic x.
       01  External-File-Names.
           05  Sequential-Input-File-DDNAME  pic x(8) value "SQINPUT".
           05  Sequential-Output-File-DDNAME pic x(8) value "SQOUTPUT".
       01  File-Status-Indicators.
           05  Sequential-Input-File-Status  pic x(2).
               88  Sequential-Input-OK       value "00".
               88  Sequential-Input-EOF      value "10".
           05  Sequential-Output-File-Status pic x(2).
               88  Sequential-Output-OK      value "00".
       01  Error-Messages.
           05  Error-Message                 pic x(132).
           05  IO-Error-Message.
               10  filler                    pic x(11) value "Got status".
               10  Error-Status              pic x(2).
               10  filler                    pic x(4)  value " on".
               10  Error-Operation           pic x(5).
               10  filler                    pic x(4)  value " of".
               10  Error-DDNAME              pic x(8).
               10  filler                    pic x     value ".".
       01  Pseudo-Constants.
           05  Const-OPEN                    pic x(4)  value "OPEN".
           05  Const-CLOSE                   pic x(5)  value "CLOSE".
           05  Const-READ                    pic x(4)  value "READ".
           05  Const-WRITE                   pic x(5)  value "WRITE".                                     
       Procedure Division.
           perform 0000-Initialize
           perform 1000-Process
           perform 9000-Housekeeping
           goback
           .
       0000-Initialize.
           perform 0100-Open-Files    
           .
       0100-Open-Files.
           open input Sequential-Input-File
           if not Sequential-Input-OK
               move Sequential-Input-File-Status to Error-Status
               move Const-OPEN to Error-Operation 
               move Sequential-Input-File-DDNAME to Error-DDNAME
               move IO-Error-Message to Error-Message
               perform 8900-Scream-and-Die
           end-if
           open output Sequential-Output-File
           if not Sequential-Output-OK
               move Sequential-Output-File-Status to Error-Status
               move Const-OPEN to Error-Operation 
               move Sequential-Output-File-DDNAME to Error-DDNAME
               move IO-Error-Message to Error-Message
               perform 8900-Scream-and-Die
           end-if
           .
       1000-Process.
           perform 1100-Read-Next-Input-Record
           perform with test before
                   until Sequential-Input-EOF
               perform 1200-Business-Logic
               perform 1100-Read-Next-Input-Record
           end-perform
           .
       1100-Read-Next-Input-Record.
           read Sequential-Input-File
           if not Sequential-Input-OK and not Sequential-Input-EOF
               move Sequential-Input-File-Status to Error-Status
               move Const-READ to Error-Operation 
               move Sequential-Input-File-DDNAME to Error-DDNAME
               move IO-Error-Message to Error-Message
               perform 8900-Scream-and-Die
           end-if
           .
       1200-Business-Logic.
           move Sequential-Input-Record to Sequential-Output-Record
           perform 7000-Write-Output-Record
           .
       7000-Write-Output-Record.
           write Sequential-Output-Record
           if not Sequential-Output-OK
               move Sequential-Output-File-Status to Error-Status
               move Const-WRITE to Error-Operation 
               move Sequential-Output-File-DDNAME to Error-DDNAME
               move IO-Error-Message to Error-Message
               perform 8900-Scream-and-Die
           end-if
           .
       8900-Scream-and-Die.
           display Error-Message
           move 12 to return-code
           goback
           .
       9000-Housekeeping.
           close Sequential-Output-File
           close Sequential-Input-File
           .    