       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DB2COMMT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       01  WS-EMPID           PIC X(06) VALUE '000123'.
       01  WS-NEW-SALARY      PIC S9(7)V99 COMP-3 VALUE 55000.00.
       01  WS-COMMIT-FLAG     PIC X VALUE 'Y'.  *> Set to 'N' to trigger rollback

       PROCEDURE DIVISION.

       MAIN-PARA.

           DISPLAY "Starting DB2 transaction".

           EXEC SQL
               UPDATE EMPLOYEE
               SET SALARY = :WS-NEW-SALARY
               WHERE EMPID = :WS-EMPID
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY "SQL ERROR ON UPDATE: " SQLCODE
               GO TO DB-ROLLBACK
           END-IF

           EVALUATE WS-COMMIT-FLAG
               WHEN 'Y'
                   DISPLAY "Committing transaction..."
                   EXEC SQL COMMIT END-EXEC
                   IF SQLCODE = 0
                       DISPLAY "Commit successful."
                   ELSE
                       DISPLAY "Commit failed. SQLCODE: " SQLCODE
                   END-IF
               WHEN OTHER
                   GO TO DB-ROLLBACK
           END-EVALUATE

           GOBACK.

       DB-ROLLBACK.

           DISPLAY "Rolling back transaction..."
           EXEC SQL ROLLBACK END-EXEC
           IF SQLCODE = 0
               DISPLAY "Rollback successful."
           ELSE
               DISPLAY "Rollback failed. SQLCODE: " SQLCODE
           END-IF

           GOBACK.
