       Identification Division.
       Program-Id. DATES.
      *****************************************************************
      * Sandbox for demonstrating date and time handling.
      *****************************************************************
       Data Division.
       Working-Storage Section.
       01  filler.
           05  Date-Time.
               10  DT-YYYYMMDD          pic 9(8).
               10  filler redefines DT-YYYYMMDD.
                   15  DT-Year          pic 9(4).
                   15  DT-Month         pic 9(2).
                   15  DT-Day-of-Month  pic 9(2).
               10  DT-Hour              pic 9(2).
               10  DT-Minute            pic 9(2).
               10  DT-Second            pic 9(2).
               10  DT-Hundredth         pic 9(2).
               10  filler               pic x.
                   88  Behind-GMT       value "-".
                   88  Ahead-of-GMT     value "+".
               10  DT-Hour-Offset       pic 9(2).
               10  DT-Minute-Offset     pic 9(2).   

           05  Last-Payment-Day         pic 9(7).
           05  Past-Due-Days            pic s9(7).
           05  Integer-Date-1           pic 9(7).
           05  Integer-Date-2           pic 9(7).

       Procedure Division.

      * FUNCTION CURRENT-DATE returns the system date and time 
      * in the format shown in Date-Time in Working-Storage.

           move function current-date to Date-Time
           display "Result of FUNCTION CURRENT-DATE:"
           display Date-Time
           display space

      * ACCEPT without the FROM option will read a record from
      * SYSIN into the Data Division item named on the ACCEPT
      * statement. We can use this mechanism to override the
      * system date and time for purposes of job restart or 
      * testing date-dependent logic. The COBOL program must
      * be coded in a way that is aware of this technique.
      * It does not "automatically" set the system date/time.
      * If the program contains a statement like this and no
      * SYSIN data set is provided in theJCL, the job will
      * fail with abend code U4086.
      * This is all user-defined code. By convention, it is
      * simpler to use the same layout as the CURRENT-DATE
      * function.

           accept Date-Time
           display "Result of ACCEPT Date-Time"
           display Date-Time
           display space

      * The second ACCEPT (without FROM) reads the second record
      * from the SYSIN data set.
      * Here we are picking up a Julian date. IBM's Julian date
      * format is YYYYDDD.

           accept Last-Payment-Day
           display "Result of ACCEPT Last-Payment-Day"
           display Last-Payment-Day
           display space

      * Now we use FUNCTION INTEGER-OF-DAY and FUNCTION
      * INTEGER-OF-DATE to convert the "current" date and the
      * "last payment" date into integers that represent the
      * number of days since December 31, 1600.

           compute Integer-Date-1 =
               function integer-of-date(DT-YYYYMMDD)
           end-compute
           compute Integer-Date-2 =                       
               function integer-of-day(Last-Payment-Day)
           end-compute
           subtract
               Integer-Date-1 from Integer-Date-2
               giving Past-Due-Days
           end-subtract
           evaluate true
               when Past-Due-Days > 120
                   display "120+ days past due - to Collections"
               when Past-Due-Days > 60
                   display "60-119 days past due - 15% late fee"
               when Past-Due-Days > 30
                   display "30-59 days past due - 5% late fee"
               when Past-Due-Days < 0
                   display "Early payment - apply 5% discount"
               when other
                   display "Normal payment"
           end-evaluate
           display space

      * This is an older way to get the system date. It is
      * limited compared to the CURRENT-DATE function, but you may
      * see it in older programs that you support.

           accept Date-Time from Date
           display "Result of ACCEPT Date-Time FROM DATE"
           display Date-Time
           display space

           goback
           .                                                               