       identification division.
       program-id. A7-CallCenterOpReport.
       author. Ahmed Butt.
       date-written. 2021-04-07.
      *Program Description: Reads data and generates a report based on 
      *that data
      *
       environment division.
       input-output section.
       file-control.
      *
           select emp-file
               assign to '../../../A7.dat'
               organization is line sequential.
      *
           select report-file
               assign to '../../../A7-CallCenterOpReport.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd emp-file
           data record is emp-rec
           record contains 51 characters.
      *
       01 emp-rec.
         05 emp-rec-num                pic x(3).
         05 emp-rec-name               pic x(12).
         05 emp-rec-calls              pic 999 occurs 12 times.

      *
       fd report-file
           data record is report-line
           record contains 132 characters.
      *
       01 report-line                  pic x(132).
      *
       working-storage section.
      *
      *create the necessary working storage variables
      *
       01 ws-constants.
         05 ws-number-of-months        pic 99 value 12.
      *
       01 ws-calculated-fields.
         05 ws-non-zero-month-count    pic 9(2) value 0.
         05 ws-months-no-calls         pic 9(4) value 0.
      *
       01 ws-eof-flag                  pic x value 'n'.
         88 ws-end-of-file value "y".
      *
       01 ws-totals.
         05 ws-grand-total             pic 9(5) value 0.
         05 ws-emp-total               pic 9(5) value 0.
         05 ws-total-no-calls          pic 9(5) value 0.
      *
       01 ws-name-line.
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(25) value 
         '    Ahmed Butt    '.
      *               ----+----1----+----2----+
         05 filler                     pic x(29) value 
         '                        lab 7'.
      *               ----+----1----+----2----+----
         05 filler                     pic x(5) value spaces.
         05 ws-name-line-date          pic 9(6).
         05 filler                     pic x(4) value spaces.
         05 ws-name-line-time          pic 9(8).
         05 filler                     pic x(50) value spaces.
      *
       01 ws-report-heading.
         05 filler                     pic x(40) value spaces.
         05 filler                     pic x(40) value
                   'call centre volumes for july - june     '.
      *               ----+----1----+----2----+----3----+----4
         05 filler                     pic x(40) value spaces.
         05 filler                     pic x(12) value spaces.
      *
       01 ws-heading-line1.
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(8) value 'operator'.
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(8) value 'operator'.
         05 filler                     pic x(8) value spaces.
         05 filler                     pic x(3) value 'jul'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'aug'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'sep'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'oct'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'nov'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'dec'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'jan'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'feb'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'mar'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'apr'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'may'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'jun'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(5) value 'total'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'avg'.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(3) value 'rem'.
         05 filler                     pic x(3) value spaces.
      *
       01 ws-heading-line2.
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(1) value '#'.
         05 filler                     pic x(8) value spaces.
         05 filler                     pic x(4) value 'name'.
         05 filler                     pic x(114) value spaces.
      *
       01 ws-detail-line.
         05 filler                     pic x(4) value spaces.
         05 ws-detail-line-num         pic x(6).
         05 filler                     pic x(2) value spaces.
         05 ws-detail-line-name        pic x(12).
         05 ws-detail-line-months      pic zzzzzz9 occurs 12 times.
         05 filler                     pic x(4) value spaces.
         05 ws-detail-line-total       pic zzzz9.
         05 filler                     pic x(2) value spaces.
         05 ws-detail-line-avg         pic zzzz9.
         05 ws-detail-line-zero        pic x(5) redefines 
         ws-detail-line-avg.
         05 filler                     pic x(6) value spaces.
         05 ws-detail-line-rem         pic 9.
         05 filler                     pic x(84) value spaces.

       01 ws-totals-line-with-calls.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(20) value 
         "Operators with calls".
         05 ws-months-with             pic zzzzzz9 occurs 12 times.

       01 ws-totals-line-totals.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(6) value "Totals".
         05 filler                     pic x(14) value spaces.
         05 ws-months-totals           pic zzzzzz9 occurs 12 times.
         05 filler                     pic x(4) value spaces.
         05 ws-total-total             pic zzzz9.
         05 filler                     pic x(3) value spaces.
         05 ws-total-avg               pic zzz9.
         05 filler                     pic x(4) value spaces.
         05 ws-total-rem               pic zz9.

       01 ws-totals-line-avg.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(8) value "Averages".
         05 filler                     pic x(12) value spaces.
         05 ws-months-avg              pic zzzzzz9 occurs 12 times.

      *
       01 ws-total-line1.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(39) value
                   "Number of operators with no calls: ".
      *               ----+----1----+----2----+----3----+
         05 ws-total-line-no-calls     pic zzzz9.
         05 filler                     pic x(86) value spaces.
      *
       01 ws-total-line2.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(40) value
                   "Number of months with no calls:    ".
      *               ----+----1----+----2----+----3----+
         05 ws-total-line-zero-mths    pic zzzz9.
         05 filler                     pic x(86) value spaces.

       01 ws-monthly-highest.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(43) value
       "Operator with the Highest Monthly Average: ".
      *               ----+----1----+----2----+----3----+
         05 ws-monthly-num             pic x(3).
         05 filler                     pic x(1) value spaces.
         05 ws-monthly-avg             pic zz9.

       01 ws-monthly-lowest.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(43) value
                   "Operator with the Lowest Monthly Average: ".
      *               ----+----1----+----2----+----3----+
         05 ws-monthly-num-low         pic x(3).
         05 filler                     pic x(1) value spaces.
         05 ws-monthly-avg-low         pic zz9.

       01 ws-month-highest-avg.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(43) value
                   "Month with the Highest Monthly Average: ".
      *               ----+----1----+----2----+----3----+
         05 ws-month-avg-ind           pic z9.
         05 filler                     pic x(1) value spaces.
         05 ws-month-name              pic x(3).

      *
       01 ws-total-line3.
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(43) value
                   "Overall total calls:               ".
      *               ----+----1----+----2----+----3----+
         05 ws-total-line-calls        pic zzzz9.
         05 filler                     pic x(86) value spaces.

       01 ws-calc.
         05 ws-calc-total              pic 9(5).
         05 ws-calc-total-total        pic 9(5).
         05 ws-calc-avg                pic 9(5).
         05 ws-calc-avg-total          pic 9(5).
         05 ws-calc-rem                pic 9(3).
         05 ws-calc-rem-total          pic 9(3).
         05 ws-months-with-calc        pic 99 value 0 occurs 12 times.
         05 ws-months-totals-calc      pic 9(5) value 0
         occurs 12 times.
         05 ws-months-avg-calc         pic 9(5) occurs 12 times.

       01 ws-highest-lowest-avg.
         05 ws-highest-avg             pic 9(3) value 0.
         05 ws-lowest-avg              pic 9(3) value 999.
         05 ws-highest-num             pic x(3).
         05 ws-lowest-num              pic x(3).
         05 ws-high-avg                pic 9(5) value 0.

       77 ws-sub                       pic 99 value 1.
      *
       procedure division.
      *
       000-main.
      *
      *open files
           open input emp-file,
             output report-file.
      *
      *get the current date & time
           accept ws-name-line-date from date.
           accept ws-name-line-time from time.
      *
      *output first headings
           perform 100-print-headings.
      *
      *process input file & output results
           perform 200-read-input-file.
      *
           perform 300-process-records
             until ws-end-of-file.
      *
      *output total lines
           perform 400-print-totals.
      *
      *close files
           close emp-file
             report-file.
      *
           stop run.
      *
       100-print-headings.
      *
           write report-line from ws-name-line
             after advancing 1 line.
      *
           write report-line from ws-report-heading
             after advancing 1 line.
      *
           write report-line from ws-heading-line1
             after advancing 2 lines.
      *
           write report-line from ws-heading-line2
             after advancing 1 line.
      *
       200-read-input-file.
      *reads a line from input file & stores it in emp-rec
      *unless eof is encountered in which case it sets ws-eof-flag to y
           read emp-file
               at end
                   move 'y'                        to ws-eof-flag.

       300-process-records.
      * TODO: Use Perform Varying to loop through monthly calls
      *       in each record to calculate the required values
      *       for each record and accumulate the required data
      *       for total lines

           perform varying ws-sub from 1 by 1
             until ws-sub > ws-number-of-months

               add emp-rec-calls(ws-sub)           to 
               ws-months-totals-calc(ws-sub)

               move ws-months-totals-calc(ws-sub)  to ws-months-totals(
               ws-sub)

               if emp-rec-calls(ws-sub) is not zero
                   add 1                           to 
                   ws-non-zero-month-count
                   add emp-rec-calls(ws-sub)       to ws-grand-total
                   add 1                           to 
                   ws-months-with-calc(ws-sub)
               else
                   add 1                           to 
                   ws-months-no-calls
               end-if

               add emp-rec-calls(ws-sub)           to ws-calc-total

               move emp-rec-calls(ws-sub)          to 
               ws-detail-line-months(ws-sub)

               move ws-months-with-calc(ws-sub)    to ws-months-with(
               ws-sub)

           end-perform.

      * TODO: Implement average calculation logic
      *       as outlined in the requirments

           add ws-calc-total                       to 
           ws-calc-total-total.

           divide ws-calc-total by ws-non-zero-month-count giving 
           ws-calc-avg rounded remainder ws-calc-rem.

           if ws-calc-avg > ws-highest-avg
               move ws-calc-avg                    to ws-highest-avg
               move emp-rec-num                    to ws-highest-num
           end-if.

           if ws-calc-avg < ws-lowest-avg
               move ws-calc-avg                    to ws-lowest-avg
               move emp-rec-num                    to ws-lowest-num
           end-if.

           move ws-calc-rem                        to 
           ws-detail-line-rem.

           add ws-calc-avg                         to 
           ws-calc-avg-total.
           add ws-calc-rem                         to
           ws-calc-rem-total.

           if ws-non-zero-month-count is zero
               move " ZERO"                        to 
               ws-detail-line-zero
               add 1                               to ws-total-no-calls
           else
               move ws-calc-avg                    to 
               ws-detail-line-avg
           end-if.

      * TODO: Move required data to detail line for output
      *
           move emp-rec-num                        to 
           ws-detail-line-num.
           move emp-rec-name                       to 
           ws-detail-line-name.
           move ws-calc-total                      to 
           ws-detail-line-total.

      *
      * print detail line
           write report-line from ws-detail-line
             after advancing 1 lines.
      *
      * TODO: reset fields for next record
           move 0                                  to ws-emp-total.
           move 0                                  to 
           ws-non-zero-month-count.
           move 0                                  to ws-calc-total.

      *
      * read next record (if any)
           perform 200-read-input-file.
      *
       400-print-totals.
      *
      * TODO: Move required data to total lines for output
      *
           move ws-total-no-calls                  to 
           ws-total-line-no-calls.
           move ws-grand-total                     to 
           ws-total-line-calls.
           move ws-months-no-calls                 to 
           ws-total-line-zero-mths.
           move ws-calc-total-total                to 
           ws-total-total.
           move ws-calc-avg-total                  to ws-total-avg.
           move ws-calc-rem-total                  to ws-total-rem.
           move ws-highest-avg                     to ws-monthly-avg.
           move ws-highest-num                     to ws-monthly-num.
           move ws-lowest-avg                      to 
           ws-monthly-avg-low.
           move ws-lowest-num                      to 
           ws-monthly-num-low.

           perform varying ws-sub from 1 by 1
             until ws-sub > ws-number-of-months

               divide ws-months-totals-calc(ws-sub) by
                 ws-months-with-calc(ws-sub) giving ws-months-avg-calc(
                   ws-sub) rounded

               move ws-months-avg-calc(ws-sub)     to ws-months-avg(
               ws-sub)

               if ws-months-avg-calc(ws-sub) > ws-high-avg
                   move ws-months-avg(ws-sub)      to ws-high-avg
                   move ws-sub                     to ws-month-avg-ind
                   if ws-sub = 1
                       move "JUL"                  to ws-month-name
                   end-if

                   if ws-sub = 2
                       move "AUG"                  to ws-month-name
                   end-if

                   if ws-sub = 3
                       move "SEP"                  to ws-month-name
                   end-if

                   if ws-sub = 4
                       move "OCT"                  to ws-month-name
                   end-if

                   if ws-sub = 5
                       move "NOV"                  to ws-month-name
                   end-if

                   if ws-sub = 6
                       move "DEC"                  to ws-month-name
                   end-if

                   if ws-sub = 7
                       move "JAN"                  to ws-month-name
                   end-if

                   if ws-sub = 8
                       move "FEB"                  to ws-month-name
                   end-if

                   if ws-sub = 9
                       move "MAR"                  to ws-month-name
                   end-if

                   if ws-sub = 10
                       move "APR"                  to ws-month-name
                   end-if

                   if ws-sub = 11
                       move "MAY"                  to ws-month-name
                   end-if

                   if ws-sub = 12
                       move "JUN"                  to ws-month-name
                   end-if
               end-if

           end-perform.

      *
           write report-line from ws-totals-line-with-calls
             after advancing 2 lines.
           write report-line from ws-totals-line-totals
             after advancing 2 lines.
           write report-line from ws-totals-line-avg
             after advancing 2 lines.
           write report-line from ws-total-line1
             after advancing 2 lines.
           write report-line from ws-total-line2
             after advancing 2 lines.
           write report-line from ws-monthly-highest
             after advancing 2 lines.
           write report-line from ws-monthly-lowest
             after advancing 2 lines.
           write report-line from ws-month-highest-avg
             after advancing 2 lines.
           write report-line from ws-total-line3
             after advancing 2 lines.
      *
       end program A7-CallCenterOpReport.