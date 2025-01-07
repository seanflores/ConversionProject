DECLARE FUNCTION TICKS! ()
'    "PFGA"
' THIS PROGRAM IS USED WITH THE PFG100A FREQ GEN.  IT IS USED TO CONSTRUCT
' THE PROGRAM MEMORY AND SEND IT TO THE PFG FREQ GEN.  THIS PROGRAM IS
' USED WITH EITHER THE 93C56 (2K) OR 93C66 (4K) EEPROM.

DEF FNINDEX (Y, COLUMN) = COLLEN * (COLUMN - COLMIN) + Y - 4

COLLEN = 16                             'Column length
COLMIN = 1:                  'COLMIN=1 for 96 frequencies

NUMCOL(1) = 3: NUMCOL(2) = 3: NUMCOL(3) = 3'No. of columns-1 in each minor column
NUMCOL(4) = 3: NUMCOL(5) = 3: NUMCOL(6) = 3
NUMCOL(7) = 3: NUMCOL(8) = 3: NUMCOL(9) = 3
COLPOS(1) = 1: COLPOS(2) = 7: COLPOS(3) = 13  'Starting position of each minor column
COLPOS(4) = 19: COLPOS(5) = 25: COLPOS(6) = 31
COLPOS(7) = 40: COLPOS(8) = 48: COLPOS(9) = 58
PASTEND(1) = 21: PASTEND(2) = 21: PASTEND(3) = 21
PASTEND(4) = 21: PASTEND(5) = 21: PASTEND(6) = 21
PASTEND(7) = 7: PASTEND(8) = 21: PASTEND(9) = 13


DIM DWELL$(16), FREQD$(96), OLDFREQD$(96), INTRPT$(16), MODE$(16)
DIM FREQ(18, 96) AS INTEGER, NEWFREQ(96) AS INTEGER, HEAD$(18)
DIM FP$(96), OUTPU$(18), SCANBIT(18), C$(12)
DATA " 0.2"," 0.6"," 1.2","   3","  10","  20","  30","  45"
DATA "  60","  90"," 120"," 180"," 300"," 600","1200"," INF"
FOR J = 1 TO 16: READ DWELL$(J): NEXT J
DATA "0.10","0.15","0.30","0.50","1.00","2.00","4.00","CONT"
DATA "    ","    ","    ","    ","    ","    ","    ","    "
FOR J = 1 TO 16: READ INTRPT$(J): NEXT J
MODE$(1) = "STEP": MODE$(2) = "SCAN"
GOSUB FREQZERO

'**********************************************************************
'*                      PROGRAM  SETUP ROUTINE                        *
'*                                                                    *
'**********************************************************************
   SCREEN 0, 0, 0: KEY OFF: CLS : WIDTH 80
   CR$ = CHR$(13)
   LOCATE 1, 20: PRINT "PFG-100A PROGRAMMING SOFTWARE"
   LOCATE 2, 28: PRINT "VERSION 1.0"
   LOCATE 4, 26: PRINT "PART NUMBER SP1A"
   LOCATE 8, 1
   PRINT "This program is capable of creating program files for the original PFG100"
   PRINT "or for the A version (PFG100A).   The respective limits are:"
   PRINT "                                PFG100   PFG100A"
   PRINT "Frequency Range              10-10,000  1-10,000 Hz"
   PRINT "Maximum # of Programs                9        18"
   PRINT "Maximum # of Master Frequencies     64        96"
   PRINT "Total of all Program Frequencies   118       264 or more"
   
   PRINT : PRINT "A file created within the PFG100 limits can be used on either the PFG100 or"
   PRINT "the PFG100A.  If outside the PFG100 limits, a PFG100A is required."
RES:
   PRINT
   INPUT "Is the programming cable connected to COM1(1) or COM2(2) ? > ", COMM
   IF COMM = 1 THEN LSR = &H3FD: MCR = &H3FC: LCR = &H3FB: COMM$ = "COM1:"
   IF COMM = 2 THEN LSR = &H2FD: MCR = &H2FC: LCR = &H2FB: COMM$ = "COM2:"
   CLOSE : OPEN COMM$ + "2400,N,8,1,CS,DS,RS" FOR RANDOM AS #1' OPEN COMMUNICATIONS
   GOSUB FINDTYPE       'DETERMINE ACTIVE TYPE OF MONITOR
   GOSUB CAPNUM         'Turn on CAPSLOCK & off NUMLOCK so arrow keys active

'*********************************************************************
'*                          PROGRAM BODY                             *
'*                                                                   *
'*********************************************************************
MAINMENU: CLS
MAINMENU1:
  LOCATE 1, 21: PRINT "MODEL PFG-100A SOFTWARE REV 1.0"
  LOCATE 4, 25: PRINT "M A I N   M E N U"
  LOCATE 5, 25: PRINT "================="
  LOCATE 8, 25: PRINT "(C) CREATE FILE"
  LOCATE 9, 25: PRINT "(E) EDIT FILE"
  LOCATE 10, 25: PRINT "(S) SEND FILE"
  LOCATE 11, 25: PRINT "(P) PRINT FILE"
  LOCATE 12, 25: PRINT "(Q) QUIT"
10
  LOCATE 17, 25: INPUT "SELECTION ? > ", W$
  IF W$ = "" THEN 10
  IF W$ = "Q" OR W$ = "q" THEN
     DEF SEG = &H40
     POKE (&H17), STATUSBYTE  'Return NUMLOCK to its original position
     DEF SEG
     CLOSE
     END
   END IF

   IF W$ = "S" OR W$ = "s" THEN
     W$ = "S"
     GOSUB SENDFILE
     GOTO MAINMENU
   END IF
  
   IF W$ = "P" OR W$ = "p" THEN         'Print file
     W$ = "P"
     GOSUB GETFILENAME
     GOSUB GETFILEDATA
     GOSUB PRINTDATA
     GOTO MAINMENU
   END IF

   IF W$ = "E" OR W$ = "e" THEN         'Edit file
     W$ = "E"
     GOSUB GETFILENAME
     GOSUB GETFILEDATA
     ACT$ = "EDIT"
     GOTO CREATEDITMENU
   END IF

   IF W$ = "C" OR W$ = "c" THEN         'Create file
     W$ = "C"
     ACT$ = "CREATE"
     GOTO CREATEDITMENU
   END IF
  
   LOCATE 19, 20: PRINT "EXPECTING C,E,S,P OR Q": BEEP: GOTO MAINMENU1

'***************************************************
'            CREATE AND EDIT MENUS
'***************************************************
CREATEDITMENU:
  CLS
  LOCATE 1, 20: PRINT ACT$ + " FILE MENU"
  LOCATE 2, 20: PRINT STRING$(LEN(ACT$) + 10, "=")
  LOCATE 4, 20: PRINT "(M) " + ACT$ + " MASTER FREQUENCY LIST"
  LOCATE 5, 20: PRINT "(#) " + ACT$ + " PROGRAM P#(1 - 9/18)"
  LOCATE 6, 20: PRINT "(S) SAVE FILE"
  LOCATE 7, 20: PRINT "(R) RETURN TO MAIN MENU"
SELECT1:
  LOCATE 10, 20: INPUT "SELECTION ? > ", W$
  IF W$ = "" THEN GOTO SELECT1
  COLOR 7, 0
  IF INSTR("MmSsRr", W$) = 0 THEN
    W = VAL(W$)
    IF W < 1 OR W > 18 THEN
      BEEP: LOCATE 11, 20: PRINT "EXPECTING M, 1 - 18, S, R"
      GOTO SELECT1
    END IF
  END IF
 
  IF W$ = "M" OR W$ = "m" THEN
    GOSUB FREQLIST
    GOSUB ENTERFREQS
    GOSUB SORTFREQ
    GOTO CREATEDITMENU
  END IF

  IF W$ = "S" OR W$ = "s" THEN
    GOTO SAVENEW
  END IF

  IF W$ = "R" OR W$ = "r" THEN GOTO MAINMENU

'If none of the above then it must be a program number
  PROGNUM = VAL(W$)       ' PROGNUM = 1 - 9 or 18
PROGRAMENTRY:
  GOSUB PROGLIST
  GOSUB PUTDATA
  GOSUB SELECTITEMS
  IF ADVANCEFLAG = 0 THEN GOTO CREATEDITMENU
  PROGNUM = PROGNUM + 1
  IF PROGNUM > 18 THEN PROGNUM = 1
  GOTO PROGRAMENTRY




'***************************************************
'             GET FILE NAME
'***************************************************
GETFILENAME:
     CLS
     ON ERROR GOTO 1920
1900 IF W$ = "E" OR W$ = "e" THEN
       INPUT "NAME OF FILE TO EDIT (CR TO CANCEL) ? > ", NAMEFILE$
     ELSE
       INPUT "NAME OF FILE TO PRINT (CR TO CANCEL) ? > ", NAMEFILE$
     END IF
     IF NAMEFILE$ = "" THEN GOTO MAINMENU
     CLOSE 3: OPEN NAMEFILE$ + ".P10" FOR INPUT AS #3
     CLOSE 3
     ON ERROR GOTO 0
RETURN
1920 PRINT : BEEP: PRINT "FILE DOES NOT EXIST": PRINT
     RESUME 1900

'***************************************************
'             GET FILE DATA
'***************************************************
GETFILEDATA:
  FOR J = 1 TO 18: OUTPU$(J) = "": NEXT J
  CLOSE 3: OPEN NAMEFILE$ + ".P10" FOR INPUT AS #3
  INPUT #3, WW$
  CLOSE 3
  'Check file format
  W = VAL("&H" + MID$(WW$, 6, 2))
  IF W = 0 THEN W = VAL("&H" + MID$(WW$, 8, 2))
  IF W = 128 THEN FILEFMT = 0 ELSE FILEFMT = 1  '0=small fmt, 1=large fmt
  'HMFL is Highest master frequency location.  3F for small fmt, >3F for large
  HMFL = 63 + W - 128   '3F + adder in 8080 MS byte

  TOTWORDS = ASC(MID$(WW$, 3, 1)) - 48 'Total words in string from char after W
  MSNVADDR = VAL("&H" + MID$(WW$, 4, 2))    'MS NVMEM Address
  QUITFLAG = 0
  'LISTSIZE = TOTWORDS - (MSNVADDR - 95) - 1'# frequencies in main list excluding 0
  LISTSIZE = TOTWORDS - (MSNVADDR - HMFL)  '# frequencies in main list
  'Put main list frequencies in array
  GOSUB FREQZERO                     'Initialize array to blank
  FOR J = 1 TO LISTSIZE
    W9 = VAL("&H" + MID$(WW$, TOTWORDS * 4 - J * 4 + 6, 4))
    W9$ = MID$(STR$(W9), 2)     '1st char of pos STR$ conversion is blank
    DO WHILE LEN(W9$) < 4
      W9$ = " " + W9$
    LOOP
    FREQD$(J) = W9$      '4-digit decimal values
  NEXT J
  J = 5 + 4 * (MSNVADDR - HMFL)
  JEND = J
  J = J - 3
  PROGNUM = 1
515 J = J
  COUNTER = 0
  DO
    COUNTER = COUNTER + 1
    W9 = VAL("&H" + MID$(WW$, J, 2))
    J = J - 2
  LOOP UNTIL W9 > 127
  IF COUNTER = 1 THEN GOTO QUIT
  JSTART = J + 4
  OUTPU$(PROGNUM) = MID$(WW$, JSTART, JEND - JSTART + 1)
  PROGNUM = PROGNUM + 1
  JEND = JSTART - 1
  GOTO 515
QUIT:
'Since data in this program is stored in arrays, create the arrays.
  MAXPROGNUM = PROGNUM - 1
  FOR PROGNUM = 1 TO MAXPROGNUM
    L = LEN(OUTPU$(PROGNUM))
'Next 4 lines apply to both Small & Large formats
    SCANBIT(PROGNUM) = INT(((VAL("&H" + MID$(OUTPU$(PROGNUM), L - 3, 2)) AND 64) + .5) / 64)
    W = VAL("&H" + MID$(OUTPU$(PROGNUM), L - 3, 2)) AND 63  'Remove Scan Bit
    W$ = RIGHT$("0" + HEX$(W), 2)
    MID$(OUTPU$(PROGNUM), L - 3, 2) = W$   'Scan bit removed
    IF FILEFMT = 1 THEN
'If large format, remove SCAN byte from header
      OUTPU$(PROGNUM) = LEFT$(OUTPU$(PROGNUM), L - 4) + RIGHT$(OUTPU$(PROGNUM), 2)
'Remove 7F filler byte if present
      L = LEN(OUTPU$(PROGNUM))
      IF LEFT$(OUTPU$(PROGNUM), 2) = "7F" THEN OUTPU$(PROGNUM) = RIGHT$(OUTPU$(PROGNUM), L - 2)
    END IF
'Determine HEAD$ array
    HEAD$(PROGNUM) = RIGHT$(OUTPU$(PROGNUM), 2)
'Determine FREQ array from remaining OUTPU$()
    FOR J = 1 TO 96: FREQ(PROGNUM, J) = 0: NEXT J
    L = LEN(OUTPU$(PROGNUM))
    K = HMFL - LISTSIZE
    FOR J = 1 TO L - 3 STEP 2
      W = VAL("&H" + MID$(OUTPU$(PROGNUM), J, 2))
      FREQ(PROGNUM, W - K) = 1
    NEXT J
  NEXT PROGNUM
RETURN
'***************************************************
'                    SAVE NEW FILE
'***************************************************
SAVENEW:
   CLS
   FREQSTR$ = ""
   FOR J = LISTSIZE TO 1 STEP -1
     F$ = HEX$(VAL(FREQD$(J)))
     WHILE LEN(F$) < 4
       F$ = "0" + F$
     WEND
     FREQSTR$ = FREQSTR$ + F$
   NEXT J
   NUMFREQ = LEN(FREQSTR$) / 4  '# of freq words S/B same as LISTSIZE
'Determine number of programs and number of program frequencies
   NUMPROGFREQ = 0
   MAXPROGNUM = 18  'Final value if not altered
   FOR PROGNUM = 1 TO 18
     FIRSTFREQFLAG = 0
     FOR J = 1 TO 96
       IF FREQ(PROGNUM, J) = 1 THEN
         NUMPROGFREQ = NUMPROGFREQ + 1
         FIRSTFREQFLAG = 1
       END IF
     NEXT J
     IF FIRSTFREQFLAG = 0 THEN MAXPROGNUM = PROGNUM - 1: EXIT FOR
   NEXT PROGNUM
  
   W = NUMPROGFREQ + MAXPROGNUM
   IF W MOD 2 = 1 THEN FILL = 1 ELSE FILL = 0
   W = W + FILL
   PROGWORDS = INT(W / 2) + 1

   PRINT : PRINT "                        USES   LIMIT"
   PRINT USING "MASTER FREQUENCIES        ##      64"; NUMFREQ
   PRINT USING "NUMBER OF PROGRAMS        ##       9"; MAXPROGNUM
   PRINT USING "NO. PROGRAM FREQUENCIES  ###     ###"; NUMPROGFREQ; 126 - MAXPROGNUM - FILL
   PRINT
   IF NUMFREQ < 65 THEN HMFL = 63 ELSE HMFL = NUMFREQ - 1 'HMFL min is 63 else as required

'Can it be used in an original PFG100 (non-A version) ?
   IF NUMFREQ > 64 OR MAXPROGNUM > 9 OR PROGWORDS > 64 THEN
     PRINT "THIS FILE CAN BE USED ONLY WITH A PFG100A BECAUSE ONE OR MORE OF THE ABOVE"
     PRINT "LIMITS WERE EXCEEDED.": PRINT
PR:  INPUT "PROCEED WITH A PFG100A FILE (Y/N) ? > ", W$
     IF W$ <> "Y" AND W$ <> "N" THEN BEEP: PRINT "EXPECTING Y OR N": GOTO PR
     IF W$ = "N" THEN GOTO CREATEDITMENU ELSE GOTO AVERS
   ELSE
     PRINT "THIS FILE CAN BE USED WITH AN ORIGINAL PFG100 OR A PFG100A"
   END IF
'Create small format file.  Add in SCANBIT's to lowest freq of each program
   OUTP$ = ""
   K = HMFL - LISTSIZE
   FOR PROGNUM = 1 TO MAXPROGNUM
     OUTPU$(PROGNUM) = ""
     FOR J = 64 TO 1 STEP -1
       IF FREQ(PROGNUM, J) = 1 THEN
         W$ = RIGHT$("0" + HEX$(J + K), 2)
         OUTPU$(PROGNUM) = OUTPU$(PROGNUM) + W$
       END IF
     NEXT J
     IF SCANBIT(PROGNUM) = 1 THEN
       L = LEN(OUTPU$(PROGNUM))
       W = VAL("&H" + MID$(OUTPU$(PROGNUM), L - 1, 2)) + 64
       W$ = HEX$(W)
       MID$(OUTPU$(PROGNUM), L - 1, 2) = W$
     END IF
     OUTPU$(PROGNUM) = OUTPU$(PROGNUM) + HEAD$(PROGNUM)
     OUTP$ = OUTPU$(PROGNUM) + OUTP$
   NEXT PROGNUM
   OUTP$ = "8080" + OUTP$
   IF LEN(OUTP$) MOD 4 <> 0 THEN OUTP$ = "00" + OUTP$
   GOTO FINAL

'Create Large format file for use only a PFG100A.  Add in SCANBIT's to
'  2-byte header.
AVERS:
   OUTP$ = ""
   EXTRA = 0   'Number of filler bytes
   IF HMFL = 63 THEN HMFL = 64   'Min HMFL for Version A
   K = HMFL - LISTSIZE
   FOR PROGNUM = 1 TO MAXPROGNUM
     OUTPU$(PROGNUM) = ""
     FOR J = 96 TO 1 STEP -1
       IF FREQ(PROGNUM, J) = 1 THEN
         W$ = RIGHT$("0" + HEX$(J + K), 2)
         OUTPU$(PROGNUM) = OUTPU$(PROGNUM) + W$
       END IF
     NEXT J
     IF LEN(OUTPU$(PROGNUM)) MOD 4 <> 0 THEN
       OUTPU$(PROGNUM) = "7F" + OUTPU$(PROGNUM)
       EXTRA = EXTRA + 1
     END IF
     IF SCANBIT(PROGNUM) = 1 THEN S$ = "40" ELSE S$ = "00"
     OUTPU$(PROGNUM) = OUTPU$(PROGNUM) + S$ + HEAD$(PROGNUM)
     OUTP$ = OUTPU$(PROGNUM) + OUTP$
   NEXT PROGNUM
   W = 128 + HMFL - 63
   W$ = HEX$(W)
   OUTP$ = W$ + "80" + OUTP$                  'End with dummy P6,P7
   L = LEN(OUTP$)     ' Program nibble length
   PROGWORDS = INT((L + .5) / 4)
   PRINT : PRINT "                        USES   LIMIT"
   PRINT USING "MASTER FREQUENCIES        ##      96"; NUMFREQ
   PRINT USING "NUMBER OF PROGRAMS        ##      18"; MAXPROGNUM
   PRINT USING "NO. PROGRAM FREQUENCIES  ###     ###"; NUMPROGFREQ; 511 - 2 * HMFL - J9 * 2 - 2 - EXTRA

'Can it be used with a PFG100A ?
   IF (NUMFREQ > 96) OR (MAXPROGNUM > 18) OR (PROGWORDS > (255 - HMFL - MAXPROGNUM - 1 - EXTRA / 2)) THEN
     PRINT "THIS FILE CANNOT BE USED WITH A PFG100A BECAUSE ONE OR MORE OF THE ABOVE"
     PRINT "LIMITS WERE EXCEEDED.": PRINT
     PRINT "HIT ANY KEY TO RETURN TO THE MENU"
PQ:  W$ = INKEY$: IF W$ = "" THEN GOTO PQ
     GOTO CREATEDITMENU
   ELSE
     PRINT "THIS FILE CAN BE USED ONLY WITH A PFG100A"
   END IF
FINAL:
   NUMPROGWORDS = LEN(OUTP$) / 4          'No. words in program half only
   MSWORD = HMFL + NUMPROGWORDS
   MSWORD$ = RIGHT$(HEX$(MSWORD), 2)
   NUMWORDS = NUMPROGWORDS + LISTSIZE     'No. words including frequencies
   NUMWORDS$ = CHR$(48 + NUMWORDS)        '1 char representing # words
   OUTP$ = "*W" + NUMWORDS$ + MSWORD$ + OUTP$ + FREQSTR$

5000 PRINT : INPUT "NAME OF FILE TO SAVE DATA UNDER (CR TO CANCEL) ?  > ", NAMEFILE$
     IF NAMEFILE$ = "" THEN GOTO CREATEDITMENU
     ON ERROR GOTO 5020
     OPEN NAMEFILE$ + ".P10" FOR INPUT AS #3'Check if file already exists.  Error if not.
     PRINT : BEEP: PRINT NAMEFILE$ + " ALREADY EXISTS.  FILE DATA WILL BE REPLACED WITH NEW DATA."
5005 PRINT : INPUT "PROCEED (Y/N) ? ", W$
     IF INSTR("YyNn", W$) = 0 THEN
       PRINT : PRINT "EXPECTING Y OR N"
       GOTO 5005
     END IF
     IF W$ <> "Y" AND W$ <> "y" THEN CLOSE 3: GOTO 5000
5010 CLOSE 3
     OPEN NAMEFILE$ + ".P10" FOR OUTPUT AS #3
     PRINT #3, OUTP$
     CLOSE 3
     ON ERROR GOTO 0
GOTO MAINMENU
5020 RESUME 5010           'Cant open for input. File name doesnt exist. So OK for saving

'***************************************************
'               SEND FILE
'***************************************************
SENDFILE:
     CLS
     ON ERROR GOTO 1700
1350 NOFILE = 0: PRINT : INPUT "NAME OF FILE TO SEND (CR TO CANCEL) ? > ", FILE$
     IF FILE$ = "" THEN GOTO MAINMENU
1351 CLOSE 2: OPEN FILE$ + ".P10" FOR INPUT AS #2
     IF NOFILE = 1 THEN PRINT : PRINT "FILE NOT FOUND!": GOTO 1350
     INPUT #2, DAT$
     PRINT : PRINT "HIT ANY KEY WHEN READY TO SEND DATA"
     PRINT
1454 IF INKEY$ = "" THEN 1454
     COLOR 23, 0
     PRINT : CL = CSRLIN: PRINT "DATA TRANSMISSION IN PROCESS"
     COLOR 7, 0
     PRINT : PRINT "*** PFG-100 DISPLAY SHOULD BE BLANK WHILE DATA BEING RECEIVED ***"
     'PRINT DAT$
  ' Parse data into Preliminary batchs of 32 words and a Final batch
     NWORDS = ASC(MID$(DAT$, 3, 1)) - 48'Total number of words in file string
     STARTNVADDR$ = MID$(DAT$, 4, 2)    'Starting NVMEM address
     STARTNVADDR = VAL("&H" + STARTNVADDR$)
     DAT$ = MID$(DAT$, 6)               'Keep only data
     DO WHILE NWORDS > 32
       P$ = "*P" + CHR$(32 + 48) + STARTNVADDR$ + MID$(DAT$, 1, 128)
       PRINT #1, P$
       'PRINT P$: PRINT        'For test only
       NWORDS = NWORDS - 32
       STARTNVADDR = STARTNVADDR - 32
       STARTNVADDR$ = HEX$(STARTNVADDR)
       IF LEN(STARTNVADDR$) < 2 THEN STARTNVADDR$ = "0" + STARTNVADDR$
       DAT$ = MID$(DAT$, 129)
  '  Introduce a 15 tick delay to ensure no overlap on reading RAM memory
  '  for writing freq to NVMEM while writing program data to same RAM memory
       W = TICKS
       W = W + 30
       IF W > 65535 THEN W = W - 65535
       DO WHILE (TICKS < W) AND ((W - TICKS) < 1000) '2nd covers TICKS=0 W=65535
       LOOP
     LOOP
     F$ = "*F" + CHR$(NWORDS + 48) + STARTNVADDR$ + DAT$
     PRINT #1, F$: PRINT
     'PRINT F$
     CLOSE 2
     LOCATE CL, 1: PRINT "DATA TRANSMISSION IN PROCESS !"
     PRINT : PRINT : PRINT
     PRINT : PRINT "DATA TRANSMISSION COMPLETE !"
     PRINT : PRINT "*** PFG-100 DISPLAY SHOULD READ 'P 1' ***"
     PRINT
     PRINT : PRINT "HIT ESC KEY TO RETURN TO MAIN MENU"
     PRINT : PRINT "HIT ANY OTHER KEY TO REPEAT SEND"
1520 W$ = INKEY$: IF W$ = "" THEN 1520
     IF W$ <> CHR$(27) THEN CLS : GOTO 1351
     ON ERROR GOTO 0
     GOTO MAINMENU
     END        'TEMP
'ERROR ROUTINE
1700 NOFILE = 1
     RESUME NEXT


'********************************************************************
'                      SCREEN CHARACTERS
'  COLOR 7,0  Normal char    SCREEN(Y,X,1)=   7   SCREEN(Y,X)= ASCII val
'  COLOR 0,7  Reverse char   SCREEN(Y,X,1)= 112             "
'  COLOR 23,0 Normal flash   SCREEN(Y,X,1)= 135             "
'  COLOR 16,7 Reverse flash  SCREEN(Y,X,1)= 240             "
'********************************************************************

'***************************************************
'             FLASH POSITION
'***************************************************
FLASHPOS:
  X = COLPOS(COLUMN)
  FOR X1 = 0 TO NUMCOL(COLUMN)
  ASK(X1) = SCREEN(Y, X + X1)    'Get ASCII value
  NEXT X1
  IF SCREEN(Y, X, 1) = 7 THEN
    COLOR 23, 0
  ELSE
    COLOR 16, 7
  END IF
  FOR X1 = 0 TO NUMCOL(COLUMN)
  LOCATE Y, X + X1: PRINT CHR$(ASK(X1));
  NEXT X1
  X1 = 0
  COLOR 7, 0
RETURN

'***************************************************
'             NO FLASH POSITION
'***************************************************
NOFLASH:
  IF SCREEN(Y, X, 1) MOD 16 = 7 THEN
    COLOR 7, 0
  ELSE
    COLOR 0, 7
  END IF
  FOR X1 = 0 TO NUMCOL(COLUMN)
  LOCATE Y, X + X1: PRINT CHR$(ASK(X1));
  NEXT X1
  X1 = 0
  COLOR 7, 0
RETURN

'********************************************************************
'             MAIN FREQUENCY LIST
'********************************************************************
FREQZEROENT:
  GOSUB FREQZERO
FREQLIST:
  FOR J = 1 TO 96: OLDFREQD$(J) = FREQD$(J): NEXT J 'Save old list
  FOR J = COLMIN TO 6: PASTEND(J) = 5 + COLLEN: NEXT J
  'PASTEND(6) = 4 + COLLEN
  CLS
  IF TYPEV = 0 THEN COLOR 1, 0 ELSE COLOR 7, 1
  LOCATE 1, 10: PRINT "MASTER FREQUENCY LIST"
  COLOR 7, 0
  LOCATE 3, 1: PRINT "FREQ  FREQ  FREQ  FREQ  FREQ  FREQ"
  IF TYPEV = 0 THEN COLOR 1, 0 ELSE COLOR 7, 1
  LOCATE 4, 1: PRINT " HZ    HZ    HZ    HZ    HZ    HZ"
  COLOR 7, 0
  OLDLISTSIZE = 0    'Used at end of SORTFREQ procedure
  FOR Y = 5 TO 20
    FOR COLUMN = COLMIN TO 6
   '   IF COLUMN = 6 AND Y = 20 THEN EXIT FOR
      X = 6 * COLUMN - 5
      IF FREQD$(FNINDEX(Y, COLUMN)) <> "????" THEN
        COLOR 0, 7
        OLDLISTSIZE = OLDLISTSIZE + 1
      END IF
      LOCATE Y, X: PRINT FREQD$(FNINDEX(Y, COLUMN))
      COLOR 7, 0
    NEXT COLUMN
  NEXT Y
  LOCATE 25, 1: PRINT "HIT 'Z' TO ZERO DATA OR 'R' TO RETURN TO MENU";
RETURN
         
'********************************************************************
'                    ENTER MAIN LIST FREQUENCIES
'********************************************************************
ENTERFREQS:
  Y = 5: COLUMN = COLMIN
  GOSUB FLASHPOS
9280  W$ = INKEY$: IF W$ = "" THEN 9280
  IF LEN(W$) = 2 THEN W = ASC(RIGHT$(W$, 1)) + 1000 ELSE W = ASC(W$)
  IF W = 1077 THEN GOTO RIGHTARROWS
  IF W = 1075 THEN GOTO LEFTARROWS
  IF W = 1072 THEN GOTO UPARROWS
  IF W = 1080 THEN GOTO DOWNARROWS
  IF W = 90 OR W = 122 THEN GOSUB FREQZEROENT: GOTO ENTERFREQS 'Z  ZERO DATA
  IF W = 82 OR W = 114 THEN GOSUB READFREQ: RETURN     'R  RETURN TO MENU
  IF W = 13 THEN GOTO ALTERNATES             'ENTER
  IF ((W > 47 AND W < 58) OR W = 8) THEN GOSUB CHANGEFREQ: GOTO 9280
  BEEP: GOTO 9280
  END

RIGHTARROWS:
  GOSUB NOFLASH
INCCOLS:
  COLUMN = COLUMN + 1: IF COLUMN > 6 THEN COLUMN = COLMIN
  IF Y >= PASTEND(COLUMN) THEN GOTO INCCOLS
  GOSUB FLASHPOS
  GOTO 9280

LEFTARROWS:
  GOSUB NOFLASH
DECCOLS:
  COLUMN = COLUMN - 1: IF COLUMN < COLMIN THEN COLUMN = 6
  IF Y >= PASTEND(COLUMN) THEN GOTO DECCOLS
  GOSUB FLASHPOS
  GOTO 9280

UPARROWS:
  GOSUB NOFLASH
  Y = Y - 1: IF Y = 4 THEN Y = PASTEND(COLUMN) - 1
  GOSUB FLASHPOS
  GOTO 9280
DOWNARROWS:
  GOSUB NOFLASH
  Y = Y + 1: IF Y = PASTEND(COLUMN) THEN Y = 5
  GOSUB FLASHPOS
  GOTO 9280
CHANGEFREQ:
  IF W = 8 THEN
    X1 = X1 - 1
    IF X1 < 0 THEN BEEP: X1 = 0
    IF X1 = 0 THEN
      LOCATE Y, X: PRINT CHR$(32)
    ELSE
      FOR I = X1 TO 3
        LOCATE Y, X + I: PRINT CHR$(32)
        ASK(I) = 32
      NEXT I
    END IF
  ELSE
    IF X1 > 3 THEN X1 = 3
    LOCATE Y, X + X1
    PRINT CHR$(W);
    ASK(X1) = SCREEN(Y, X + X1)
    X1 = X1 + 1
    IF X1 < 3 THEN
      FOR I = X1 TO 3
        LOCATE Y, X + I: PRINT CHR$(32)
        ASK(I) = 32
      NEXT I
    END IF
  END IF
RETURN

ALTERNATES:
  IF ASK(3) = 63 THEN GOTO DOWNARROWS
  DO UNTIL ASK(3) <> 32         'This loop right-hand justifies
    FOR I = 3 TO 1 STEP -1
      ASK(I) = ASK(I - 1)
    NEXT I
    ASK(0) = 32
  LOOP
  IF SCREEN(Y, X, 1) MOD 16 = 7 THEN
    COLOR 0, 7
  ELSE
    COLOR 7, 0
  END IF
9290 W1$ = ""
  LZFLAG = 0       'Suppress leading zeros for sorting reasons
  FOR X1 = 0 TO 3
  IF ASK(X1) = 48 AND LZFLAG = 0 THEN ASK(X1) = 32 ELSE LZFLAG = 1
  LOCATE Y, X + X1: PRINT CHR$(ASK(X1));
  W1$ = W1$ + CHR$(ASK(X1))
  NEXT X1
'  IF VAL(W1$) < 10 THEN ASK(2) = 49: ASK(3) = 48: GOTO 9290
  X1 = 0
  COLOR 7, 0
  GOTO DOWNARROWS

READFREQ:
   LOCATE 23, 1: PRINT "WAIT___";
   LISTSIZE = 0
   OUTP$ = ""
   FOR COLUMN = COLMIN TO 6
     X = COLPOS(COLUMN)
     FOR Y = 5 TO COLLEN + 4
       IF SCREEN(Y, X, 1) MOD 16 = 0 THEN     'Reverse selected item
         LISTSIZE = LISTSIZE + 1
         ASK$ = ""
         FOR X1 = 0 TO 3
           ASK$ = ASK$ + CHR$(SCREEN(Y, X + X1))
         NEXT X1
         FREQD$(FNINDEX(Y, COLUMN)) = ASK$
       ELSE
         FREQD$(FNINDEX(Y, COLUMN)) = "????"
       END IF
     NEXT Y
   NEXT COLUMN
RETURN

'********************************************************************
'            SORTFREQ
'********************************************************************

SORTFREQ:
  FLAG = 0
  FOR J = 1 TO 95
  'FOR J = 1 TO NUMFREQ - 1
    IF FREQD$(J + 1) < FREQD$(J) THEN
      SWAP FREQD$(J), FREQD$(J + 1)
      FLAG = 1
    END IF
  NEXT J
  IF FLAG = 1 THEN GOTO SORTFREQ
  J = 1
NODUPS:
  IF LISTSIZE < 2 THEN RETURN
  IF FREQD$(J + 1) = FREQD$(J) THEN
    FREQD$(J + 1) = "????"
    LISTSIZE = LISTSIZE - 1
    GOTO SORTFREQ
  END IF
  J = J + 1: IF J < LISTSIZE THEN GOTO NODUPS
'Update program frequency arrays
  FOR PROGNUM = 1 TO MAXPROGNUM
    FOR J = 1 TO LISTSIZE: NEWFREQ(J) = 0: NEXT J
    FOR J = 1 TO OLDLISTSIZE
      IF FREQ(PROGNUM, J) = 1 THEN
        W$ = OLDFREQD$(J)
        FOR K = 1 TO LISTSIZE
          IF FREQD$(K) = W$ THEN
          '  IF J <> K THEN
              NEWFREQ(K) = 1
          '  END IF
          END IF
        NEXT K
      END IF
    NEXT J
    FOR J = 1 TO LISTSIZE: FREQ(PROGNUM, J) = NEWFREQ(J): NEXT J
  NEXT PROGNUM

RETURN

'********************************************************************
'            CREATE PROGRAM LIST
'********************************************************************

PROGLIST:
  FOR J = COLMIN TO 6: PASTEND(J) = 5 + COLLEN: NEXT J
'  PASTEND(6) = 4 + COLLEN     'Only 95 freq's
  CLS
  IF TYPEV = 0 THEN COLOR 1, 0 ELSE COLOR 7, 1
  LOCATE 1, 18: PRINT USING "PROGRAM NUMBER ##"; PROGNUM
  COLOR 7, 0: PRINT
  LOCATE 2, 1: PRINT "                                                          ON/"
  LOCATE 3, 1:  PRINT "FREQ  FREQ  FREQ  FREQ  FREQ  FREQ     MODE    DWELL      OFF"
  IF TYPEV = 0 THEN COLOR 1, 0 ELSE COLOR 7, 1
  LOCATE 4, 1: PRINT " HZ    HZ    HZ    HZ    HZ    HZ               SEC       SEC"
  COLOR 7, 0
  FOR Y = 5 TO 20
  FOR COLUMN = COLMIN TO 9
    X = COLPOS(COLUMN)
    IF COLUMN < 7 THEN
      W$ = FREQD$(FNINDEX(Y, COLUMN))
      IF W$ = "????" THEN
        W$ = "    "
        IF Y < PASTEND(COLUMN) THEN PASTEND(COLUMN) = Y
      END IF
      LOCATE Y, X: PRINT USING "\  \"; W$
    END IF
   
    IF COLUMN = 7 THEN
      LOCATE Y, X: PRINT MODE$(Y - 4)
    END IF
 
    IF COLUMN = 8 THEN
      LOCATE Y, X: PRINT DWELL$(Y - 4)
    END IF

    IF COLUMN = 9 THEN
      LOCATE Y, X: PRINT INTRPT$(Y - 4)
    END IF
 
  NEXT COLUMN
  NEXT Y
  LOCATE 5, 44: PRINT "*": LOCATE 8, 52: PRINT "*": LOCATE 6, 62: PRINT "*"
  LOCATE 22, 1: PRINT "* Default values for unselected parameters"
  LOCATE 25, 1: PRINT "HIT 'Z' TO ZERO DATA, 'R' TO RETURN TO MENU, 'A' TO ADVANCE TO NEXT PROGRAM";
RETURN

'***************************************************
'             PUT DATA
'***************************************************
PUTDATA:
  W9 = VAL("&H" + HEAD$(PROGNUM))
 
  W0 = W9 AND 15                'DWELL INDEX
  COLUMN = 8
  Y = W0 + 5
  GOSUB REVERSE

  W1 = INT((W9 AND 112) / 16 + .5)'INTRPT INDEX
  COLUMN = 9
  Y = W1 + 5
  GOSUB REVERSE
   
  FOR J = 1 TO LISTSIZE
    IF FREQ(PROGNUM, J) = 1 THEN
      COLUMN = COLMIN + INT((J - .5) / COLLEN)
      Y = ((J - 1) MOD COLLEN) + 5
      IF (Y > 4 AND Y < (COLLEN + 5)) THEN GOSUB REVERSE
    END IF
  NEXT J
 
  COLUMN = 7
  Y = SCANBIT(PROGNUM) + 5
  GOSUB REVERSE
RETURN

REVERSE:
    GOSUB FLASHPOS
    VALFLAG = 0
    FOR X1 = 0 TO NUMCOL(COLUMN)
      IF ASK(X1) <> 32 THEN VALFLAG = 1
    NEXT X1
    IF VALFLAG = 0 THEN RETURN
    COLOR 0, 7
    FOR X1 = 0 TO NUMCOL(COLUMN)
      LOCATE Y, X + X1: PRINT CHR$(ASK(X1));
    NEXT X1
    X1 = 0
    COLOR 7, 0
RETURN
'***************************************************
'             SELECT ITEMS
'***************************************************
SELECTITEMS:
  ADVANCEFLAG = 0
  Y = 5: COLUMN = COLMIN
  GOSUB FLASHPOS
3280  LOCATE 23, 1: PRINT STRING$(75, " ")
3281  W$ = INKEY$: IF W$ = "" THEN 3281
  ERRORTYP = 0
  IF LEN(W$) = 2 THEN W = ASC(RIGHT$(W$, 1)) + 1000 ELSE W = ASC(W$)
  IF W = 1077 THEN GOTO RIGHTARROW
  IF W = 1075 THEN GOTO LEFTARROW
  IF W = 1072 THEN GOTO UPARROW
  IF W = 1080 THEN GOTO DOWNARROW
  IF W = 13 THEN GOTO ALTERNATE                             'CR ENTER
  IF W = 90 OR W = 122 THEN GOSUB BLANKDATA: GOTO SELECTITEMS  'Z  ZERO DATA
  IF W = 82 OR W = 114 THEN ADVANCEFLAG = 0: GOTO TESTERR  'R  RETURN TO MENU
  IF W = 65 OR W = 78 OR W = 97 OR W = 110 THEN ADVANCEFLAG = 1: GOTO TESTERR'A OR N  NEXT PROGRAM
  BEEP: GOTO 3280
  
TESTERR:
    GOSUB NOFLASH
    GOSUB READSCRN
    IF ERRORTYP = 0 THEN RETURN
    GOSUB PRINTERR: Y = 5: COLUMN = COLMIN: GOSUB FLASHPOS: GOTO 3281

RIGHTARROW:
  GOSUB NOFLASH
INCCOL:
  COLUMN = COLUMN + 1: IF COLUMN > 9 THEN COLUMN = COLMIN
  IF Y >= PASTEND(COLUMN) THEN GOTO INCCOL
  GOSUB FLASHPOS
  GOTO 3280

LEFTARROW:
  GOSUB NOFLASH
DECCOL:
  COLUMN = COLUMN - 1: IF COLUMN < COLMIN THEN COLUMN = 9
  IF Y >= PASTEND(COLUMN) THEN GOTO DECCOL
  GOSUB FLASHPOS
  GOTO 3280

UPARROW:
  GOSUB NOFLASH
  Y = Y - 1: IF Y = 4 THEN Y = PASTEND(COLUMN) - 1
  GOSUB FLASHPOS
  GOTO 3280
DOWNARROW:
  GOSUB NOFLASH
  Y = Y + 1
  IF Y = PASTEND(COLUMN) THEN
    Y = 5
  ELSEIF COLUMN < 5 THEN
    IF FREQD$(FNINDEX(Y, COLUMN)) = "????" THEN Y = 5
  END IF
  GOSUB FLASHPOS
  GOTO 3280

BLANKDATA:
  OUTPU$(PROGNUM) = ""
  GOSUB PROGLIST
RETURN

PRINTERR:
  BEEP: LOCATE 23, 1
  COLOR 23, 0
  SELECT CASE ERRORTYP
    CASE 1
      PRINT "ERROR";
      COLOR 7, 0
      PRINT " - SELECT ONLY 1 PARAMETER EACH FROM THE RIGHT 3 COLUMNS"
    CASE 2
      PRINT "ERROR";
      COLOR 7, 0
      PRINT " - SELECT EXACTLY 2 FREQUENCIES IN THE SCAN MODE"
  END SELECT
RETURN

'***************************************************
'             ALTERNATE NORMAL, REVERSE
'***************************************************
ALTERNATE:
  IF SCREEN(Y, X, 1) MOD 16 = 7 THEN
    COLOR 16, 7
  ELSE
    COLOR 23, 0
  END IF
  FOR X1 = 0 TO NUMCOL(COLUMN)
  LOCATE Y, X + X1: PRINT CHR$(ASK(X1));
  NEXT X1
  X1 = 0
  COLOR 7, 0
GOTO DOWNARROW

'********************************************************************
'                      READ SCREEN DATA
'   If no item selected for DWELL or INTR(ON/OFF) then default values
'     DWELL=1 and INTR=1 are assigned.
'   If more than 1 item is selected the lower one on the list is assigned.
'********************************************************************
READSCRN:
   OUTP$ = ""
   NUMFREQSELECTED = 0             '=0 at end if no freqs selected, zero data
   NDWELL = 0: NINTR = 0: NMODE = 0         '# items counters
   SCANBIT(PROGNUM) = 0
   FOR J = 1 TO 96: FREQ(PROGNUM, J) = 0: NEXT J
   FOR COLUMN = COLMIN TO 9
     X = COLPOS(COLUMN)
     FOR Y = 5 TO COLLEN + 4
       IF SCREEN(Y, X, 1) MOD 16 = 0 THEN     'Reverse selected item
         IF COLUMN = 7 THEN
           MODE = Y - 5
           NMODE = NMODE + 1
         ELSEIF COLUMN = 8 THEN
           DWELL = Y - 5
           NDWELL = NDWELL + 1
         ELSEIF COLUMN = 9 THEN
           INTR = Y - 5
           NINTR = NINTR + 1
         ELSE
           FREQ(PROGNUM, FNINDEX(Y, COLUMN)) = 1
           NUMFREQSELECTED = NUMFREQSELECTED + 1
         END IF
       END IF
     NEXT Y
   NEXT COLUMN
   IF NUMFREQSELECTED = 0 THEN RETURN
   IF NDWELL = 0 THEN DWELL = 3       'DWELL not selected, set default val
   IF NINTR = 0 THEN INTR = 1         'INTR not selected, set default val
   IF NMODE = 0 THEN MODE = 0         'MODE not selected, set default val
   IF NDWELL > 1 OR NINTR > 1 OR NMODE > 1 THEN ERRORTYP = 1: RETURN
   IF MODE = 1 AND NUMFREQSELECTED <> 2 THEN ERRORTYP = 2: RETURN
   SCANBIT(PROGNUM) = MODE
   HEAD = 128 + 16 * INTR + DWELL
   HEAD$(PROGNUM) = HEX$(HEAD)
RETURN

'***************************************************
'             PRINT DATA
'***************************************************
PRINTDATA:
  LPRINT STRING$(65, " ") + "FILE= " + NAMEFILE$
  LPRINT "MASTER FREQUENCY LIST"
  LPRINT "---------------------"
  NUMFREQ = LISTSIZE
  PAGEROWS = 3
  GOSUB PRINTMFREQ         'Print Master Freq List
  FOR PROGNUM = 1 TO MAXPROGNUM
    W$ = OUTPU$(PROGNUM)
    W9 = VAL("&H" + HEAD$(PROGNUM))  'Header value
    W2 = SCANBIT(PROGNUM)   'MODE, STEP OR SCAN
    W0 = W9 AND 15                    'DWELL INDEX
    W1 = INT((W9 AND 112) / 16 + .5)  'INTRPT INDEX

    LPRINT USING "PROGRAM ##    \  \/\  \/\  \"; PROGNUM; MODE$(W2 + 1); DWELL$(W0 + 1); INTRPT$(W1 + 1)
    PAGEROWS = PAGEROWS + 1
'    LMAX = LEN(W$) - 2
'    K = 1
'    NUMFREQ = 0
'    FOR L = 1 TO LMAX STEP 2
'      W = VAL("&H" + MID$(W$, L, 2))   'Freq address
'      K8 = W - (HMFL - LISTSIZE)
'      K = INT((LMAX - 1) / 2) - INT(L / 2) + 1
'      FP$(K) = FREQD$(K8)
'NL: NEXT L
'    NUMFREQ = INT(LMAX / 2)       'Total # of freq in Program
    NUMFREQ = 0
    K = 0
    FOR J = 1 TO LISTSIZE
      IF FREQ(PROGNUM, J) = 1 THEN
        K = K + 1
        FP$(K) = FREQD$(J)
        NUMFREQ = NUMFREQ + 1
      END IF
    NEXT J
    GOSUB PRINTFREQ
  NEXT PROGNUM
RETURN

PRINTMFREQ:
  FOR J = 1 TO NUMFREQ
    FP$(J) = FREQD$(J)          'To use next subroutine
  NEXT J
PRINTFREQ:
  NUMROW = INT((NUMFREQ - .5) / 12) + 1
  PAGEROWS = PAGEROWS + NUMROW
  IF PAGEROWS > 62 THEN LPRINT CHR$(12): PAGEROWS = 0
  FOR RROW = 1 TO NUMROW
    IF RROW <> NUMROW THEN
      FOR CCOL = 1 TO 12
        C$(CCOL) = FP$(12 * (RROW - 1) + CCOL)
      NEXT CCOL
    ELSE
      FOR K1 = 1 TO 12: C$(K1) = "": NEXT K1
      FOR CCOL = 1 TO (NUMFREQ - 1) MOD 12 + 1
        C$(CCOL) = FP$(12 * (RROW - 1) + CCOL)
      NEXT CCOL
    END IF
    LPRINT USING "  \  \"; C$(1); C$(2); C$(3); C$(4); C$(5); C$(6); C$(7); C$(8); C$(9); C$(10); C$(11); C$(12)
  NEXT RROW
RETURN

'********************************************************************
'                  FIND VIDEO DISPLAY TYPE
'   TYPE=0 MONOCHROME,  TYPE=1 80 X 25 GRAPHICS, TYPE=2 EGA, TYPE=3 OTHER
'********************************************************************
FINDTYPE:
  DEF SEG = &H40
  X = PEEK(&H87)
  IF ((X <> 0) AND ((X AND 8) = 0)) THEN
    TYPEV = 2
  ELSE
    SELECT CASE PEEK(&H10) AND 48
      CASE 48
        TYPEV = 0
      CASE 32
        TYPEV = 1
      CASE ELSE
        TYPEV = 3
    END SELECT
  END IF
  DEF SEG
RETURN

'********************************************************************
'      TURN ON CAPLOCK, OFF NUMLOCK TO ENABLE ARROW KEYS
'********************************************************************
CAPNUM:                         'See P.104 of Problem Solver
  DEF SEG = &H40
  STATUSBYTE = PEEK(&H17)
  NEWBYTE = (STATUSBYTE AND 223) OR 64   'Set bit 5 = 0, bit 6 = 1
  POKE (&H17), NEWBYTE
  DEF SEG
RETURN

'********************************************************************
'                   ZERO FREQUENCY ARRAY
'********************************************************************
FREQZERO:
  FOR J = 1 TO 96: FREQD$(J) = "????": NEXT J
RETURN

FUNCTION TICKS
  DEF SEG = &H40
  A = PEEK(&H6D)
  B = PEEK(&H6C)
  C = PEEK(&H6D)
  IF A <> C THEN B = 0
  TICKS = 256 * C + B
  DEF SEG
END FUNCTION

