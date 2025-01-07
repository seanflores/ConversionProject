// *************************************************************
//   Created with BCX -- The BASIC To C Translator (ver 5.12-08.08.22)
//   BCX (c) 1999 - 2008 by Kevin Diggins
// *************************************************************
//     Translated for compiling with a C Compiler
// *************************************************************
#include <windows.h>    // Win32 Header File 
#include <windowsx.h>   // Win32 Header File 
#include <commctrl.h>   // Win32 Header File 
#include <commdlg.h>    // Win32 Header File 
#include <mmsystem.h>   // Win32 Header File 
#include <shellapi.h>   // Win32 Header File 
#include <shlobj.h>     // Win32 Header File 
#include <richedit.h>   // Win32 Header File 
#include <wchar.h>      // Win32 Header File 
#include <objbase.h>    // Win32 Header File 
#include <ocidl.h>      // Win32 Header File 
#include <winuser.h>    // Win32 Header File 
#include <olectl.h>     // Win32 Header File 
#include <oaidl.h>      // Win32 Header File 
#include <ole2.h>       // Win32 Header File 
#include <oleauto.h>    // Win32 Header File 
#include <conio.h>
#include <direct.h>
#include <ctype.h>
#include <io.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <setjmp.h>
#include <time.h>
#include <stdarg.h>
#include <process.h>


// ***************************************************
// Compiler Defines
// ***************************************************

// C++
#if defined( __cplusplus )
  #define overloaded
  #define C_EXPORT EXTERN_C __declspec(dllexport)
  #define C_IMPORT EXTERN_C __declspec(dllimport)
#else
  #define C_EXPORT __declspec(dllexport)
  #define C_IMPORT __declspec(dllimport)
#endif

// Open Watcom defs
#if defined( __WATCOM_CPLUSPLUS__ ) || defined( __TINYC__ )
  #define atanl atan
  #define sinl  sin
  #define cosl  cos
  #define tanl  tan
  #define asinl asin
  #define acosl acos
  #define log10l log10
  #define logl   log
  #define _fcloseall fcloseall
#endif

// Borland C++ 5.5.1 defs - bcc32.exe
#if defined( __BCPLUSPLUS__ )
  // ===== Borland Libraries ==========
  #include <dos.h>
  #pragma comment(lib,"import32.lib")
  #pragma comment(lib,"cw32.lib")
  // ==================================
#endif

// Microsoft VC++
#ifndef DECLSPEC_UUID
  #if (_MSC_VER >= 1100) && defined ( __cplusplus )
    #define DECLSPEC_UUID(x)    __declspec(uuid(x))
  #else
    #define DECLSPEC_UUID(x)
  #endif
#endif


#if !defined( __LCC__ )
// *************************************************
// Instruct Linker to Search Object/Import Libraries
// *************************************************
#pragma comment(lib,"kernel32.lib")
#pragma comment(lib,"user32.lib")
#pragma comment(lib,"gdi32.lib")
#pragma comment(lib,"comctl32.lib")
#pragma comment(lib,"advapi32.lib")
#pragma comment(lib,"winspool.lib")
#pragma comment(lib,"shell32.lib")
#pragma comment(lib,"ole32.lib")
#pragma comment(lib,"oleaut32.lib")
#pragma comment(lib,"uuid.lib")
#pragma comment(lib,"odbc32.lib")
#pragma comment(lib,"odbccp32.lib")
#pragma comment(lib,"winmm.lib")
#pragma comment(lib,"comdlg32.lib")
#pragma comment(lib,"imagehlp.lib")
#pragma comment(lib,"version.lib")
#else
#pragma lib <winspool.lib>
#pragma lib <shell32.lib>
#pragma lib <ole32.lib>
#pragma lib <oleaut32.lib>
#pragma lib <uuid.lib>
#pragma lib <odbc32.lib>
#pragma lib <odbccp32.lib>
#pragma lib <winmm.lib>
#pragma lib <imagehlp.lib>
#pragma lib <version.lib>
// *************************************************
// End of Object/Import Libraries To Search
// *************************************************
#endif
// *************************************************
//                System Variables
// *************************************************

char    CR  [2]={13,0};  // Carr Rtn
COORD   cursor;
HANDLE  hConsole;
int     color_fg = 7;
int     color_bg = 0;
int     ScanError;
char    InputBuffer[1048576];
jmp_buf GosubStack[32];
int     GosubNdx;

// *************************************************
//            User Global Variables
// *************************************************

static FILE   *FP1;
static FILE   *FP3;
static FILE   *FP2;
static HDC     BcxPtr_hDC;
static long    BcxPtr_FontMetrix;
static long    BcxPtr_LineCtr;
static long    BcxPtr_PrinterOn;
static HFONT   BcxPtr_hFont;
static HFONT   BcxPtr_hFontOld;
static DOCINFO BcxPtr_di;
static LOGFONT BcxPtr_Lf;
static TEXTMETRIC BcxPtr_tm;
static char    BcxPtr_Text[2048];
static char    BcxPtr_Buffer[2048];
static char    DWELL(16)[2048];
static char    FREQD(96)[2048];
static char    OLDFREQD(96)[2048];
static char    INTRPT(16)[2048];
static char    MODE(16)[2048];
static int     FREQ(18,96);
static int     NEWFREQ(96);
static char    HEAD(18)[2048];
static char    FP(96)[2048];
static char    OUTPU(18)[2048];
static int     SCANBIT(18);
static char    C(12)[2048];


// *************************************************
//              User Data Statements
// *************************************************

char * DATA [] =
{
" 0.2"," 0.6"," 1.2","   3","  10","  20","  30","  45",
"  60","  90"," 120"," 180"," 300"," 600","1200"," INF",
"0.10","0.15","0.30","0.50","1.00","2.00","4.00","CONT",
"    ","    ","    ","    ","    ","    ","    ","    ",
};


// *************************************************
//               Standard Macros
// *************************************************

#define BAND &
#define BOR |
#define VAL(a)(double)atof(a)
#define FINT(a)floor(a)


// *************************************************
//               Standard Prototypes
// *************************************************

int     str_cmp(char*, char*);
void    cls(void);
void    color (int,int);
void    locate (int,int,int=1,int=12);
int     Csrlin (void);
void    midstr (char*, int, int, char *);
void    swap (byte*,byte*,int);
char*   BCX_TmpStr(size_t);
char*   mid (char*, int, int=-1);
char*   left (char*,int);
char*   right (char*,int);
char*   string (int,int);
char*   extract (char*,char*);
char*   str (double);
char*   hex (int);
char*   RemoveStr (char*,char*);
char*   join (int, ... );
char*   chr(int,int=0,int=0,int=0,int=0,int=0,int=0,int=0,int=0,int=0);
int     instr(char*,char*,int=0,int=0);
char    *MakeLCaseTbl(void);
char    *_stristr_(char*,char*);
char    *_strstr_(char*,char*);
char*   inkey (void);
int     Screen (int,int,int=0);
int     scan (char *input, char *format, ... );
int     Split (char [][2048], char*, char*, int=0);
int  PrinterOpen  (void);
void PrinterWrite (char*);
void EjectPage    (void);
void PrinterClose (void);

// *************************************************
//               User Prototypes
// *************************************************

C_IMPORT float   __stdcall TICKS(void);
int     TICKS (void);
int     INDEX (int, int);


// *************************************************
//            User Global Initialized Arrays
// *************************************************


// *************************************************
//                  Main Program
// *************************************************

int main(int argc, char *argv[])
{
  hConsole = GetStdHandle (STD_OUTPUT_HANDLE);
COLLEN=16;
COLMIN=1;
NUMCOL(1)=3;
NUMCOL(2)=3;
NUMCOL(3)=3;
NUMCOL(4)=3;
NUMCOL(5)=3;
NUMCOL(6)=3;
NUMCOL(7)=3;
NUMCOL(8)=3;
NUMCOL(9)=3;
COLPOS(1)=1;
COLPOS(2)=7;
COLPOS(3)=13;
COLPOS(4)=19;
COLPOS(5)=25;
COLPOS(6)=31;
COLPOS(7)=40;
COLPOS(8)=48;
COLPOS(9)=58;
PASTEND(1)=21;
PASTEND(2)=21;
PASTEND(3)=21;
PASTEND(4)=21;
PASTEND(5)=21;
PASTEND(6)=21;
PASTEND(7)=7;
PASTEND(8)=21;
PASTEND(9)=13;
for(J=1; J<=16; J+=1)
  {
    READDWELL(J);
  }
for(J=1; J<=16; J+=1)
  {
    READINTRPT(J);
  }
strcpy(MODE(1),"STEP");
strcpy(MODE(2),"SCAN");
if (setjmp(GosubStack[GosubNdx++])==0) goto FREQZERO;
Screen0,0,0;
KEYOFF;
cls();
WIDTH80;
strcpy(CR,chr(13));
locate (1,20);
printf("%s\n","PFG-100A PROGRAMMING SOFTWARE");
locate (2,28);
printf("%s\n","VERSION 1.0");
locate (4,26);
printf("%s\n","PART NUMBER SP1A");
locate (8,1);
printf("%s\n","This program is capable of creating program files for the original PFG100");
printf("%s\n","or for the A version (PFG100A).   The respective limits are:");
printf("%s\n","                                PFG100   PFG100A");
printf("%s\n","Frequency Range              10-10,000  1-10,000 Hz");
printf("%s\n","Maximum # of Programs                9        18");
printf("%s\n","Maximum # of Master Frequencies     64        96");
printf("%s\n","Total of all Program Frequencies   118       264 or more");
printf("\n");
printf("%s\n","A file created within the PFG100 limits can be used on either the PFG100 or");
printf("%s\n","the PFG100A.  If outside the PFG100 limits, a PFG100A is required.");

RES:;
printf("\n");
printf("Is the programming cable connected to COM1(1) or COM2(2) ? > ");
COMM=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%d",&COMM);

*InputBuffer=0;
if(COMM==1)
  {
    LSR= &H3FD;
    MCR= &H3FC;
    LCR= &H3FB;
    strcpy(COMM,"COM1:");
  }
if(COMM==2)
  {
    LSR= &H2FD;
    MCR= &H2FC;
    LCR= &H2FB;
    strcpy(COMM,"COM2:");
  }
_fcloseall();
if((FP1=fopen(join(2,COMM,"2400,N,8,1,CS,DS,RS"),))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,COMM,"2400,N,8,1,CS,DS,RS"));exit(1);
 }
if (setjmp(GosubStack[GosubNdx++])==0) goto FINDTYPE;
if (setjmp(GosubStack[GosubNdx++])==0) goto CAPNUM;
MAINMENU;
cls();

MAINMENU1:;
locate (1,21);
printf("%s\n","MODEL PFG-100A SOFTWARE REV 1.0");
locate (4,25);
printf("%s\n","M A I N   M E N U");
locate (5,25);
printf("%s\n","=================");
locate (8,25);
printf("%s\n","(C) CREATE FILE");
locate (9,25);
printf("%s\n","(E) EDIT FILE");
locate (10,25);
printf("%s\n","(S) SEND FILE");
locate (11,25);
printf("%s\n","(P) PRINT FILE");
locate (12,25);
printf("%s\n","(Q) QUIT");
10;
locate (17,25);
printf("SELECTION ? > ");
*W=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",W);

*InputBuffer=0;
if(W[0]==0)
  {
    10;
  }
if(str_cmp(W,"Q")==0 ||  str_cmp(W,"q")==0)
  {
    DEFSEG= &H40;
    memmove( &H17),STATUSBYTE;
    DEFSEG;
    _fcloseall();
    fflush(stdout);
    ExitProcess(0);
  }
if(str_cmp(W,"S")==0 ||  str_cmp(W,"s")==0)
  {
    strcpy(W,"S");
    if (setjmp(GosubStack[GosubNdx++])==0) goto SENDFILE;
    goto MAINMENU;
  }
if(str_cmp(W,"P")==0 ||  str_cmp(W,"p")==0)
  {
    strcpy(W,"P");
    if (setjmp(GosubStack[GosubNdx++])==0) goto GETFILENAME;
    if (setjmp(GosubStack[GosubNdx++])==0) goto GETFILEDATA;
    if (setjmp(GosubStack[GosubNdx++])==0) goto PRINTDATA;
    goto MAINMENU;
  }
if(str_cmp(W,"E")==0 ||  str_cmp(W,"e")==0)
  {
    strcpy(W,"E");
    if (setjmp(GosubStack[GosubNdx++])==0) goto GETFILENAME;
    if (setjmp(GosubStack[GosubNdx++])==0) goto GETFILEDATA;
    strcpy(ACT,"EDIT");
    goto CREATEDITMENU;
  }
if(str_cmp(W,"C")==0 ||  str_cmp(W,"c")==0)
  {
    strcpy(W,"C");
    strcpy(ACT,"CREATE");
    goto CREATEDITMENU;
  }
locate (19,20);
printf("%s\n","EXPECTING C,E,S,P OR Q");
BEEP;
goto MAINMENU1;

CREATEDITMENU:;
cls();
locate (1,20);
printf("%s%s\n",ACT," FILE MENU");
locate (2,20);
printf("%s\n",string(strlen(ACT)+10,"="));
locate (4,20);
printf("%s%s%s\n","(M) ",ACT," MASTER FREQUENCY LIST");
locate (5,20);
printf("%s%s%s\n","(#) ",ACT," PROGRAM P#(1 - 9/18)");
locate (6,20);
printf("%s\n","(S) SAVE FILE");
locate (7,20);
printf("%s\n","(R) RETURN TO MAIN MENU");

SELECT1:;
locate (10,20);
printf("SELECTION ? > ");
*W=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",W);

*InputBuffer=0;
if(W[0]==0)
  {
    goto SELECT1;
  }
color (7,0);
if(instr("MmSsRr",W)==0)
  {
    W=VAL(W);
    if(W<1||W>18)
      {
        BEEP;
        locate (11,20);
        printf("%s\n","EXPECTING M, 1 - 18, S, R");
        goto SELECT1;
      }
  }
if(str_cmp(W,"M")==0 ||  str_cmp(W,"m")==0)
  {
    if (setjmp(GosubStack[GosubNdx++])==0) goto FREQLIST;
    if (setjmp(GosubStack[GosubNdx++])==0) goto ENTERFREQS;
    if (setjmp(GosubStack[GosubNdx++])==0) goto SORTFREQ;
    goto CREATEDITMENU;
  }
if(str_cmp(W,"S")==0 ||  str_cmp(W,"s")==0)
  {
    goto SAVENEW;
  }
if(str_cmp(W,"R")==0 ||  str_cmp(W,"r")==0)
  {
    goto MAINMENU;
  }
PROGNUM=VAL(W);

PROGRAMENTRY:;
if (setjmp(GosubStack[GosubNdx++])==0) goto PROGLIST;
if (setjmp(GosubStack[GosubNdx++])==0) goto PUTDATA;
if (setjmp(GosubStack[GosubNdx++])==0) goto SELECTITEMS;
if(ADVANCEFLAG==0)
  {
    goto CREATEDITMENU;
  }
PROGNUM+=1;
if(PROGNUM>18)
  {
    PROGNUM=1;
  }
goto PROGRAMENTRY;

GETFILENAME:;
cls();
for(;;)
{
  if(ERROR==1)
    {
      goto 1920;
    }
  break;
}
strcpy(1900IFW,"E"||W="e"THEN);
printf("NAME OF FILE TO EDIT (CR TO CANCEL) ? > ");
*NAMEFILE=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",NAMEFILE);

*InputBuffer=0;
}
else
  {
    printf("NAME OF FILE TO PRINT (CR TO CANCEL) ? > ");
    *NAMEFILE=0;
    gets(InputBuffer);
    ScanError = scan(InputBuffer,"%s",NAMEFILE);

    *InputBuffer=0;
  }
if(NAMEFILE[0]==0)
  {
    goto MAINMENU;
  }
if(FP3)
 {
   fclose(FP3);
   FP3=NULL;
 }
if((FP3=fopen(join(2,NAMEFILE,".P10"),"r"))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,NAMEFILE,".P10"));exit(1);
 }
if(FP3)
 {
   fclose(FP3);
   FP3=NULL;
 }
for(;;)
{
  if(ERROR==1)
    {
      goto 0;
    }
  break;
}
longjmp (GosubStack [--GosubNdx],1 );

1920PRINT;
BEEP;
printf("%s\n","FILE DOES NOT EXIST");
printf("\n");
RESUME1900;

GETFILEDATA:;
for(J=1; J<=18; J+=1)
  {
    *OUTPU(J)=0;
  }
if(FP3)
 {
   fclose(FP3);
   FP3=NULL;
 }
if((FP3=fopen(join(2,NAMEFILE,".P10"),"r"))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,NAMEFILE,".P10"));exit(1);
 }
3=0;
*WW=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%lG%s",&3,WW);

*InputBuffer=0;
if(FP3)
 {
   fclose(FP3);
   FP3=NULL;
 }
W=VAL(join(2,"&H",mid(WW,6,2)));
if(W==0)
  {
    W=VAL(join(2,"&H",mid(WW,8,2)));
  }
if(W==128)
  {
    FILEFMT=0;
  }
else
  {
    FILEFMT=1;
  }
HMFL=63+W-128;
TOTWORDS=(UCHAR)*(mid(WW,3,1))-48;
MSNVADDR=VAL(join(2,"&H",mid(WW,4,2)));
QUITFLAG=0;
LISTSIZE=TOTWORDS-(MSNVADDR-HMFL);
if (setjmp(GosubStack[GosubNdx++])==0) goto FREQZERO;
for(J=1; J<=LISTSIZE; J+=1)
  {
    W9=VAL(join(2,"&H",mid(WW,TOTWORDS*4-J*4+6,4)));
    strcpy(W9,mid(str(W9),2));
    while(strlen(W9)<4)
      {
        strcpy(W9, join(2," ",W9));
      }
    strcpy(FREQD(J),W9);
  }
J=5+4*(MSNVADDR-HMFL);
JEND=J;
J-=3;
PROGNUM=1;
515J=J;
COUNTER=0;
for(;;)
  {
    COUNTER+=1;
    W9=VAL(join(2,"&H",mid(WW,J,2)));
    J-=2;
    if(W9>127)
      {
        break;
      }
  }
if(COUNTER==1)
  {
    goto QUIT;
  }
JSTART=J+4;
strcpy(OUTPU(PROGNUM),mid(WW,JSTART,JEND-JSTART+1));
PROGNUM+=1;
JEND=JSTART-1;
goto 515;

QUIT:;
MAXPROGNUM=PROGNUM-1;
for(PROGNUM=1; PROGNUM<=MAXPROGNUM; PROGNUM+=1)
  {
    L=strlen(OUTPU(PROGNUM));
    SCANBIT(PROGNUM)=FINT(((VAL(join(2,"&H",mid(OUTPU(PROGNUM),L-3,2))) BAND 64)+.5)/64);
    W=VAL(join(2,"&H",mid(OUTPU(PROGNUM),L-3,2))) BAND 63;
    strcpy(W,right(join(2,"0",hex(W)),2));
    midstr(OUTPU(PROGNUM),L-3,2,W);
    if(FILEFMT==1)
      {
        strcpy(OUTPU(PROGNUM), join(2,left(OUTPU(PROGNUM),L-4),right(OUTPU(PROGNUM),2)));
        L=strlen(OUTPU(PROGNUM));
        if(str_cmp(left(OUTPU(PROGNUM),2),"7F")==0)
          {
            strcpy(OUTPU(PROGNUM),right(OUTPU(PROGNUM),L-2));
          }
      }
    strcpy(HEAD(PROGNUM),right(OUTPU(PROGNUM),2));
    for(J=1; J<=96; J+=1)
      {
        FREQ(PROGNUM,J)=0;
      }
    L=strlen(OUTPU(PROGNUM));
    K=HMFL-LISTSIZE;
    for(J=1; J<=L-3; J+=2)
      {
        W=VAL(join(2,"&H",mid(OUTPU(PROGNUM),J,2)));
        FREQ(PROGNUM,W-K)=1;
      }
  }
longjmp (GosubStack [--GosubNdx],1 );


SAVENEW:;
cls();
*FREQSTR=0;
for(J=LISTSIZE; J>=1; J+=-1)
  {
    strcpy(F,hex(VAL(FREQD(J))));
    while(strlen(F)<4)
      {
        strcpy(F, join(2,"0",F));
      }
    strcat(FREQSTR,F);
  }
NUMFREQ=strlen(FREQSTR)/4;
NUMPROGFREQ=0;
MAXPROGNUM=18;
for(PROGNUM=1; PROGNUM<=18; PROGNUM+=1)
  {
    FIRSTFREQFLAG=0;
    for(J=1; J<=96; J+=1)
      {
        if(FREQ(PROGNUM,J)==1)
          {
            NUMPROGFREQ+=1;
            FIRSTFREQFLAG=1;
          }
      }
    if(FIRSTFREQFLAG==0)
      {
        MAXPROGNUM=PROGNUM-1;
        break;
      }
  }
W=NUMPROGFREQ+MAXPROGNUM;
if(Wfmod2==1)
  {
    FILL=1;
  }
else
  {
    FILL=0;
  }
W+=FILL;
PROGWORDS=FINT(W/2)+1;
printf("\n");
printf("%s\n","                        USES   LIMIT");
printf("% .15G% d\n",(double)USING"MASTER FREQUENCIES        ##      64",(int)NUMFREQ);
printf("% .15G% d\n",(double)USING"NUMBER OF PROGRAMS        ##       9",(int)MAXPROGNUM);
printf("% .15G% d% G\n",(double)USING"NO. PROGRAM FREQUENCIES  ###     ###",(int)NUMPROGFREQ,(float)126-MAXPROGNUM-FILL);
printf("\n");
if(NUMFREQ<65)
  {
    HMFL=63;
  }
else
  {
    HMFL=NUMFREQ-1;
  }
if(NUMFREQ>64||MAXPROGNUM>9||PROGWORDS>64)
  {
    printf("%s\n","THIS FILE CAN BE USED ONLY WITH A PFG100A BECAUSE ONE OR MORE OF THE ABOVE");
    printf("%s\n","LIMITS WERE EXCEEDED.");
    printf("\n");
    PR;
    printf("PROCEED WITH A PFG100A FILE (Y/N) ? > ");
    *W=0;
    gets(InputBuffer);
    ScanError = scan(InputBuffer,"%s",W);

    *InputBuffer=0;
    if(str_cmp(W,"Y")!=0 &&  str_cmp(W,"N")!=0)
      {
        BEEP;
        printf("%s\n","EXPECTING Y OR N");
        goto PR;
      }
    if(str_cmp(W,"N")==0)
      {
        goto CREATEDITMENU;
      }
    else
      {
        goto AVERS;
      }
  }
else
  {
    printf("%s\n","THIS FILE CAN BE USED WITH AN ORIGINAL PFG100 OR A PFG100A");
  }
*OUTP=0;
K=HMFL-LISTSIZE;
for(PROGNUM=1; PROGNUM<=MAXPROGNUM; PROGNUM+=1)
  {
    *OUTPU(PROGNUM)=0;
    for(J=64; J>=1; J+=-1)
      {
        if(FREQ(PROGNUM,J)==1)
          {
            strcpy(W,right(join(2,"0",hex(J+K)),2));
            strcpy(OUTPU(PROGNUM), join(2,OUTPU(PROGNUM),W));
          }
      }
    if(SCANBIT(PROGNUM)==1)
      {
        L=strlen(OUTPU(PROGNUM));
        W=VAL(join(2,"&H",mid(OUTPU(PROGNUM),L-1,2)))+64;
        strcpy(W,hex(W));
        midstr(OUTPU(PROGNUM),L-1,2,W);
      }
    strcpy(OUTPU(PROGNUM), join(2,OUTPU(PROGNUM),HEAD(PROGNUM)));
    strcpy(OUTP, join(2,OUTPU(PROGNUM),OUTP));
  }
strcpy(OUTP, join(2,"8080",OUTP));
if(strlen(OUTP)fmod4!=0)
  {
    strcpy(OUTP, join(2,"00",OUTP));
  }
goto FINAL;

AVERS:;
*OUTP=0;
EXTRA=0;
if(HMFL==63)
  {
    HMFL=64;
  }
K=HMFL-LISTSIZE;
for(PROGNUM=1; PROGNUM<=MAXPROGNUM; PROGNUM+=1)
  {
    *OUTPU(PROGNUM)=0;
    for(J=96; J>=1; J+=-1)
      {
        if(FREQ(PROGNUM,J)==1)
          {
            strcpy(W,right(join(2,"0",hex(J+K)),2));
            strcpy(OUTPU(PROGNUM), join(2,OUTPU(PROGNUM),W));
          }
      }
    if(strlen(OUTPU(PROGNUM))fmod4!=0)
      {
        strcpy(OUTPU(PROGNUM), join(2,"7F",OUTPU(PROGNUM)));
        EXTRA+=1;
      }
    if(SCANBIT(PROGNUM)==1)
      {
        strcpy(S,"40");
      }
    else
      {
        strcpy(S,"00");
      }
    strcpy(OUTPU(PROGNUM), join(3,OUTPU(PROGNUM),S,HEAD(PROGNUM)));
    strcpy(OUTP, join(2,OUTPU(PROGNUM),OUTP));
  }
W=128+HMFL-63;
strcpy(W,hex(W));
strcpy(OUTP, join(3,W,"80",OUTP));
L=strlen(OUTP);
PROGWORDS=FINT((L+.5)/4);
printf("\n");
printf("%s\n","                        USES   LIMIT");
printf("% .15G% d\n",(double)USING"MASTER FREQUENCIES        ##      96",(int)NUMFREQ);
printf("% .15G% d\n",(double)USING"NUMBER OF PROGRAMS        ##      18",(int)MAXPROGNUM);
printf("% .15G% d% G\n",(double)USING"NO. PROGRAM FREQUENCIES  ###     ###",(int)NUMPROGFREQ,(float)511-2*HMFL-J9*2-2-EXTRA);
if((NUMFREQ>96)||(MAXPROGNUM>18)||(PROGWORDS>(255-HMFL-MAXPROGNUM-1-EXTRA/2)))
  {
    printf("%s\n","THIS FILE CANNOT BE USED WITH A PFG100A BECAUSE ONE OR MORE OF THE ABOVE");
    printf("%s\n","LIMITS WERE EXCEEDED.");
    printf("\n");
    printf("%s\n","HIT ANY KEY TO RETURN TO THE MENU");
    PQ;
    strcpy(W,inkey());
    if(W[0]==0)
      {
        goto PQ;
      }
    goto CREATEDITMENU;
  }
else
  {
    printf("%s\n","THIS FILE CAN BE USED ONLY WITH A PFG100A");
  }

FINAL:;
NUMPROGWORDS=strlen(OUTP)/4;
MSWORD=HMFL+NUMPROGWORDS;
strcpy(MSWORD,right(hex(MSWORD),2));
NUMWORDS=NUMPROGWORDS+LISTSIZE;
strcpy(NUMWORDS,chr(48+NUMWORDS));
strcpy(OUTP, join(5,"*W",NUMWORDS,MSWORD,OUTP,FREQSTR));
5000PRINT;
printf("NAME OF FILE TO SAVE DATA UNDER (CR TO CANCEL) ?  > ");
*NAMEFILE=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",NAMEFILE);

*InputBuffer=0;
if(NAMEFILE[0]==0)
  {
    goto CREATEDITMENU;
  }
for(;;)
{
  if(ERROR==1)
    {
      goto 5020;
    }
  break;
}
if((FP3=fopen(join(2,NAMEFILE,".P10"),"r"))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,NAMEFILE,".P10"));exit(1);
 }
printf("\n");
BEEP;
printf("%s%s\n",NAMEFILE," ALREADY EXISTS.  FILE DATA WILL BE REPLACED WITH NEW DATA.");
5005PRINT;
printf("PROCEED (Y/N) ? ");
*W=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",W);

*InputBuffer=0;
if(instr("YyNn",W)==0)
  {
    printf("\n");
    printf("%s\n","EXPECTING Y OR N");
    goto 5005;
  }
if(str_cmp(W,"Y")!=0 &&  str_cmp(W,"y")!=0)
  {
    if(FP3)
     {
       fclose(FP3);
       FP3=NULL;
     }
    goto 5000;
  }
5010CLOSE3;
if((FP3=fopen(join(2,NAMEFILE,".P10"),"w"))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,NAMEFILE,".P10"));exit(1);
 }
fprintf(FP3,"%s\n",OUTP);
if(FP3)
 {
   fclose(FP3);
   FP3=NULL;
 }
for(;;)
{
  if(ERROR==1)
    {
      goto 0;
    }
  break;
}
goto MAINMENU;
5020RESUME5010;

SENDFILE:;
cls();
for(;;)
{
  if(ERROR==1)
    {
      goto 1700;
    }
  break;
}
1350NOFILE=0;
printf("\n");
printf("NAME OF FILE TO SEND (CR TO CANCEL) ? > ");
*FILE=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%s",FILE);

*InputBuffer=0;
if(FILE[0]==0)
  {
    goto MAINMENU;
  }
1351CLOSE2;
if((FP2=fopen(join(2,FILE,".P10"),"r"))==0)
 {
fprintf(stderr,"Can't open file %s\n",join(2,FILE,".P10"));exit(1);
 }
if(NOFILE==1)
  {
    printf("\n");
    printf("%s\n","FILE NOT FOUND!");
    goto 1350;
  }
2=0;
*DAT=0;
gets(InputBuffer);
ScanError = scan(InputBuffer,"%lG%s",&2,DAT);

*InputBuffer=0;
printf("\n");
printf("%s\n","HIT ANY KEY WHEN READY TO SEND DATA");
printf("\n");
strcpy(1454IFinkey(),""THEN1454);
color (23,0);
printf("\n");
CL=Csrlin();
printf("%s\n","DATA TRANSMISSION IN PROCESS");
color (7,0);
printf("\n");
printf("%s\n","*** PFG-100 DISPLAY SHOULD BE BLANK WHILE DATA BEING RECEIVED ***");
NWORDS=(UCHAR)*(mid(DAT,3,1))-48;
strcpy(STARTNVADDR,mid(DAT,4,2));
STARTNVADDR=VAL(join(2,"&H",STARTNVADDR));
strcpy(DAT,mid(DAT,6));
while(NWORDS>32)
  {
    strcpy(P, join(4,"*P",chr(32+48),STARTNVADDR,mid(DAT,1,128)));
    fprintf(FP1,"%s\n",P);
    NWORDS-=32;
    STARTNVADDR-=32;
    strcpy(STARTNVADDR,hex(STARTNVADDR));
    if(strlen(STARTNVADDR)<2)
      {
        strcpy(STARTNVADDR, join(2,"0",STARTNVADDR));
      }
    strcpy(DAT,mid(DAT,129));
    W=TICKS;
    W+=30;
    if(W>65535)
      {
        W-=65535;
      }
    while((TICKS<W)&&((W-TICKS)<1000))
      {
      }
  }
strcpy(F, join(4,"*F",chr(NWORDS+48),STARTNVADDR,DAT));
fprintf(FP1,"%s\n",F);
printf("\n");
if(FP2)
 {
   fclose(FP2);
   FP2=NULL;
 }
locate (CL,1);
printf("%s\n","DATA TRANSMISSION IN PROCESS !");
printf("\n");
printf("\n");
printf("\n");
printf("\n");
printf("%s\n","DATA TRANSMISSION COMPLETE !");
printf("\n");
printf("%s\n","*** PFG-100 DISPLAY SHOULD READ 'P 1' ***");
printf("\n");
printf("\n");
printf("%s\n","HIT ESC KEY TO RETURN TO MAIN MENU");
printf("\n");
printf("%s\n","HIT ANY OTHER KEY TO REPEAT SEND");
strcpy(1520W,inkey());
if(W[0]==0)
  {
    1520;
  }
if(str_cmp(W,chr(27))!=0)
  {
    cls();
    goto 1351;
  }
for(;;)
{
  if(ERROR==1)
    {
      goto 0;
    }
  break;
}
goto MAINMENU;
fflush(stdout);
ExitProcess(0);
1700NOFILE=1;
RESUMENEXT;

FLASHPOS:;
X=COLPOS(COLUMN);
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    ASK(X1)=Screen(Y,X+X1);
  }
if(Screen(Y,X,1)==7)
  {
    color (23,0    );
  }
else
  {
    color (16,7    );
  }
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    locate (Y,X+X1);
    printf("%s",chr(ASK(X1)));
  }
X1=0;
color (7,0);
longjmp (GosubStack [--GosubNdx],1 );


NOFLASH:;
if(Screen(Y,X,1)fmod16==7)
  {
    color (7,0    );
  }
else
  {
    color (0,7    );
  }
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    locate (Y,X+X1);
    printf("%s",chr(ASK(X1)));
  }
X1=0;
color (7,0);
longjmp (GosubStack [--GosubNdx],1 );


FREQZEROENT:;
if (setjmp(GosubStack[GosubNdx++])==0) goto FREQZERO;

FREQLIST:;
for(J=1; J<=96; J+=1)
  {
    strcpy(OLDFREQD(J),FREQD(J));
  }
for(J=COLMIN; J<=6; J+=1)
  {
    PASTEND(J)=5+COLLEN;
  }
cls();
if(TYPEV==0)
  {
    color (1,0    );
  }
else
  {
    color (7,1    );
  }
locate (1,10);
printf("%s\n","MASTER FREQUENCY LIST");
color (7,0);
locate (3,1);
printf("%s\n","FREQ  FREQ  FREQ  FREQ  FREQ  FREQ");
if(TYPEV==0)
  {
    color (1,0    );
  }
else
  {
    color (7,1    );
  }
locate (4,1);
printf("%s\n"," HZ    HZ    HZ    HZ    HZ    HZ");
color (7,0);
OLDLISTSIZE=0;
for(Y=5; Y<=20; Y+=1)
  {
    for(COLUMN=COLMIN; COLUMN<=6; COLUMN+=1)
      {
        X=6*COLUMN-5;
        if(str_cmp(FREQD(INDEX(Y,COLUMN)),"????")!=0)
          {
            color (0,7            );
            OLDLISTSIZE+=1;
          }
        locate (Y,X);
        printf("%s\n",FREQD(INDEX(Y,COLUMN)));
        color (7,0        );
      }
  }
locate (25,1);
printf("%s","HIT 'Z' TO ZERO DATA OR 'R' TO RETURN TO MENU");
longjmp (GosubStack [--GosubNdx],1 );


ENTERFREQS:;
Y=5;
COLUMN=COLMIN;
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
strcpy(9280W,inkey());
if(W[0]==0)
  {
    9280;
  }
if(strlen(W)==2)
  {
    W=(UCHAR)*(right(W,1))+1000;
  }
else
  {
    W=(UCHAR)*(W);
  }
if(W==1077)
  {
    goto RIGHTARROWS;
  }
if(W==1075)
  {
    goto LEFTARROWS;
  }
if(W==1072)
  {
    goto UPARROWS;
  }
if(W==1080)
  {
    goto DOWNARROWS;
  }
if(W==90||W==122)
  {
    if (setjmp(GosubStack[GosubNdx++])==0) goto FREQZEROENT;
    goto ENTERFREQS;
  }
if(W==82||W==114)
  {
    if (setjmp(GosubStack[GosubNdx++])==0) goto READFREQ;
    longjmp (GosubStack [--GosubNdx],1 );

  }
if(W==13)
  {
    goto ALTERNATES;
  }
if(((W>47&&W<58)||W==8))
  {
    if (setjmp(GosubStack[GosubNdx++])==0) goto CHANGEFREQ;
    goto 9280;
  }
BEEP;
goto 9280;
fflush(stdout);
ExitProcess(0);

RIGHTARROWS:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;

INCCOLS:;
COLUMN+=1;
if(COLUMN>6)
  {
    COLUMN=COLMIN;
  }
if(Y>=PASTEND(COLUMN))
  {
    goto INCCOLS;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 9280;

LEFTARROWS:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;

DECCOLS:;
COLUMN-=1;
if(COLUMN<COLMIN)
  {
    COLUMN=6;
  }
if(Y>=PASTEND(COLUMN))
  {
    goto DECCOLS;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 9280;

UPARROWS:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;
Y-=1;
if(Y==4)
  {
    Y=PASTEND(COLUMN)-1;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 9280;

DOWNARROWS:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;
Y+=1;
if(Y==PASTEND(COLUMN))
  {
    Y=5;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 9280;

CHANGEFREQ:;
if(W==8)
  {
    X1-=1;
    if(X1<0)
      {
        BEEP;
        X1=0;
      }
    if(X1==0)
      {
        locate (Y,X);
        printf("%s\n",chr(32));
      }
    else
      {
        for(I=X1; I<=3; I+=1)
          {
            locate (Y,X+I);
            printf("%s\n",chr(32));
            ASK(I)=32;
          }
      }
  }
else
  {
    if(X1>3)
      {
        X1=3;
      }
    locate (Y,X+X1);
    printf("%s",chr(W));
    ASK(X1)=Screen(Y,X+X1);
    X1+=1;
    if(X1<3)
      {
        for(I=X1; I<=3; I+=1)
          {
            locate (Y,X+I);
            printf("%s\n",chr(32));
            ASK(I)=32;
          }
      }
  }
longjmp (GosubStack [--GosubNdx],1 );


ALTERNATES:;
if(ASK(3)==63)
  {
    goto DOWNARROWS;
  }
for(;;)
  {
    if(ASK(3)!=32)
      {
        break;
      }
    for(I=3; I>=1; I+=-1)
      {
        ASK(I)=ASK(I-1);
      }
    ASK(0)=32;
  }
if(Screen(Y,X,1)fmod16==7)
  {
    color (0,7    );
  }
else
  {
    color (7,0    );
  }
*9290W1=0;
LZFLAG=0;
for(X1=0; X1<=3; X1+=1)
  {
    if(ASK(X1)==48&&LZFLAG==0)
      {
        ASK(X1)=32;
      }
    else
      {
        LZFLAG=1;
      }
    locate (Y,X+X1);
    printf("%s",chr(ASK(X1)));
    strcat(W1,chr(ASK(X1)));
  }
X1=0;
color (7,0);
goto DOWNARROWS;

READFREQ:;
locate (23,1);
printf("%s","WAIT___");
LISTSIZE=0;
*OUTP=0;
for(COLUMN=COLMIN; COLUMN<=6; COLUMN+=1)
  {
    X=COLPOS(COLUMN);
    for(Y=5; Y<=COLLEN+4; Y+=1)
      {
        if(Screen(Y,X,1)fmod16==0)
          {
            LISTSIZE+=1;
            *ASK=0;
            for(X1=0; X1<=3; X1+=1)
              {
                strcat(ASK,chr(Screen(Y,X+X1)));
              }
            strcpy(FREQD(INDEX(Y,COLUMN)),ASK);
          }
        else
          {
            strcpy(FREQD(INDEX(Y,COLUMN)),"????");
          }
      }
  }
longjmp (GosubStack [--GosubNdx],1 );


SORTFREQ:;
FLAG=0;
for(J=1; J<=95; J+=1)
  {
    if(str_cmp(FREQD(J+1),FREQD(J))==-1)
      {
        swap ((byte*)&FREQD(J),(byte*)&FREQD(J+1),strlen(FREQD(J+1)));
        FLAG=1;
      }
  }
if(FLAG==1)
  {
    goto SORTFREQ;
  }
J=1;

NODUPS:;
if(LISTSIZE<2)
  {
    longjmp (GosubStack [--GosubNdx],1 );

  }
if(str_cmp(FREQD(J+1),FREQD(J))==0)
  {
    strcpy(FREQD(J+1),"????");
    LISTSIZE-=1;
    goto SORTFREQ;
  }
J+=1;
if(J<LISTSIZE)
  {
    goto NODUPS;
  }
for(PROGNUM=1; PROGNUM<=MAXPROGNUM; PROGNUM+=1)
  {
    for(J=1; J<=LISTSIZE; J+=1)
      {
        NEWFREQ(J)=0;
      }
    for(J=1; J<=OLDLISTSIZE; J+=1)
      {
        if(FREQ(PROGNUM,J)==1)
          {
            strcpy(W,OLDFREQD(J));
            for(K=1; K<=LISTSIZE; K+=1)
              {
                if(str_cmp(FREQD(K),W)==0)
                  {
                    NEWFREQ(K)=1;
                  }
              }
          }
      }
    for(J=1; J<=LISTSIZE; J+=1)
      {
        FREQ(PROGNUM,J)=NEWFREQ(J);
      }
  }
longjmp (GosubStack [--GosubNdx],1 );


PROGLIST:;
for(J=COLMIN; J<=6; J+=1)
  {
    PASTEND(J)=5+COLLEN;
  }
cls();
if(TYPEV==0)
  {
    color (1,0    );
  }
else
  {
    color (7,1    );
  }
locate (1,18);
printf("% .15G% d\n",(double)USING"PROGRAM NUMBER ##",(int)PROGNUM);
color (7,0);
printf("\n");
locate (2,1);
printf("%s\n","                                                          ON/");
locate (3,1);
printf("%s\n","FREQ  FREQ  FREQ  FREQ  FREQ  FREQ     MODE    DWELL      OFF");
if(TYPEV==0)
  {
    color (1,0    );
  }
else
  {
    color (7,1    );
  }
locate (4,1);
printf("%s\n"," HZ    HZ    HZ    HZ    HZ    HZ               SEC       SEC");
color (7,0);
for(Y=5; Y<=20; Y+=1)
  {
    for(COLUMN=COLMIN; COLUMN<=9; COLUMN+=1)
      {
        X=COLPOS(COLUMN);
        if(COLUMN<7)
          {
            strcpy(W,FREQD(INDEX(Y,COLUMN)));
            if(str_cmp(W,"????")==0)
              {
                strcpy(W,"    ");
                if(Y<PASTEND(COLUMN))
                  {
                    PASTEND(COLUMN)=Y;
                  }
              }
            locate (Y,X);
            printf("% G%s\n",(float)USING"\\  \\",W);
          }
        if(COLUMN==7)
          {
            locate (Y,X);
            printf("%s\n",MODE(Y-4));
          }
        if(COLUMN==8)
          {
            locate (Y,X);
            printf("%s\n",DWELL(Y-4));
          }
        if(COLUMN==9)
          {
            locate (Y,X);
            printf("%s\n",INTRPT(Y-4));
          }
      }
  }
locate (5,44);
printf("%s\n","*");
locate (8,52);
printf("%s\n","*");
locate (6,62);
printf("%s\n","*");
locate (22,1);
printf("%s\n","* Default values for unselected parameters");
locate (25,1);
printf("%s","HIT 'Z' TO ZERO DATA, 'R' TO RETURN TO MENU, 'A' TO ADVANCE TO NEXT PROGRAM");
longjmp (GosubStack [--GosubNdx],1 );


PUTDATA:;
W9=VAL(join(2,"&H",HEAD(PROGNUM)));
W0=W9 BAND 15;
COLUMN=8;
Y=W0+5;
if (setjmp(GosubStack[GosubNdx++])==0) goto REVERSE;
W1=FINT((W9 BAND 112)/16+.5);
COLUMN=9;
Y=W1+5;
if (setjmp(GosubStack[GosubNdx++])==0) goto REVERSE;
for(J=1; J<=LISTSIZE; J+=1)
  {
    if(FREQ(PROGNUM,J)==1)
      {
        COLUMN=COLMIN+FINT((J-.5)/COLLEN);
        Y=((J-1)fmodCOLLEN)+5;
        if((Y>4&&Y<(COLLEN+5)))
          {
            if (setjmp(GosubStack[GosubNdx++])==0) goto REVERSE;
          }
      }
  }
COLUMN=7;
Y=SCANBIT(PROGNUM)+5;
if (setjmp(GosubStack[GosubNdx++])==0) goto REVERSE;
longjmp (GosubStack [--GosubNdx],1 );


REVERSE:;
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
VALFLAG=0;
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    if(ASK(X1)!=32)
      {
        VALFLAG=1;
      }
  }
if(VALFLAG==0)
  {
    longjmp (GosubStack [--GosubNdx],1 );

  }
color (0,7);
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    locate (Y,X+X1);
    printf("%s",chr(ASK(X1)));
  }
X1=0;
color (7,0);
longjmp (GosubStack [--GosubNdx],1 );


SELECTITEMS:;
ADVANCEFLAG=0;
Y=5;
COLUMN=COLMIN;
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
3280LOCATE23,1;
printf("%s\n",string(75," "));
strcpy(3281W,inkey());
if(W[0]==0)
  {
    3281;
  }
ERRORTYP=0;
if(strlen(W)==2)
  {
    W=(UCHAR)*(right(W,1))+1000;
  }
else
  {
    W=(UCHAR)*(W);
  }
if(W==1077)
  {
    goto RIGHTARROW;
  }
if(W==1075)
  {
    goto LEFTARROW;
  }
if(W==1072)
  {
    goto UPARROW;
  }
if(W==1080)
  {
    goto DOWNARROW;
  }
if(W==13)
  {
    goto ALTERNATE;
  }
if(W==90||W==122)
  {
    if (setjmp(GosubStack[GosubNdx++])==0) goto BLANKDATA;
    goto SELECTITEMS;
  }
if(W==82||W==114)
  {
    ADVANCEFLAG=0;
    goto TESTERR;
  }
if(W==65||W==78||W==97||W==110)
  {
    ADVANCEFLAG=1;
    goto TESTERR;
  }
BEEP;
goto 3280;

TESTERR:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;
if (setjmp(GosubStack[GosubNdx++])==0) goto READSCRN;
if(ERRORTYP==0)
  {
    longjmp (GosubStack [--GosubNdx],1 );

  }
if (setjmp(GosubStack[GosubNdx++])==0) goto PRINTERR;
Y=5;
COLUMN=COLMIN;
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 3281;

RIGHTARROW:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;

INCCOL:;
COLUMN+=1;
if(COLUMN>9)
  {
    COLUMN=COLMIN;
  }
if(Y>=PASTEND(COLUMN))
  {
    goto INCCOL;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 3280;

LEFTARROW:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;

DECCOL:;
COLUMN-=1;
if(COLUMN<COLMIN)
  {
    COLUMN=9;
  }
if(Y>=PASTEND(COLUMN))
  {
    goto DECCOL;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 3280;

UPARROW:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;
Y-=1;
if(Y==4)
  {
    Y=PASTEND(COLUMN)-1;
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 3280;

DOWNARROW:;
if (setjmp(GosubStack[GosubNdx++])==0) goto NOFLASH;
Y+=1;
if(Y==PASTEND(COLUMN))
  {
    Y=5;
  }
else if(COLUMN<5)
  {
    if(str_cmp(FREQD(INDEX(Y,COLUMN)),"????")==0)
      {
        Y=5;
      }
  }
if (setjmp(GosubStack[GosubNdx++])==0) goto FLASHPOS;
goto 3280;

BLANKDATA:;
*OUTPU(PROGNUM)=0;
if (setjmp(GosubStack[GosubNdx++])==0) goto PROGLIST;
longjmp (GosubStack [--GosubNdx],1 );


PRINTERR:;
BEEP;
locate (23,1);
color (23,0);
for(;;)
{
  if(ERRORTYP==1)
    {
      printf("%s","ERROR");
      color (7,0      );
      printf("%s\n"," - SELECT ONLY 1 PARAMETER EACH FROM THE RIGHT 3 COLUMNS");
      break;
    }
  if(ERRORTYP==2)
    {
      printf("%s","ERROR");
      color (7,0      );
      printf("%s\n"," - SELECT EXACTLY 2 FREQUENCIES IN THE SCAN MODE");
    }
  break;
}
longjmp (GosubStack [--GosubNdx],1 );


ALTERNATE:;
if(Screen(Y,X,1)fmod16==7)
  {
    color (16,7    );
  }
else
  {
    color (23,0    );
  }
for(X1=0; X1<=NUMCOL(COLUMN); X1+=1)
  {
    locate (Y,X+X1);
    printf("%s",chr(ASK(X1)));
  }
X1=0;
color (7,0);
goto DOWNARROW;

READSCRN:;
*OUTP=0;
NUMFREQSELECTED=0;
NDWELL=0;
NINTR=0;
NMODE=0;
SCANBIT(PROGNUM)=0;
for(J=1; J<=96; J+=1)
  {
    FREQ(PROGNUM,J)=0;
  }
for(COLUMN=COLMIN; COLUMN<=9; COLUMN+=1)
  {
    X=COLPOS(COLUMN);
    for(Y=5; Y<=COLLEN+4; Y+=1)
      {
        if(Screen(Y,X,1)fmod16==0)
          {
            if(COLUMN==7)
              {
                strcpy(MODE,Y-5);
                NMODE+=1;
              }
            else if(COLUMN==8)
              {
                strcpy(DWELL,Y-5);
                NDWELL+=1;
              }
            else if(COLUMN==9)
              {
                INTR=Y-5;
                NINTR+=1;
              }
            else
              {
                FREQ(PROGNUM,INDEX(Y,COLUMN))=1;
                NUMFREQSELECTED+=1;
              }
          }
      }
  }
if(NUMFREQSELECTED==0)
  {
    longjmp (GosubStack [--GosubNdx],1 );

  }
if(NDWELL==0)
  {
    strcpy(DWELL,3);
  }
if(NINTR==0)
  {
    INTR=1;
  }
if(NMODE==0)
  {
    strcpy(MODE,0);
  }
if(NDWELL>1||NINTR>1||NMODE>1)
  {
    ERRORTYP=1;
    longjmp (GosubStack [--GosubNdx],1 );

  }
if(MODE==1&&NUMFREQSELECTED!=2)
  {
    ERRORTYP=2;
    longjmp (GosubStack [--GosubNdx],1 );

  }
SCANBIT(PROGNUM)=MODE;
strcpy(HEAD,128+16*INTR+DWELL);
strcpy(HEAD(PROGNUM),hex(HEAD));
longjmp (GosubStack [--GosubNdx],1 );


PRINTDATA:;
sprintf(BcxPtr_Buffer,"%s%s%s",string(65," "),"FILE= ",NAMEFILE);
PrinterWrite(BcxPtr_Buffer);
sprintf(BcxPtr_Buffer,"%s","MASTER FREQUENCY LIST");
PrinterWrite(BcxPtr_Buffer);
sprintf(BcxPtr_Buffer,"%s","---------------------");
PrinterWrite(BcxPtr_Buffer);
NUMFREQ=LISTSIZE;
PAGEROWS=3;
if (setjmp(GosubStack[GosubNdx++])==0) goto PRINTMFREQ;
for(PROGNUM=1; PROGNUM<=MAXPROGNUM; PROGNUM+=1)
  {
    strcpy(W,OUTPU(PROGNUM));
    W9=VAL(join(2,"&H",HEAD(PROGNUM)));
    W2=SCANBIT(PROGNUM);
    W0=W9 BAND 15;
    W1=FINT((W9 BAND 112)/16+.5);
    sprintf(BcxPtr_Buffer,"% .15G% d%s%s%s",(double)USING"PROGRAM ##    \\  \\/\\  \\/\\  \\",(int)PROGNUM,MODE(W2+1),DWELL(W0+1),INTRPT(W1+1));
    PrinterWrite(BcxPtr_Buffer);
    PAGEROWS+=1;
    NUMFREQ=0;
    K=0;
    for(J=1; J<=LISTSIZE; J+=1)
      {
        if(FREQ(PROGNUM,J)==1)
          {
            K+=1;
            strcpy(FP(K),FREQD(J));
            NUMFREQ+=1;
          }
      }
    if (setjmp(GosubStack[GosubNdx++])==0) goto PRINTFREQ;
  }
longjmp (GosubStack [--GosubNdx],1 );


PRINTMFREQ:;
for(J=1; J<=NUMFREQ; J+=1)
  {
    strcpy(FP(J),FREQD(J));
  }

PRINTFREQ:;
NUMROW=FINT((NUMFREQ-.5)/12)+1;
PAGEROWS+=NUMROW;
if(PAGEROWS>62)
  {
    sprintf(BcxPtr_Buffer,"%s",chr(12));
    PrinterWrite(BcxPtr_Buffer);
    PAGEROWS=0;
  }
for(RROW=1; RROW<=NUMROW; RROW+=1)
  {
    if(RROW!=NUMROW)
      {
        for(CCOL=1; CCOL<=12; CCOL+=1)
          {
            strcpy(C(CCOL),FP(12*(RROW-1)+CCOL));
          }
      }
    else
      {
        for(K1=1; K1<=12; K1+=1)
          {
            *C(K1)=0;
          }
        for(CCOL=1; CCOL<=(NUMFREQ-1)fmod12+1; CCOL+=1)
          {
            strcpy(C(CCOL),FP(12*(RROW-1)+CCOL));
          }
      }
    sprintf(BcxPtr_Buffer,"% G%s%s%s%s%s%s%s%s%s%s%s%s",(float)USING"  \\  \\",C(1),C(2),C(3),C(4),C(5),C(6),C(7),C(8),C(9),C(10),C(11),C(12));
    PrinterWrite(BcxPtr_Buffer);
  }
longjmp (GosubStack [--GosubNdx],1 );


FINDTYPE:;
DEFSEG= &H40;
X=PEEK( &H87);
if(((X!=0)&&((X&&8)==0)))
  {
    TYPEV=2;
  }
else
  {
    for(;;)
    {
      if(PEEK( &H10)&&48==48)
        {
          TYPEV=0;
          break;
        }
      if(PEEK( &H10)&&48==32)
        {
          TYPEV=1;
          break;
        }
        // case else
        {
          TYPEV=3;
        }
      break;
    }
  }
DEFSEG;
longjmp (GosubStack [--GosubNdx],1 );


CAPNUM:;
DEFSEG= &H40;
STATUSBYTE=PEEK( &H17);
NEWBYTE=(STATUSBYTE BAND 223) BOR 64;
memmove( &H17),NEWBYTE;
DEFSEG;
longjmp (GosubStack [--GosubNdx],1 );


FREQZERO:;
for(J=1; J<=96; J+=1)
  {
    strcpy(FREQD(J),"????");
  }
longjmp (GosubStack [--GosubNdx],1 );

  return 0;   //  End of main program
}

// *************************************************
//                 Runtime Functions
// *************************************************

char *BCX_TmpStr (size_t Bites)
{
  static int   StrCnt;
  static char *StrFunc[2048];
  StrCnt=(StrCnt + 1) & 2047;
  if(StrFunc[StrCnt]) free (StrFunc[StrCnt]);
  return StrFunc[StrCnt]=(char*)calloc(Bites+128,sizeof(char));
}


int str_cmp (char *a, char *b)
{
  register int counter=0;
  for(;;)
   {
    if((a[counter]^b[counter]))
     {
      if((UINT) a[counter]>= (UINT) b[counter])
      return  1;
      return -1;
     }
    if(!a[counter]) return 0;
    counter++;
   }
#if !defined( __cplusplus )
 return 0;
#endif
}


char *left (char *S, int length)
{
  register int tmplen = strlen(S);
  if(length<1) return BCX_TmpStr(1);
  if(length<tmplen) tmplen=length;
  char *strtmp = BCX_TmpStr(tmplen);
  return (char*)memcpy(strtmp,S,tmplen*sizeof(char));
}


char *right (char *S, int length)
{
  int tmplen = strlen(S);
  char *BCX_RetStr = BCX_TmpStr(tmplen);
  tmplen -= length;
  if (tmplen<0) tmplen = 0;
  return strcpy(BCX_RetStr, &S[tmplen]);
}


char *mid (char *S, int start, int length)
{
  char *strtmp;
  register int tmplen = strlen(S);
  if(start>tmplen||start<1) return BCX_TmpStr(1);
  if (length<0 || length>(tmplen-start)+1)
    length = (tmplen-start)+1;
  strtmp = BCX_TmpStr(length);
  return (char*)memcpy(strtmp,&S[start-1],length*sizeof(char));
}


char *extract (char *mane, char *match)
{
  register char *a;
  register char *strtmp = BCX_TmpStr(strlen(mane));
  if(*match!=0)
    {
      a=_strstr_(mane,match);
      if(a) return (char*)memcpy(strtmp,mane,a-mane);
    }
  return strcpy(strtmp,mane);
}


char *RemoveStr (char *a, char *b)
{
  char *strtmp, *p, *d;
  int  tmplen;
  strtmp = d = BCX_TmpStr(strlen(a));
  if(!b || !*b) return strcpy(strtmp,a);
  p=_strstr_(a,b); tmplen = strlen(b);
  while(p)
   {
     memcpy(d,a,p-a);
     d+= (p-a);
     a=p+tmplen;
     p=_strstr_(a,b);
   }
  strcpy(d,a);
  return strtmp;
}


char *str (double d)
{
  register char *strtmp = BCX_TmpStr(16);
  sprintf(strtmp,"% .15G",d);
  return strtmp;
}


char *hex (int a)
{
  register char *strtmp = BCX_TmpStr(16);
  sprintf(strtmp,"%X",a);
  return strtmp;
}


char *string (int count, int a)
{
  if(count<1) return BCX_TmpStr(1);
  register char *strtmp = BCX_TmpStr(count);
  return (char*)memset(strtmp,a,count);
}


char *chr (int a,int b,int c,int d,int e,int f,int g,int h,int i,int j)
{
  register char *strtmp = BCX_TmpStr(11);
  strtmp[0]  = a;
  strtmp[1]  = b;
  strtmp[2]  = c;
  strtmp[3]  = d;
  strtmp[4]  = e;
  strtmp[5]  = f;
  strtmp[6]  = g;
  strtmp[7]  = h;
  strtmp[8]  = i;
  strtmp[9]  = j;
  strtmp[10] = 0;
  return strtmp;
}


char * join(int n, ...)
{
  register int i = n, tmplen = 0;
  register char *s_;
  register char *strtmp;
  va_list marker;
  va_start(marker, n); // Initialize variable arguments
  while(i-- > 0)
  {
    s_ = va_arg(marker, char *);
    if(s_) tmplen += strlen(s_);
  }
  strtmp = BCX_TmpStr(tmplen);
  va_end(marker); // Reset variable arguments
  i = n;
  va_start(marker, n); // Initialize variable arguments
  while(i-- > 0)
  {
    s_ = va_arg(marker, char *);
    if(s_) strcat(strtmp, s_);
  }
  va_end(marker); // Reset variable arguments
  return strtmp;
}


char* inkey(void)
{
  char *strtmp = BCX_TmpStr(2);
  if (kbhit())
   {
     int asccodereturn = getch();
     strtmp[0] = asccodereturn;
     strtmp[1] = 0;
   }
  return strtmp;
}


int instr(char* mane,char* match,int offset,int sensflag)
{
  register char *s;
  if (!mane || !match || ! *match || offset>(int)strlen(mane)) return 0;
  if (sensflag)
    s = _stristr_(offset>0 ? mane+offset-1 : mane,match);
  else
    s = _strstr_(offset>0 ? mane+offset-1 : mane,match);
  return s ? (int)(s-mane)+1 : 0;
}


char  *MakeLCaseTbl (void)
{
  static char tbl[256];
  if(!tbl['a'])
    {
      int i; for (i=0; i < 256; i++)
        tbl[i] = (char)(int)CharLowerA((char *)(unsigned char)i);
    }
  return tbl;
}


char *_stristr_(char *String, char *Pattern)
{
  int   mi=-1;
  char *LowCase = MakeLCaseTbl();
  while(Pattern[++mi])
   {
     if(String[mi]==0) return 0;
     if(LowCase[(unsigned char)String[mi]]!=LowCase[(unsigned char)Pattern[mi]])
       { String++; mi=-1; }
   }
  return String;
}


char *_strstr_(char *String, char *Pattern)
{
  int   mi=-1;
  while(Pattern[++mi])
   {
     if(String[mi]==0) return 0;
     if(String[mi]!=Pattern[mi])
       { String++; mi=-1; }
   }
  return String;
}


void locate (int row,int col,int show,int shape)
{
  CONSOLE_CURSOR_INFO cci = {0};
  cursor.X = col-1;
  cursor.Y = row-1;
  SetConsoleCursorPosition(hConsole,cursor);
  cci.bVisible = show;
  cci.dwSize   = shape;
  SetConsoleCursorInfo(hConsole,&cci);
}


int Csrlin (void)
{
  HANDLE  hStdOut;
  CONSOLE_SCREEN_BUFFER_INFO  csbi = {0};
  hStdOut=GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(hStdOut,&csbi);
  return csbi.dwCursorPosition.Y+1;
}


void cls (void)
{
  COORD coordScreen = {0,0};
  DWORD cCharsWritten;
  CONSOLE_SCREEN_BUFFER_INFO csbi = {0};
  DWORD dwConSize;
  register int attr;
  cursor.X = 0;
  cursor.Y = 0;
  GetConsoleScreenBufferInfo( hConsole, &csbi );
  dwConSize = csbi.dwSize.X * csbi.dwSize.Y;
  FillConsoleOutputCharacter (hConsole, 32, dwConSize,coordScreen, &cCharsWritten);
  attr = color_fg + color_bg * 16;
  FillConsoleOutputAttribute (hConsole, attr, dwConSize,coordScreen, &cCharsWritten);
  locate(1,1,1);
}


void color (int fg, int bg)
{
 SetConsoleTextAttribute (hConsole,fg+bg*16);
  color_fg = fg;
  color_bg = bg;
}


void midstr (char *src, int st, int length, char *rep)
{
  int srclen = strlen(src);
  if(st>srclen++ || st<1) return;
  int replen = strlen(rep);
  if(replen < length || length==-1) length=replen;
  if((st+length) > srclen) length=srclen-st;
  memcpy(&src[st-1],rep,length);
}


void swap ( byte* A, byte* B, int length)
{
  byte t;
  while(length--)
  {
    t = *A;
    *(A++) = *B;
    *(B++) = t;
  }
}


int Screen (int y,int x,int mode)
{
  HANDLE  hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
  DWORD Slen;
  COORD Coord;
  WORD  Attr;
  char  Char[2];
  Coord.X=x-1;
  Coord.Y=y-1;
  ReadConsoleOutputCharacterA(hConsole,Char,1,Coord,&Slen);
  ReadConsoleOutputAttribute(hConsole,&Attr,1,Coord,&Slen);
  if(mode)
  return  Attr;
  return  Char[0];
}


int scan(char *input, char *format, ... )
{
  register int c,d ;
  char     *s_;
  int      *intptr;
  float    *floatptr;
  double   *doubleptr;
  char     A[50][2048];
  va_list  marker;
  c = 0;
  d = Split(A,input,",");
  va_start(marker, format); //Initialize arguments
  while(d && *format)
   {
     if(*format == '%') format++;
     if(*format == 's')
       {
         s_ = va_arg(marker, char *);
         strcpy(s_, A[c]);
         c++;
         d--;
       }
  if(*format == 'd')
    {
      intptr = va_arg(marker, int *);
      *intptr = atoi(A[c]);
      c++;
      d--;
    }
  if(*format == 'g')
    {
      floatptr = va_arg(marker, float *);
      *floatptr = atof(A[c]);
      c++;
      d--;
    }
 if(*format == 'l')
    {
      format++;
      doubleptr = va_arg(marker, double *);
      *doubleptr = atof(A[c]);
      c++;
      d--;
     }
  format++;
   }
  va_end(marker);              // Reset variable arguments
  if(d) return(1);             // More data than variables
  if(*format == 0) return(0);  // OK
  return(-1);                  // More variables than data
}


int Split (char Buf[][2048], char *T, char *Delim, int Flg)
{
  int  Begin = 0;
  int  Count = 0;
  int  Quote = 0;
  int  Index,i;
  int  lenT  = strlen(T);
  char Chr34[2]={34,0};
  for(Index=1;Index<=lenT;Index++)
    {
      if(instr(Delim,mid(T,Index,1))&&!Quote)
        {
          strcpy(Buf[Count],(char*)mid(T,Begin,Index-Begin));
          if ((Flg & 2) == 0)  // 0 if old version
           Count++;
          else
           if (Buf[Count][0] != 0) Count++;
           Begin=0;
           if((Flg & 1) == 1)   // 1 if true
              strcpy(Buf[Count++],(char*)mid(T,Index,1));
        }
   else
     {
       if(strcmp(mid(T,Index,1),Chr34)==0) Quote=!Quote;
       if(Begin==0) Begin=Index;
     }
   }
  if(Begin)
     strcpy(Buf[Count++],(char*)mid(T,Begin,Index-Begin));
  if((Flg & 1) == 0)   // 0 if false
      for(i=0;i<Count;i++) strcpy(Buf[i],(char*)RemoveStr(Buf[i],Chr34));
  return Count;
}


int PrinterOpen (void)
{
  int  PointSize=12;
  char zPrinter[2048];
  GetProfileString("WINDOWS","DEVICE","",zPrinter,127);
  strcpy(zPrinter,(char*)extract(zPrinter,","));
  strcpy(BcxPtr_Text,"Printing ...");
  BcxPtr_hDC=CreateDC("",zPrinter,"",0);
  if(!BcxPtr_hDC) return 0;
  BcxPtr_di.cbSize=sizeof(BcxPtr_di);
  BcxPtr_di.lpszDocName=BcxPtr_Text;
  StartDoc(BcxPtr_hDC,&BcxPtr_di);
  StartPage(BcxPtr_hDC);
  SetTextAlign(BcxPtr_hDC,TA_BASELINE | TA_NOUPDATECP | TA_LEFT);
  SetBkMode(BcxPtr_hDC,TRANSPARENT);
  BcxPtr_Lf.lfHeight=PointSize*GetDeviceCaps(BcxPtr_hDC,LOGPIXELSY)/72;
  BcxPtr_Lf.lfWidth=0;
  BcxPtr_Lf.lfEscapement=0;
  BcxPtr_Lf.lfOrientation=0;
  BcxPtr_Lf.lfWeight=FW_NORMAL;
  BcxPtr_Lf.lfItalic=0;
  BcxPtr_Lf.lfUnderline=0;
  BcxPtr_Lf.lfStrikeOut=0;
  BcxPtr_Lf.lfCharSet=ANSI_CHARSET;
  BcxPtr_Lf.lfOutPrecision=OUT_DEFAULT_PRECIS;
  BcxPtr_Lf.lfClipPrecision=CLIP_DEFAULT_PRECIS;
  BcxPtr_Lf.lfQuality=PROOF_QUALITY;
  BcxPtr_Lf.lfPitchAndFamily=VARIABLE_PITCH | FF_ROMAN;
  strcpy(BcxPtr_Lf.lfFaceName,TEXT("Courier New"));
  BcxPtr_hFont=CreateFontIndirect(&BcxPtr_Lf);
  BcxPtr_hFontOld=(HFONT)SelectObject(BcxPtr_hDC,BcxPtr_hFont);
  GetTextMetrics(BcxPtr_hDC,&BcxPtr_tm);
  BcxPtr_FontMetrix=BcxPtr_Lf.lfHeight;
  BcxPtr_PrinterOn=1;
  return   1;
}


void PrinterWrite (char *TextIn)
{
  int LPP=60;
  int CPL=80;
  char sTemp[2048]={0};
  if(!BcxPtr_PrinterOn)
    {
      MessageBox (GetActiveWindow(),"Problem with Printer","",0);
      return;
    }
  strcpy(sTemp,TextIn);
  for(;;)
    {
      if(strlen(sTemp)>CPL)
        {
          strcpy(BcxPtr_Text,(char*)left(sTemp,CPL));
          strcpy(sTemp,(char*)mid(sTemp,CPL+1));
        }
      else
        {
          strcpy(BcxPtr_Text,sTemp);
          *sTemp=0;
        }
      BcxPtr_LineCtr+=1;
      if(BcxPtr_LineCtr>=LPP)
        {
          EndPage(BcxPtr_hDC);
          BcxPtr_LineCtr=0;
          StartPage(BcxPtr_hDC);
        }
      TextOut(BcxPtr_hDC,20,BcxPtr_FontMetrix*BcxPtr_LineCtr,BcxPtr_Text,strlen(BcxPtr_Text));
      if(sTemp[0]==0) break;
    }
}


void PrinterClose (void)
{
  if(!BcxPtr_PrinterOn) return;
  SelectObject(BcxPtr_hDC,BcxPtr_hFontOld);
  DeleteObject(BcxPtr_hFont);
  EndPage(BcxPtr_hDC);
  EndDoc(BcxPtr_hDC);
  DeleteDC(BcxPtr_hDC);
  BcxPtr_LineCtr=0;
  BcxPtr_PrinterOn=0;
}


void EjectPage(void)
{
  EndPage(BcxPtr_hDC);
  BcxPtr_LineCtr=0;
  StartPage(BcxPtr_hDC);
}



// ************************************
//       User Subs and Functions
// ************************************


int TICKS (void)
{
  DEFSEG= &H40;
  A=PEEK( &H6D);
  B=PEEK( &H6C);
  strcpy(C,PEEK( &H6D));
  if(A!=C)
    {
      B=0;
    }
  TICKS=256*C+B;
  DEFSEG;
}


int INDEX (int Y, int COLUMN)
{
  INDEX=COLLEN*(COLUMN-COLMIN)+Y-4;
}


