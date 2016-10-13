       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. GMAEMUNIMUNIA.                                           000
       AUTHOR. 'MIRTA  23/05/94 LILIANA 29-06-12'.
      *REMARKS.'GENERACION DE HISCAR MUNICIPALIDADES'.                      000
      *REMARKS.'  ARREGLOS   MONTSE   08/96  PARA RECALCULAR ANT.  '.       000
      *REMARKS.' MOv. 03-2007 *BONI.M091 de $160 pasa a $168 Pto.02/2006'   000
      *REMARKS.' y a $220 Pto.01/2007                               '       000
      *REMARKS.'SILVIA: para Correlaciones Andrea las Bonificaciones'.
      *REMARKS.'no codificadas (Pos.15-16='NO') le muevo 00 al PORC  '.     000   
      *REMARKS.'SE SEPARAN LOS SUPL- DESDE DIC/2012                   '.    000   
       ENVIRONMENT DIVISION.                                                000
       DATA DIVISION.                                                       000
       WORKING-STORAGE SECTION.                                             001
           COPY WSFECHA.CPY.
       77  I                 PIC 99 VALUE ZEROS.
       77  X                 PIC 99 VALUE ZEROS.
       77  INDX              PIC 99 VALUE ZEROS.
       77  IND1176           PIC 99 VALUE ZEROS.
       77  K                 PIC 99 VALUE ZEROS.
       77  J                 PIC 99 VALUE ZEROS.
       77  JJB               PIC 99 VALUE ZEROS.
       77  I-LETRA           PIC 99 VALUE ZEROS.
       77  I-MUN             PIC 999 VALUE ZEROS.
       77  POR9-10           PIC 999 VALUE ZEROS.
       77  POR6-8            PIC 999 VALUE ZEROS.
       77  POR7-8            PIC 999 VALUE ZEROS.
       77  PORC              PIC 999V99 VALUE ZEROS.
       77  PORCM090          PIC 999 VALUE ZEROS.
       77  PORCW090          PIC 999 VALUE ZEROS.
       77  PORC-AUX          PIC 99 VALUE ZEROS.
       77  MAR-CMH           PIC X  VALUE ZEROS.
       77  MARCA1            PIC 9  VALUE ZEROS.
       77  MARCA2            PIC 9  VALUE ZEROS.
       77  MARCAAMP          PIC 9  VALUE ZEROS.
       77  PORC-MUN-AUX      PIC 999V9 VALUE ZEROS.
       77  PORANT-M117       PIC 99V99999 VALUE ZEROS.
       77  HABER-M117        PIC S9(9)V99 VALUE ZEROS.
       77  H-500430          PIC S9(9)V99 VALUE ZEROS.
       77  HABER-AUX         PIC S9(9)V99        VALUE ZEROS.
       77  IMP-M116-40       PIC S9(9)V99 VALUE ZEROS.
       77  IMP-M009-FU       PIC S9(9)V99 VALUE ZEROS.
       77  SUP-AUX           PIC S9(9)V99        VALUE ZEROS.
       77  AUX1176           PIC S9(9)V99        VALUE ZEROS.
       77  BONDEDFC          PIC S9(9)V99        VALUE ZEROS.
       77  IMPORTE60         PIC S9(9)V99        VALUE ZEROS.
       77  IMPORTE68         PIC S9(9)V99        VALUE ZEROS.
       77  M119FIJA          PIC S9(9)V99        VALUE ZEROS.
       77  M119REM           PIC S9(9)V99        VALUE ZEROS.
       77  M119REMJ          PIC S9(9)V99        VALUE ZEROS.
       77  SF68              PIC S9(9)V99        VALUE ZEROS.
       77  SF68-1            PIC S9(9)V99        VALUE ZEROS.
       77  SF68-2            PIC S9(9)V99        VALUE ZEROS.
       77  S-M102            PIC S9(9)V99        VALUE ZEROS.
       77  BASE-097          PIC S9(9)V99        VALUE ZEROS.
       77  HABER-ANT         PIC S9(9)V99        VALUE ZEROS.
       77  HABER-ACT         PIC S9(9)V99        VALUE ZEROS.
       77  HABERANT-M079         PIC S9(9)V99        VALUE ZEROS.
       77  HABERACT-M079         PIC S9(9)V99        VALUE ZEROS.
       77  DTO423                PIC S9(9)V99        VALUE ZEROS.
       77  AUM-M079              PIC S9(9)V99        VALUE ZEROS.
       77  DIF13-M079            PIC S9(9)V99        VALUE ZEROS.
       77  DOSANT                PIC S9(9)V99        VALUE ZEROS.
       77  DOSACT                PIC S9(9)V99        VALUE ZEROS.
       77  M110-2961         PIC S9(9)V99        VALUE ZEROS.
       77  SUFI1             PIC S9(9)V99        VALUE ZEROS.
       77  SUFI2             PIC S9(9)V99        VALUE ZEROS.
       77  ANTIG-AUX         PIC S9(10)V99       VALUE ZEROS.
       77  ANTIANT           PIC S9(10)V99       VALUE ZEROS.
       77  FUN119            PIC S9(10)V99       VALUE ZEROS.
       77  HSFUNQ            PIC S9(10)V99       VALUE ZEROS.
       77  HSFUNK            PIC S9(10)V99       VALUE ZEROS.
       77  PORC2             PIC 99 VALUE ZEROS.
       77  ANTIG1            PIC 99 VALUE ZEROS.
       77  ANTIG1-N          PIC S99 VALUE ZEROS.
       77  ANTIG-NB          PIC S99 VALUE ZEROS.
       77  COEF1             PIC 99V9999 VALUE ZEROS.
       77  IMPOR-HAB         PIC S9(8)V99        VALUE ZEROS.
       77  IMPOR-ANT         PIC S9(8)V99        VALUE ZEROS.
       77  IMPOANT          PIC S9(8)V99        VALUE ZEROS.
       77  IMPOACT          PIC S9(8)V99        VALUE ZEROS.
       77  BASE-BM010       PIC S9(8)V99        VALUE ZEROS.
       77  BONIM106         PIC S9(8)V99        VALUE ZEROS.
       77  BONIM1061         PIC S9(8)V99        VALUE ZEROS.
       77  BONIM050         PIC S9(8)V99        VALUE ZEROS.
     **  Obs M049 dde jul08 cambia el calc de los titu, son % del sdo min o del propio, estos quedan fijos.
       77  SECM049O       PIC S9(8)V99        VALUE 55.61.
       77  TECM049O       PIC S9(8)V99        VALUE 80.55.
       77  UNBM049O       PIC S9(8)V99        VALUE 107.40.
       77  UNAM049O       PIC S9(8)V99        VALUE 161.10.
       77  SP7M049O      PIC S9(8)V99        VALUE 82.64.
       77  TP7M049O      PIC S9(8)V99        VALUE 119.71.
       77  BP7M049O      PIC S9(8)V99        VALUE 159.61.
       77  AP7M049O      PIC S9(8)V99        VALUE 239.39.
     ** para Obras M049 dde jul08 cambia el calc de los titulos, son %, estos quedan fijos. Cargar base en TABLA de titulos.
     **************                                        
     ** para Obras M049 solo las bon.D*M049O se actualizan por aumentos,
     ** las otras son sumas fij.constantes hta nuevo aviso.DDE jul05 las D*M049O son %.DDe jul08 los titu son %. 
     **77  D1M049O       PIC S9(8)V99        VALUE 335.99.
       77  D1M049O       PIC S9(8)V99        VALUE 376.31.             
     **77  D1M049O       PIC S9(8)V99        VALUE 391.36.por diciem05
      *77  D1M049O       PIC S9(8)V99        VALUE 407.01.hasta feb07            
      *77  D1M049O       PIC S9(8)V99        VALUE 532.79.despues vino el informe del cambio de calculo.
     **77  DAM049O       PIC S9(8)V99        VALUE 378.28.
       77  DAM049O       PIC S9(8)V99        VALUE 423.67.             
      *77  DAM049O       PIC S9(8)V99        VALUE 440.61.por dic05
      *77  DAM049O       PIC S9(8)V99        VALUE 458.23.hasta feb07
      *77  DAM049O       PIC S9(8)V99        VALUE 599.83. despues vino el cambio de calculo.
     **77  DFM049O       PIC S9(8)V99        VALUE 414.29.
       77  DFM049O       PIC S9(8)V99        VALUE 464.00.                 
      *77  DFM049O       PIC S9(8)V99        VALUE 482.56.por dic05
      *77  DFM049O       PIC S9(8)V99        VALUE 501.86.hasta feb07
      *77  DFM049O       PIC S9(8)V99        VALUE 656.95.despues vino el cambio de calculo.
       77  FCM049O       PIC S9(8)V99        VALUE 255.69.
       77  MINOBS49      PIC S9(8)V99        VALUE ZEROS.  
       77  BON-MDAD012   PIC S9(8)V99        VALUE ZEROS.  
       77  T                 PIC 99 VALUE ZEROS.
       77  TITU              PIC S9(8)V99        VALUE ZEROS.
       77  TITU135           PIC S9(8)V99        VALUE ZEROS.
       77  TITUM090          PIC 9(3)        VALUE ZEROS.
       77  SUMA-ORD          PIC S9(8)V99        VALUE ZEROS.
       77  SUM008            PIC S9(8)V99        VALUE ZEROS.
       77  B-M017            PIC S9(8)V99        VALUE ZEROS.
       77  FUN015            PIC  9(3)           VALUE ZEROS.
       77  LEYENM106         PIC  X(16)          VALUE ZEROS.
       77  B-M130            PIC S9(6)V99        VALUE ZEROS.
       77  BONI109           PIC S9(6)V99        VALUE ZEROS.
       77  PT0501            PIC 9(2)V99        VALUE ZEROS.
       77  PT0502            PIC 9(2)V99        VALUE ZEROS.
       77  PT0503            PIC 9(2)V99        VALUE ZEROS.
       77  PT0504            PIC 9(2)V99        VALUE ZEROS.
       77  TPM063            PIC S9(6)V99        VALUE ZEROS.
       77  TRAMOB            PIC S9(6)V99        VALUE ZEROS.
       77  TRAMOC            PIC S9(6)V99        VALUE ZEROS.
       77  TRAMOD            PIC S9(6)V99        VALUE ZEROS.

       01  DET-HORA.
           02 HORA-HH       PIC XX.
           02 HORA-MM       PIC XX.
           02 HORA-SS       PIC XX.
      *
       01  TABLA-LETRAS-1ER.
           02  FILLER  PIC X(4) VALUE '01 A'.                           
           02  FILLER  PIC X(4) VALUE '02 B'.                           
           02  FILLER  PIC X(4) VALUE '03 C'.                           
           02  FILLER  PIC X(4) VALUE '04 D'.                           
           02  FILLER  PIC X(4) VALUE '05 E'.                           
           02  FILLER  PIC X(4) VALUE '06 F'.                           
           02  FILLER  PIC X(4) VALUE '07 H'.                           
           02  FILLER  PIC X(4) VALUE '08 J'.                           
           02  FILLER  PIC X(4) VALUE '09 L'.                           
           02  FILLER  PIC X(4) VALUE '10 M'.                           
           02  FILLER  PIC X(4) VALUE '11 N'.                           
           02  FILLER  PIC X(4) VALUE '12 O'.                           
           02  FILLER  PIC X(4) VALUE '13 P'.                           
           02  FILLER  PIC X(4) VALUE '14 Q'.                           
           02  FILLER  PIC X(4) VALUE '15 R'.                           
           02  FILLER  PIC X(4) VALUE '16 S'.                           
           02  FILLER  PIC X(4) VALUE '17 T'.                           
           02  FILLER  PIC X(4) VALUE '18 U'.                           
           02  FILLER  PIC X(4) VALUE '19 V'.                           
           02  FILLER  PIC X(4) VALUE '20 W'.                           
           02  FILLER  PIC X(4) VALUE '21 X'.                           
           02  FILLER  PIC X(4) VALUE '22 Y'.                           
           02  FILLER  PIC X(4) VALUE '23 Z'.                           
       01  T-LETRAS-1ER REDEFINES TABLA-LETRAS-1ER.
           02  LETRA1-X OCCURS 23.           
            03 ANIOS1        PIC 99.
            03 F             PIC X.
            03 LETRA1        PIC X.
      *
       01  TABLA-LETRAS-2DO.
           02  FILLER  PIC X(4) VALUE '01 A'.                           
           02  FILLER  PIC X(4) VALUE '02 B'.                           
           02  FILLER  PIC X(4) VALUE '03 C'.                           
           02  FILLER  PIC X(4) VALUE '04 D'.                           
           02  FILLER  PIC X(4) VALUE '05 E'.                           
           02  FILLER  PIC X(4) VALUE '06 G'.                           
           02  FILLER  PIC X(4) VALUE '07 I'.                           
           02  FILLER  PIC X(4) VALUE '08 K'.                           
           02  FILLER  PIC X(4) VALUE '09 L'.                           
           02  FILLER  PIC X(4) VALUE '10 M'.                           
           02  FILLER  PIC X(4) VALUE '11 N'.                           
           02  FILLER  PIC X(4) VALUE '12 O'.                           
           02  FILLER  PIC X(4) VALUE '13 P'.                           
           02  FILLER  PIC X(4) VALUE '14 Q'.                           
           02  FILLER  PIC X(4) VALUE '15 R'.                           
           02  FILLER  PIC X(4) VALUE '16 S'.                           
           02  FILLER  PIC X(4) VALUE '17 T'.                           
           02  FILLER  PIC X(4) VALUE '18 U'.                           
           02  FILLER  PIC X(4) VALUE '19 V'.                           
           02  FILLER  PIC X(4) VALUE '20 W'.                           
           02  FILLER  PIC X(4) VALUE '21 X'.                           
           02  FILLER  PIC X(4) VALUE '22 Y'.                           
           02  FILLER  PIC X(4) VALUE '23 Z'.                           
       01  T-LETRAS-2DO REDEFINES TABLA-LETRAS-2DO.
           02  LETRA2-X OCCURS 23.           
            03 ANIOS2        PIC 99.
            03 F             PIC X.
            03 LETRA2        PIC X.
      *
     *********** MOV JUN16: cargo % de la M078 para 2017 ......... y tb tiene modificacion en el perform.
       01  TABLA-MUNICIPAL-1ER.
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *002*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *003*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *004*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001004004004004'.
      *                                               *005*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *006*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001002003003003'.
      *                                               *007*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001003003'.
      *                                               *008*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001002002'.
      *                                               *009*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *010*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001003001001001001001001001001002'.
      *                                               *011*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *012*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *013*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *014*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *015*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *016*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *017*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *018*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001002003003'.
      *                                               *019*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *020*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *021*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *022*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *023*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *024*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *025*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *026*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001003003'.
      *                                               *027*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *028*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001003003'.
      *                                               *029*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *030*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *031*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *032*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *033*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *034*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *035*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001003
      -                           '003001001001001001001001001001001'.
      *                                               *036*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001002
      -                           '003003003003001001001001001001001'.
      *                                               *037*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *038*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *039*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *040*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *041*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *042*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001002
      -                           '003003003003003001001001001001001'.
      *                                               *043*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *044*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *045*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *046*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '003003003003003003003003003003003'.
      *                                               *047*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *048*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *049*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *050*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *051*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      ****                        '001003003003003003003'.informa el 3ro q no cobran asi.mov mayo12.
      *                                               *052*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *053*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *054*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *055*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001003003'.
      *                                               *056*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *057*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *058*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *059*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *060*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001002002002002002'.
      *                                               *061*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001002003003'.
      *                                               *062*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *063*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001002002002002002002002002002002'.
      *                                               *064*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *065*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *066*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *067*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *068*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001002'.
      *                                               *069*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *070*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *071*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *072*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *073*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *074*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *075*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '003003003003003003003003003003003'.
      *                                               *076*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001003003003003003003003'.
      *                                               *077*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *078*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                          '001001001001001001001001001001001002'.
      *                                               *079*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *080*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *081*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *082*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *083*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *084*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *085*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *086*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *087*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *088*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '003003003003003003003003003003003'.
      *                                               *089*
           02  FILLER  PIC X(78)   VALUE '001001001003001001001001001001
      -                           '001001001003003003003003003003003'.
      *                                               *090*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *091*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *092*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *093*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *094*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *095*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *096*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *097*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *098*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *099*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *100*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *101*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *102*
           02  FILLER  PIC X(78)   VALUE '003001001001001001001001001001
      -                           '001001001001001003003003003003003'.
      *                                               *103*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '002002002002002002002002002002002'.
      *                                               *104*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001002002002002'.
      *                                               *105*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *106*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *107*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *108*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *109*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001003003'.
      *                                               *110*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *111*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *112*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *113*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *114*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001003'.
      *                                               *115*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *116*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *117*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001                                           
      -                           '001001001001001001001001001001001'.
   ******* 02  FILLER  PIC X(78)   VALUE '001001001001001001001001001002 en mov feb16 el 3ro advierte en recibos q la ant dp
   *******                        '002002002002002002002002002002001'.   1995 es siempre al 1, no encuentro info de respaldo.
   ******* 02  FILLER  PIC X(78)   VALUE '001001001001001001001001001002 Cambio % de 2106, en mar16 cambio 2005 a 2015.
   *******                        '002002002002002002002002002002001'.                                                               
      *                                               *118*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *119*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *120*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *121*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *122*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *123*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *124*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *125*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *126*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *127*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *128*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *129*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *130*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *131*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001004004'.
      *                                               *132*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *133*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *134*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *135*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *136*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
      *                                               *137*
           02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001
      -                           '001001001001001001001001001001001'.
       01  T-MUNIC-1ER REDEFINES TABLA-MUNICIPAL-1ER.
           02  T-M1 OCCURS 137.
               03  PORC-MUN-X1 OCCURS 26 PIC X(3).

********    segundo semestre    hasta el 2007   el 27-02-06
       01  TABLA-MUNICIPAL-2DO.
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *002*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *003*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *004*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001004'.
      *                                               *005*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *006*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *007*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *008*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *009*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *010*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '003001001001001001'.
      *                                               *011*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *012*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *013*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *014*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *015*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *016*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *017*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *018*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *019*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *020*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *021*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *022*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *023*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *024*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *025*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *026*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *027*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *028*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *029*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *030*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *031*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *032*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *033*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *034*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *035*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001003003
      -                           '001001001001001001'.
      *                                               *036*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001002
      -                           '003003003001001001'.
      *                                               *037*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *038*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *039*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *040*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *041*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *042*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001002003
      -                           '003003003003003003'.
      *                                               *043*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *044*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *045*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *046*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '003003003003003003'.
      *                                               *047*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *048*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *049*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *050*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *051*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
    *****                         '001003003003003003'.
      *                                               *052*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *053*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *054*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *055*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *056*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *057*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *058*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *059*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *060*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *061*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *062*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *063*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '002001001001001001'.
      *                                               *064*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *065*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.  
      *                                               *066*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *067*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *068*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *069*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *070*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *071*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *072*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *073*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *074*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *075*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001003
      -                           '003003003003003003'.
      *                                               *076*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *077*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *078*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *079*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *080*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *081*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *082*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *083*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *084*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *085*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *086*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *087*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *088*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001003
      -                           '003003003003003003'.
      *                                               *089*
           02  FILLER PIC X(78) VALUE '001001001003001001001001001001001
      -                           '001001003003003003'.
      *                                               *090*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *091*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *092*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *093*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *094*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *095*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *096*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *097*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *098*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *099*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *100*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *101*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *102*
           02  FILLER PIC X(78) VALUE '003001001001001001001001001001001
      -                           '001001001001002002'.
      *                                               *103*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '002002002002002002'.
      *                                               *104*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *105*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *106*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *107*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *108*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *109*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *110*
           02  FILLER PIC X(78) VALUE '002002002002002002002002002002002
      -                           '002002002002002002'.
      *                                               *111*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001002002002'.
      *                                               *112*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *113*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *114*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *115*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *116*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *117*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001002002
      -                           '002002002002002002'.
      *                                               *118*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *119*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *120*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *121*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *122*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *123*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *124*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *125*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *126*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *127*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *128*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *129*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *130*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *131*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *132*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *133*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *134*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *135*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *136*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
      *                                               *137*
           02  FILLER PIC X(78) VALUE '001001001001001001001001001001001
      -                           '001001001001001001'.
       01  T-MUNIC-2DO REDEFINES TABLA-MUNICIPAL-2DO.
           02  T-M2 OCCURS 137.
               03  PORC-MUN-X2 OCCURS 26 PIC X(3).

     **** agrego clase 2 dde mov noviembre08 p/dias de gdia a pedido, ppto nov07
     **** agrego clases 3 y 4 dde mov febrero13 p/dias de gdia a pedido, ppto nov07
       01  TABLA-OBSAN-M049-BONIF.
           02  FILLER PIC X(029) VALUE '1 000000 000000 000000 003308'. 
           02  FILLER PIC X(029) VALUE '2 000000 000000 000000 000000'. 
           02  FILLER PIC X(029) VALUE '3 000000 000000 000000 000000'. 
           02  FILLER PIC X(029) VALUE '4 000000 000000 000000 000000'. 
           02  FILLER PIC X(029) VALUE '5 000000 000000 000000 003616'. 
           02  FILLER PIC X(029) VALUE '6 000436 000000 000000 003867'. 
           02  FILLER PIC X(029) VALUE '7 000450 000000 000000 004141'. 
           02  FILLER PIC X(029) VALUE '8 000459 000000 000000 004420'. 
           02  FILLER PIC X(029) VALUE '9 000510 000000 000000 004795'. 
           02  FILLER PIC X(029) VALUE 'A 000604 000000 000000 005111'. 
           02  FILLER PIC X(029) VALUE 'B 000669 005560 004705 005575'. 
           02  FILLER PIC X(029) VALUE 'C 000745 005985 004747 006158'. 
           02  FILLER PIC X(029) VALUE 'D 000910 006410 004688 007087'. 
           02  FILLER PIC X(029) VALUE 'E 001193 007532 005428 007695'. 
           02  FILLER PIC X(029) VALUE 'F 001594 008534 006065 008449'. 
           02  FILLER PIC X(029) VALUE 'G 001834 008996 006001 009002'. 
           02  FILLER PIC X(029) VALUE 'H 002172 011168 007775 000000'. 
           02  FILLER PIC X(029) VALUE 'I 002578 013748 009955 000000'. 
           02  FILLER PIC X(029) VALUE 'J 002909 017497 012894 000000'. 
           02  FILLER PIC X(029) VALUE 'K 003669 021207 016104 000000'. 
           02  FILLER PIC X(029) VALUE 'L 000000 025617 020058 000000'. 
           02  FILLER PIC X(029) VALUE 'M 000000 030159 023676 000000'. 
       01  T-O-BONIF REDEFINES TABLA-OBSAN-M049-BONIF.
           02  T-TI OCCURS 22. 
               03  CLASE PIC X.
               03  F     PIC X.
               03  ETEC  PIC 9(4)V99.
               03  F     PIC X.
               03  EUNA  PIC 9(4)V99.
               03  F     PIC X.
               03  EUNB   PIC 9(4)V99.
               03  F     PIC X.
               03  GROT   PIC 9(4)V99.

     **** agrego bases de obs -clase I- desde ppto.julio 08 p/calculo de titulos, fech hisc.               
       01  TABLA-OBSAN-M049-TITUL.
           02  FILLER PIC X(014) VALUE '201609 1217766'.                    
           02  FILLER PIC X(014) VALUE '201606 1170929'.                    
           02  FILLER PIC X(014) VALUE '201604 1054891'.                    
           02  FILLER PIC X(014) VALUE '201507 0941868'.                    
           02  FILLER PIC X(014) VALUE '201506 0869417'.                    
           02  FILLER PIC X(014) VALUE '201505 0796965'.                    
           02  FILLER PIC X(014) VALUE '201410 0735608'.                    
           02  FILLER PIC X(014) VALUE '201405 0645270'.                    
           02  FILLER PIC X(014) VALUE '201307 0566026'.                    
           02  FILLER PIC X(014) VALUE '201304 0511250'.                    
           02  FILLER PIC X(014) VALUE '201206 0456473'.                    
           02  FILLER PIC X(014) VALUE '201203 0415315'.                    
           02  FILLER PIC X(014) VALUE '201110 0374158'.                    
           02  FILLER PIC X(014) VALUE '201108 0333235'.                    
           02  FILLER PIC X(014) VALUE '201009 0292311'.                    
           02  FILLER PIC X(014) VALUE '201008 0263080'.                    
           02  FILLER PIC X(014) VALUE '200910 0233849'.                    
           02  FILLER PIC X(014) VALUE '200906 0214360'.                    
           02  FILLER PIC X(014) VALUE '200901 0194873'.                    
           02  FILLER PIC X(014) VALUE '200811 0174873'.                    
           02  FILLER PIC X(014) VALUE '200810 0154873'.                    
           02  FILLER PIC X(014) VALUE '200807 0134873'.                    
       01  T-O-TITUL REDEFINES TABLA-OBSAN-M049-TITUL.
           02  T-TIT OCCURS 22. 
               03  PPTO-T PIC X(6).
               03  F      PIC X.
               03  BAS-T  PIC 9(5)V99.

     **** agrego Suma fija no remunerativa ppto enero09 autor.expte 21557-125444-09 -NR- 
     **** agrego Suma fija no remunerativa ppto enero10 autor.expte 21557-155776-10 -NR1(dtos 2843/09 y 3/10), eliminado 
     ****  por dtos.3859/10 y 3/11 dde enero11.                                                                 
     **** agrego Suma fija no remunerativa ppto jun12 -NR2- dto 1343/12, se da de baja en mar13 por dtos 726/13 y 15/13.
       01  TABLA-NOREM-M068.          
           02  F PIC X(70) VALUE '104075 124889 00000 S252           '.
           02  F PIC X(70) VALUE '129802 035761 00000 S450 S312 S463  '.
           02  F PIC X(70) VALUE '127162 032593 00000 S100 S147 S501  '.
           02  F PIC X(70) VALUE '127882 033457 00000 S484 S464 S485  '.
           02  F PIC X(70) VALUE                                        
               '126442 031729 00000 S478 S121 S476 S502      '.
           02  F PIC X(70) VALUE '139232 047077 00000 S052            '.
           02  F PIC X(70) VALUE '122962 027553 00000 S059            '.
           02  F PIC X(70) VALUE '019030 022636 19477 S103 C113 CB13  '.
           02  F PIC X(70) VALUE '016471 019566 16836 S303 C112       '.
           02  F PIC X(70) VALUE                                              
                     '013913 016496 14196 S304 C410 CB04 C40M S309    '.
           02  F PIC X(70) VALUE 
                     '013142 015431 13287 200A 3010 4010 5010 6010 7010 
      -                  '       '.
           02  F PIC X(70) VALUE                                          
                     '011429 013374 11518 200B 300A 4009 5009 6009 7009 
      -                  '       '.
           02  F PIC X(70) VALUE '010957 012809 11042 2008 3008 400A 500     
      -                  '8 6008 700A C608 C408 5123      '.
           02  F PIC X(70) VALUE '009715 011318 09750 2007 300B 4007 500
      -        '7 6007 7007 5122 6522      '.
           02  F PIC X(70) VALUE '009158 010650 09176 2006 3006 400B 50A
      -       '6 6006 700B C606 C406 500A      '.
           02  F PIC X(70) VALUE '009135 010558 09100 2005 300C 4005 500
      -           '2 6005 7005 5602 6521      '.
           02  F PIC X(70) VALUE '009081 010461 09018 2004 3004 400C 500
      -                    'C 6200 700C      '.
           02  F PIC X(70) VALUE '009044 010373 08945 2003 300D 400D 560
      -       '5 6040 700D 6643 5043 5643 6043 '.
       01  T-NOREM-M068  REDEFINES TABLA-NOREM-M068.        
           02  TA-M068 OCCURS 18. 
               03  NR-068 PIC 9(4)V99.
               03  F      PIC X.
               03  NR1-068 PIC 9(4)V99.
               03  F      PIC X.
               03  NR2-068 PIC 9(3)V99.
               03  F      PIC X.
               03  CODI-M068 OCCURS 10.
                   04  CO-068 PIC X(4).     
                   04  F      PIC X.

     **** agrego Sdos jun09 p/no remunerativa ppto agosto09 autor.expte 21557-142394-09.                    
       01  TABLA-NOREM-M102.          
           02  F PIC X(12) VALUE                                              
                     '037456 N002 '.
           02  F PIC X(12) VALUE                                              
                     '047151 N004 '.
           02  F PIC X(12) VALUE                                              
                     '039086 N006 '.
           02  F PIC X(12) VALUE                                              
                     '040601 N008 '.
           02  F PIC X(12) VALUE                                              
                     '042114 N009 '.
           02  F PIC X(12) VALUE                                              
                     '043278 N011 '.
           02  F PIC X(12) VALUE                                              
                     '053293 N016 '.
           02  F PIC X(12) VALUE                                              
                     '061329 N017 '.
           02  F PIC X(12) VALUE                                              
                     '068318 N019 '.
           02  F PIC X(12) VALUE                                              
                     '079379 N020 '.
           02  F PIC X(12) VALUE                                              
                     '160500 1001 '.
           02  F PIC X(12) VALUE                                              
                     '153000 1002 '.
           02  F PIC X(12) VALUE                                              
                     '153000 2001 '.
           02  F PIC X(12) VALUE                                              
                     '137100 1003 '.
           02  F PIC X(12) VALUE                                              
                     '137100 2002 '.
           02  F PIC X(12) VALUE                                              
                     '137100 3001 '.
           02  F PIC X(12) VALUE                                              
                     '123300 1004 '.
           02  F PIC X(12) VALUE                                              
                     '123300 2003 '.
           02  F PIC X(12) VALUE                                              
                     '123300 3002 '.
           02  F PIC X(12) VALUE                                              
                     '123300 E001 '.
           02  F PIC X(12) VALUE                                              
                     '123300 4001 '.
           02  F PIC X(12) VALUE                                              
                     '091700 2004 '.
           02  F PIC X(12) VALUE                                              
                     '091700 3003 '.
           02  F PIC X(12) VALUE                                              
                     '091700 E002 '.
           02  F PIC X(12) VALUE                                              
                     '091700 4002 '.
           02  F PIC X(12) VALUE                                              
                     '091700 5001 '.
           02  F PIC X(12) VALUE                                              
                     '087800 2005 '.
           02  F PIC X(12) VALUE                                              
                     '087800 3004 '.
           02  F PIC X(12) VALUE                                              
                     '087800 E003 '.
           02  F PIC X(12) VALUE                                              
                     '087800 4003 '.
           02  F PIC X(12) VALUE                                             
                     '087800 5002 '.
           02  F PIC X(12) VALUE                                              
                     '087800 6001 '.
           02  F PIC X(12) VALUE                                              
                     '084600 3005 '.
           02  F PIC X(12) VALUE                                              
                     '084600 E004 '.
           02  F PIC X(12) VALUE                                             
                     '084600 4004 '.
           02  F PIC X(12) VALUE                                              
                     '084600 5003 '.
           02  F PIC X(12) VALUE                                              
                     '084600 6002 '.
           02  F PIC X(12) VALUE                                              
                     '081900 E005 '.
           02  F PIC X(12) VALUE                                              
                     '081900 4005 '.
           02  F PIC X(12) VALUE                                              
                     '081900 5004 '.
           02  F PIC X(12) VALUE                                              
                     '081900 6003 '.
           02  F PIC X(12) VALUE                                              
                     '080800 5005 '.
           02  F PIC X(12) VALUE                                              
                     '080800 6004 '.
           02  F PIC X(12) VALUE                                             
                     '079600 6005 '.
       01  T-NOREM-M102  REDEFINES TABLA-NOREM-M102.        
           02  TA-M102 OCCURS 44. 
               03  NR-102 PIC 9(4)V99.
               03  F      PIC X.
               03  CO-102 PIC X(4).     
               03  F      PIC X.

       LINKAGE SECTION.
       01  REG-PARAM.
           02  COD-RETRO         PIC X.
           02  FECHA-MES.
               03 FE-ANIO        PIC X(4).
               03 FE-MES         PIC XX.
           02  PORC-MAS          PIC XX.
           02  TIPOLIQ           PIC X(02).
           02  TITUS             PIC X(68).
           02  SACA-PL           PIC X.
      *
      *      DISEºO DE MAESTRO DESEMPAQUETADO                               007
      *                                                                     007
       01  REG-MAE.
           02 MARCA           PIC X.                                        007
           02 CONC            PIC XX.                                       007
           02 SEC.                                                          007
              03 SEC1         PIC X.                                        007
              03 SEC2         PIC X.                                        007
           02 NRO             PIC X(10).                                    008
           02 EME             PIC X.                                        008
           02 APYN            PIC X(27).                                    008
           02 PAGO            PIC X(4).                                     008
           02 HABER           PIC S9(9)V99.                                 009
           02 IOMA            PIC S9(7)V99.                                 009
           02 PARENT          PIC X.                                        009
           02 ESPOSA.                                                       009
              03 ESPOSA-N     PIC 9.                                        009
           02 HIJOS.                                                        009
              03 HIJOS-N      PIC 99.                                       009
           02 CADUC.                                                        009
              03 CADUC-AA     PIC X(4).
              03 CADUC-MM     PIC X(2).
           02 MARCA-IOMA      PIC X.                                        009
           02 GRUPO           PIC 9(7).                                     009
           02 ANTIG           PIC S9(7)V99.                                 009
           02 SI-ES-VIAL      PIC X.                                        009
           02 SI-LETRA-ANTIG  PIC X.                                        009
           02 CODIGO.                                                       009
              03 POS-1-A-16.                                                009
                 04 POS-1-4.                                                009
                   05 POS-1     PIC X.                                      009
                   05 POS-2-4.                                              009
                      06 POS-2     PIC X.                                   009
                      06 POS-3-4.                                           009
                         07 POS-3     PIC X.                                009
                         07 POS-4     PIC X.                                009
                 04 POS-5-8.                                                009
                    05 POS-5-6.                                    
                      06 POS-5    PIC X.                          
                      06 POS-6    PIC X.                        
                    05 POS-7-8.                                             009
                      06 POS-7    PIC X.                                     009
                      06 POS-8    PIC X.                                     009
                 04 POS-5-8R REDEFINES POS-5-8.                             009
                    05 POS-5R    PIC X.
                    05 POS-6-7   PIC XX.                                        
                    05 POS-8R    PIC X.                                     009
                 04 POS-5-8RR REDEFINES POS-5-8.                             009
                    05 POS-5RR    PIC X.
                    05 POS-6-8N  PIC 999.                                       
                 04 POS-5-8NN REDEFINES POS-5-8.                             009
                    05 POS-5-6N   PIC XX.
                    05 POS-7-8N  PIC 99.                                       
                 04 POS-5-888 REDEFINES POS-5-8.                             009
                    05 POS-5-7    PIC XXX.
                    05 POS-8N     PIC 9.                                       
                 04 POS-9-10   PIC XX.                                      009
                 04 POS-9-10N REDEFINES POS-9-10 PIC 99.                    009
                 04 POS-9-10R REDEFINES POS-9-10.                           009
                    05 POS-9    PIC X.                                      009
                    05 POS-10   PIC X.                                      009
                 04 POS-11-12R.                                             009
                    05 POS-11-12  PIC 99.                                   009
                 04 POS-13-14.                                              009
                    05 POS-13  PIC X.                                       009
                    05 POS-14  PIC X.                                       009
                 04 POS-13-14N REDEFINES POS-13-14 PIC 99.                  009
                 04 POS-15-16.                                              009
                    05 POS-15  PIC X.                                       009
                    05 POS-16  PIC X.                                       009
                 04 POS-15-16N REDEFINES POS-15-16 PIC 99.                  009
                 04 POS-17-20.                                              009
                    05 POS-17-18.           
                      06 POS-17  PIC X.
                      06 POS-18  PIC X.
                    05 POS-17-18N REDEFINES POS-17-18 PIC 99.           
                    05 POS-19-20.           
                      06 POS-19  PIC X.                                       009
                      06 POS-20  PIC X.                                       009
                    05 POS-19-20N REDEFINES POS-19-20.
                      06 POS-19N PIC 9.                                       009
                      06 POS-20N PIC 9.                                       009
                 04 POS-17-20R REDEFINES POS-17-20.                           009
                      06 POS-17R     PIC X.
                      06 POS-18-19.            
                          07 POS-18-19R  PIC 99.   
                      06 POS-20R     PIC X.                                   009
                 04 POS-17-20B REDEFINES POS-17-20 PIC 999V9.         
           02 CODIGO2.                                                        009
              03 POS-1-14-2.                  
                 04 POS-1-4-2    PIC X(04).
                 04 POS-5-8-2    PIC X(04).
                 04 POS-9-14-2   PIC X(06).
              03 POS-15-2      PIC X.
           02 NRO-SEC          PIC 9(6).                                    011
           02 POS-138-139      PIC XX.                                      011
           02 POS-140          PIC X.                                       011
       01  REG-AUX.                                                         003
           02 H-FECHA                    PIC X(6).                          003
           02 H-CODIGO.                                                     003
              03 H-SECCI.                                                   003
                 04 SECC-1               PIC X.                             004
              03 SECC-M                  PIC X(3).                          004
              03 AGRUP                   PIC X.                             004
              03 CARGO                   PIC 999.                           004
              03 CARG1 REDEFINES CARGO.                                     004
                 04 COD-CARG1            PIC X.                             004
                 04 COD-CARG2            PIC X.                            004
                 04 COD-CARG3            PIC X.                            004
              03 FILLER                  PIC X(2).                          004
              03 BONIF                   PIC 99.                            004
              03 BONIF1 REDEFINES BONIF.                                    004
                 04 LET1                 PIC X.                             004
                 04 LET2                 PIC X.                             004
           02 F                          PIC XXX.                           004
           02 H-HABER                    PIC S9(9)V99   COMP-3.              004
      **                                              * POS 22 - 27
           02 SUPL1                      PIC S9(9)V99   COMP-3.              005
      **                                              * POS 28 - 33
           02 SUPL2                      PIC S9(9)V99   COMP-3.              005
      **                                              * POS 34 - 39
           02 TABLA.                                                        005
              03 H-ANTIG   OCCURS 41 TIMES PIC S9(9)V99 COMP-3.              005
      **                                              * POS 40 - 285
           02 SUMAFIJ1                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 286- 291
           02 SUMAFIJ2                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 292- 297
           02 SUMAFIJ3                   PIC S9(7)V99 COMP-3.               005
      **                                              * POS 298- 302
      *
       01  VEC-LIQ.
           02 REGISTRO  OCCURS 15.
              03 A-MARCA           PIC X.                                   007
              03 A-CONC            PIC XX.                                  007
              03 A-SEC.                                                     007
                 04 A-SEC1         PIC X.                                   007
                 04 A-SEC2         PIC X.                                   007
              03 A-NRO             PIC X(10).                               008
              03 A-EME             PIC X.                                   008
              03 A-APYN            PIC X(27).                               008
              03 A-PAGO            PIC X(4).                                008
              03 A-HABER           PIC S9(9)V99.                            009
              03 A-IOMA            PIC S9(7)V99.                            009
              03 A-PARENT          PIC X.                                   009
              03 ESPOSA.                                                    009
                 04 A-ESPOSA-N     PIC 9.                                   009
              03 A-HIJOS.                                                   009
                 04 A-HIJOS-N      PIC 99.                                  009
              03 A-CADUC.                                                   009
                 04 A-CADUC-AA     PIC X(4).
                 04 A-CADUC-MM     PIC X(2).
              03 A-MARCA-IOMA      PIC X.                                   009
           03 A-GRUPO           PIC 9(7).                                   009
           03 A-ANTIG           PIC S9(7)V99.                               009
           03 A-SI-ES-VIAL      PIC X.                                      009
           03 A-SI-LETRA-ANTIG  PIC X.                                      009
           03 A-CODIGO.                                                     009
              04 A-POS-1-A-16.                                              009
                 05 A-POS-1-4.                                              009
                    06 A-POS-1-2.                                           009
                       07 A-POS-1     PIC X.                                009
                       07 A-POS-2     PIC X.                                009
                    06 A-POS-3-4.                                           009
                       07 A-POS-3     PIC X.                                009
                       07 A-POS-4     PIC X.                                009
                 05 A-POS-5-8.                                              009
                   06 A-POS-5-6.                            
                     07 A-POS-5    PIC X.                                    009
                     07 A-POS-6    PIC X.                                    009
                   06 A-POS-7-8    PIC XX.                      
                 05 A-POS-9-10   PIC XX.                                    009
                 05 A-POS-9-10N REDEFINES A-POS-9-10  PIC 99.    
                 05 A-POS-11-12  PIC XX.                                    009
                 05 A-POS-13-14.                                            009
                    06 A-POS-13  PIC X.                                     009
                    06 A-POS-14  PIC X.                                     009
                 05 A-POS-15-16.                                            009
                    06 A-POS-15  PIC X.                                     009
                    06 A-POS-16  PIC X.                                     009
                 05 A-POS-17-18.                 
                    06 A-POS-17     PIC X(1).
                    06 FILLER       PIC X(1).
                 05 FILLER       PIC X(2).
           03 A-CODIGO2          PIC X(15).                                 009
           03 A-NRO-SEC          PIC 9(6).                                  011
           03 A-POS-138-139      PIC XX.                                    009
           03 A-POS-140          PIC X.                                     009
      *
       01  MASI.
           02 MARCA-MASI   PIC 9.
       01  IMPORTE-MUNI.
           02 HABER-MUNI   PIC S9(7)V99.
           02 ANTIG-MUNI   PIC S9(5)V99.
      *

       PROCEDURE DIVISION  USING REG-PARAM REG-MAE REG-AUX VEC-LIQ MASI     043
                                 IMPORTE-MUNI.
           COPY PDFECHA.CPY.
           MOVE SPACES TO VEC-LIQ MASI.
           MOVE ZEROS  TO HABER-MUNI ANTIG-MUNI MAR-CMH.
           MOVE ZEROS  TO PORANT-M117 PORCW090 BONDEDFC H-500430.                     
           IF SEC2 = 'P'
               THEN MOVE '1'      TO MASI
                    GO TO CERRAR.
           IF POS-13-14  NOT NUMERIC                               
                    THEN MOVE '1'      TO MASI
                         GO TO CERRAR.
           MOVE 0 TO I.
           MOVE POS-13-14 TO ANTIG1.
           IF ANTIG1 > 60
               THEN MOVE '1'      TO MASI
                    GO TO CERRAR.
           IF (POS-1-4 = 'M079' OR 'M112')
                                AND POS-5-8 = 'BMLV'
                         THEN MOVE 100 TO PORC
                              MOVE '00' TO POS-15-16 
                              GO TO ABAJO.
       SIGUEW. 
************   agregado para adecuaciones el 14-02-06
           IF COD-RETRO = 'C'
             THEN
                IF POS-15-16 = 'NO' 
                  THEN MOVE 0 TO PORC
                       MOVE '00' TO POS-15-16
                  ELSE
                     IF POS-15-16 = '00' OR '  '
                         THEN MOVE 100 TO PORC
                              MOVE '00' TO POS-15-16 
                         ELSE IF POS-15-16 NUMERIC
                             THEN  MOVE POS-15-16N TO PORC
                             ELSE  MOVE '1' TO MASI
                                   GO TO CERRAR

             ELSE IF POS-15-16 NUMERIC
                       THEN  MOVE POS-15-16N TO PORC
                       ELSE  MOVE '1'      TO MASI
                               GO TO CERRAR.
******************   agrego 14-5-13    
  ***************
            IF MARCA = 1 AND NRO = '8130940500' AND CONC = '02'
                         AND POS-1-4 = 'M061' AND POS-5-8 = 'B012'
                                              AND POS-15-16N = 27
                        THEN MOVE 27.3 TO PORC.
******************   agrego 14-5-13    
***************                              
************   agregado para REG ADICIONAL el 25-9-12 
            IF POS-1-4 = 'M090' AND SI-ES-VIAL = 'M' 
                           THEN
                  MOVE REG-MAE TO REGISTRO (1) 
                  MOVE ZEROS   TO A-IOMA (1) A-ANTIG (1)
                  MOVE '*M0909 TIEMPCOMP' TO A-CODIGO (1)
                  COMPUTE A-HABER (I) = H-HABER * PORC / 100 
                               GO TO CERRAR.
                                                  
************   agregado para adecuaciones el 14-02-06

       ABAJO.  

      **   IF NRO = '7064817240'
      ***   DISPLAY NRO ' ANTIG ' ANTIG1 ' PORC ' PORC.    
       
           MOVE ZEROS TO IMPORTE60 HSFUNK HSFUNQ BONDEDFC. 
           MOVE SPACES TO MAR-CMH. 
           IF POS-1 = 'W'                     
                THEN
              IF POS-1-4 = 'W115' OR 'W034' OR ('W012'           
                 AND FECHA-MES > '201203')
                            THEN
                            PERFORM ES-W115 THRU F-ES-W115
                            GO TO CERRAR                                   
                            ELSE
                  PERFORM CALCULA-HABER THRU FIN-CALCULA-HABER 
                  IF (POS-1-4 = 'W090' AND FECHA-MES > '201101')
                            THEN
                            PERFORM ES-W090 THRU F-ES-W090 
                            GO TO CERRAR                                   
                            ELSE
                            GO TO CERRAR.                                   
   ******* IF NRO = '2019651050' OR '2019745620'                             
   *******                       THEN DISPLAY 'fechaaaa ' FECHA-MES. lo saco en mov.marzo16.
   ***********   AGREGO 24-1-13 , esta en hiscar del 2005. Mov.set14 agr excepc por BRIE.
            IF POS-1-4 = 'M068' AND POS-5 = 'B' AND POS-5-8 NOT = 'BRIE'
                            THEN
                            PERFORM ES-BONI THRU F-ES-BONI 
                            GO TO CERRAR.                                  

           IF  FECHA-MES > '201211' 
                       THEN
           PERFORM CALCULA-HABER-SEPA THRU FIN-CALCULA-HABER-SEPA 
                       ELSE
                  PERFORM CALCULA-HABER THRU FIN-CALCULA-HABER.
           PERFORM CALCULA-BONIF THRU FIN-CALCULA-BONIF.
           PERFORM CALCULA-BONIF-SH THRU FIN-CALCULA-BONIF-SH.
           PERFORM CALCULA-AMPARO THRU FIN-CALCULA-AMPARO.

    ****     IF POS-1-4 = 'M008'
    ****      THEN IF FECHA-MES > '199707' AND COD-RETRO = 'M'
    ****             THEN COMPUTE HABER-AUX = HABER + ANTIG
    ****                  MOVE HABER-AUX  TO A-IOMA (1).
       CERRAR.
           GOBACK.
  ******   21-07-09  agrego munic. 034 porque unificamos calculo de antig.
       ES-BONI.   
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I) A-HABER (I).           
           IF POS-17-20 IS NUMERIC 
                          THEN
                 COMPUTE A-HABER (I) = (H-HABER *      
                                  POS-17-20B / 100) * PORC / 100.                     
       F-ES-BONI.  EXIT.  
       ES-W090.   
           IF POS-11-12R = '00'
                     THEN GO TO SINFALLA.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).           
           MOVE '*W090 FALLACAJA '  TO A-CODIGO (I).
           MOVE '02' TO A-CONC (I).
           IF POS-11-12R = 'A0'
                   THEN
              COMPUTE A-HABER(I) = H-HABER * PORC / 100 
                   ELSE
              COMPUTE A-HABER(I) = (H-HABER * POS-11-12 / 100) 
                                            * PORC / 100.
       SINFALLA.   
           IF POS-18-19 = '00'
                     THEN GO TO SINBLOQT.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).           
           MOVE '*W090 BLOQ.TITU '  TO A-CODIGO (I).
           MOVE '02' TO A-CONC (I).
           IF POS-18-19 = 'A0'
                   THEN
              COMPUTE A-HABER(I) = H-HABER * PORC / 100 
                   ELSE
           COMPUTE A-HABER(I) = (H-HABER * POS-18-19R / 100)
                                        * PORC / 100.

       SINBLOQT.   
       F-ES-W090.  EXIT.  
       ES-W115.   
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I)
                           A-HABER (I)  ANTIG-AUX ANTIG-NB
                           A-ANTIG (I) MARCAAMP.
            IF POS-11-12 = '00'              
               THEN
               IF ANTIG1 > 40
                         THEN
                      COMPUTE ANTIG1 = ANTIG1 - 40 + 1
                      COMPUTE ANTIG-AUX =
                            H-ANTIG (41) + H-ANTIG (ANTIG1)
                         ELSE 
                       ADD 1 TO ANTIG1
                       MOVE H-ANTIG (ANTIG1) TO ANTIG-AUX 
               ELSE
              IF POS-1-4 = 'W012'           
                 THEN
               IF POS-11-12 = '01'              
                   THEN COMPUTE ANTIG-AUX = (SUMAFIJ3 * 0.02)
                                               * ANTIG1    
                   ELSE DISPLAY ' NO CORRESPONDE POS-11-12 PARA  W '
                                      REG-MAE 
                 ELSE
             IF POS-11-12 = '01'              
                  THEN COMPUTE ANTIG-AUX = (SUMAFIJ3 * 0.03)
                                               * ANTIG1    
                  ELSE
             IF POS-11-12 = '02'              
                  THEN COMPUTE ANTIG-AUX = (SUMAFIJ3 * 0.035)
                                               * ANTIG1   
                  ELSE
             IF POS-11-12 = '03'              
                  THEN COMPUTE ANTIG-AUX = (SUMAFIJ3 * 0.025)
                                               * ANTIG1      
                  ELSE
             IF POS-11-12 = '04'              
                  THEN COMPUTE ANTIG-AUX = (SUMAFIJ3 * 0.02)
                                               * ANTIG1      
                  ELSE DISPLAY ' NO CORRESPONDE POS-11-12 PARA  W '
                                      REG-MAE.
           COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
           MOVE ANTIG-AUX  TO  A-ANTIG (I).
       F-ES-W115. EXIT.   

       CALCULA-HABER-SEPA.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           IF POS-1-4 =  'M117'                                         
                        THEN
              IF CONC = '01'                         
                       THEN NEXT SENTENCE
                       ELSE
***************  agrego 16-4-13 para los retrocativos de conc 02 
                       IF COD-RETRO = 'M'              
                            THEN  
   ************************************************agrego el 21-05-09
            IF CONC = '02' AND ((POS-5 = '2' OR '3' OR '4' OR '5' OR '6'
                 OR '7' OR '8' OR '9' OR 'A' OR 'I' OR 'T' OR 'R' OR 'M'
            OR 'G' OR '1' OR 'E' OR 'C' OR 'F' OR 'J') OR POS-5-6 = 'GM'
 *********                                         agrego el 05-07-10
                       OR (POS-5-8 = 'S1S3' OR 'S103'))
 *********                                         agrego el 29-06-12
                       OR POS-5 = 'B'
                          THEN NEXT SENTENCE
                          ELSE GO TO FIN-CALCULA-HABER-SEPA.
    *****   AGREGO 04-04-08
           MOVE ZEROS   TO A-IOMA (I)
                           A-HABER (I)  ANTIG-AUX ANTIG-NB
                           A-ANTIG (I) MARCAAMP.

***************  lo saco el 28-3-14  pues re-usamos pos-17-18
  ********  IF POS-1-4 = 'M049'
  *********** THEN IF POS-17 = 'A'
  ************        THEN MOVE REG-MAE TO REGISTRO (I)
  ***********              MOVE SPACE TO A-POS-17 (I)
 *************             PERFORM VER-DOCU THRU VER-DOCU-X
 ************              MOVE  1    TO MARCAAMP
 *************             IF COD-RETRO = 'A' OR 'R'
 ************                 THEN MOVE '*' TO A-POS-140 (I)
 ***********                       GO TO FIN-CALCULA-HABER-SEPA
***************               ELSE GO TO FIN-CALCULA-HABER-SEPA.


            IF ANTIG1 = ZEROS
               THEN GO TO SIGO-SEPA.

            IF POS-1-4 = 'M068'
               THEN IF SI-LETRA-ANTIG = 'A'
                     THEN GO TO SALTA-SEPA
                     ELSE IF SI-LETRA-ANTIG = 'B' AND POS-5 = '9'
                            THEN GO TO SALTA-SEPA.

            IF POS-1-4 = 'M115'
                        THEN GO TO SALTA-SEPA.

   **********                     23-10-15 Lo suspendo desde dic15 pq aviso la mdad q es antig por el anio15, no todoslos anios
   ******** IF POS-1-4 = 'M028' AND FECHA-MES > '201509'
   *********            THEN MOVE ' ' TO SI-LETRA-ANTIG
   *********                 GO TO SALTA-SEPA.
   **********                     23-10-15

   **********                     28-01-09
            IF POS-1-4 = 'M081'
               THEN IF POS-5 = '9'
                   THEN IF ANTIG1 > 40 THEN
                                        MOVE 40 TO ANTIG1
                                       ELSE NEXT SENTENCE
                   ELSE IF ANTIG1 > 24 THEN
                                        MOVE 25 TO ANTIG1
                                        GO TO SALTA-SEPA.
   **********                     28-01-09
   *********DISPLAY 'ANTIG 1  ' ANTIG1 '    ' H-ANTIG (ANTIG1).
   ******** DISPLAY REGISTRO (I).
            MOVE ZEROS TO INDX.
            IF SI-LETRA-ANTIG  NOT = SPACE
               THEN PERFORM SEPARA-ANTIG THRU FIN-SEPARA-ANTIG.
   ******** DISPLAY 'ANTIG 1  ' ANTIG1 '    ' H-ANTIG (ANTIG1).
   ******** DISPLAY REGISTRO (I).


       SALTA-SEPA.
             IF ANTIG1 = 00
                   THEN MOVE ZEROS TO ANTIG-AUX
                         GO TO SIGO-SEPA.

            IF ANTIG1 > 40
               THEN
                   COMPUTE ANTIG1 = ANTIG1 - 40 + 1
                   COMPUTE ANTIG-AUX =
                            H-ANTIG (41) + H-ANTIG (ANTIG1)
               ELSE
                    ADD 1 TO ANTIG1
                    IF H-ANTIG (ANTIG1) > ZEROS
                       THEN MOVE H-ANTIG (ANTIG1) TO ANTIG-AUX. 
       SIGO-SEPA.
           IF POS-1  = 'W' THEN GO TO WWWW-SEPA.
           IF (POS-1-4 = 'M090' AND POS-5 = '9') OR
              (POS-1-4 = 'M090' AND POS-5 NOT = 'S') OR
              (POS-1-4 = 'M090' AND POS-5-8 = 'S252') OR
              (POS-1-4 = 'M132' AND POS-5 = '9') OR
              (POS-1-4 = 'M012' AND POS-5 = '9') OR
              (POS-1-4 = 'M065' AND POS-5 = '9') OR
              (POS-1-4 = 'M031' AND POS-5 = '9') OR
              (POS-1-4 = 'M037' AND POS-5 = '9') OR
              (POS-1-4 = 'M003' AND POS-5 = '9') OR
              (POS-1-4 = 'M080' AND POS-5 = '9' AND
                  (POS-6 = 'B' OR 'E' OR 'H' OR 'I' OR 'K')) OR
              (POS-1-4 = 'M060' AND (POS-5 = 'B' OR 'E' OR 'H')) OR
              (POS-1-4 = 'M012' AND                                
                    (POS-5 = '1' OR '2' OR 'P' OR '3' OR '4' OR '5'
                                 OR '6' OR 'A'))
                    THEN COMPUTE A-HABER (I) = H-HABER
                         MOVE '*' TO MAR-CMH
                         GO TO WWWW-SEPA.

             IF POS-1-4 = 'M060' AND POS-5 = '7' AND POS-6 NOT = 'S'
                    THEN NEXT SENTENCE
                    ELSE COMPUTE A-HABER (I) = H-HABER.                        


      ***********************                                         
           IF SUMAFIJ1 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ1.
           IF SUMAFIJ2 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ2.
           MOVE ZEROS TO SUFI1 SUFI2.
           IF POS-1-4 = 'M074'                                
              THEN MOVE SUMAFIJ1 TO SUFI1                              
                   IF CONC = '01'
                         THEN NEXT SENTENCE
                         ELSE MOVE SUMAFIJ2 TO A-IOMA (I)

************  OJO VER IOMA
              ELSE
           IF (POS-1-4 = 'M049' AND POS-5 = 'O' ) OR
              (POS-1-4 = 'M068' AND POS-5 NOT = '9' ) OR
              (POS-1-4 = 'M060' AND (POS-5 = 'B' OR 'E' OR 'H'))   
                 THEN MOVE SUMAFIJ1 TO SUFI1                              
                 ELSE
                      MOVE SUMAFIJ1 TO SUFI1                              
                      MOVE SUMAFIJ2 TO SUFI2.                              

      ************************F                ARREGLO 25/03/96
       WWWW-SEPA.
    *****  DISPLAY A-NRO (I) ' ' A-APYN (I) ' PORC ' PORC ' ANTIG-AUX '
    *****     ANTIG-AUX.
           COMPUTE A-HABER (I) = A-HABER (I) * PORC / 100.
           COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
           MOVE ANTIG-AUX  TO  A-ANTIG (I).
      **
            IF POS-1-4 = 'M068'
               THEN IF SI-LETRA-ANTIG = 'A'
                     THEN GO TO SIN-RECALC-ANTIG-SEPA
                     ELSE IF SI-LETRA-ANTIG = 'B' AND POS-5 = '9'
                          THEN GO TO SIN-RECALC-ANTIG-SEPA.
            IF POS-1-4 = 'M069' AND SI-LETRA-ANTIG = 'L'
               THEN GO TO SIN-RECALC-ANTIG-SEPA.
           MOVE ZEROS TO ANTIANT.
           IF SI-LETRA-ANTIG NOT = SPACE
              THEN IF ANTIG-NB > ZEROS  THEN
                   MOVE ZEROS TO ANTIG-AUX                  
                   PERFORM RECALCULA-ANTIG THRU FIN-RECALCULA-ANTIG
                   COMPUTE A-ANTIG (I) = A-ANTIG (I) + ANTIG-AUX.
      **

       SIN-RECALC-ANTIG-SEPA.
  ********                                
           MOVE 1 TO POR9-10 POR6-8 POR7-8.
           COMPUTE A-HABER (I) = H-HABER * PORC / 100. 
 ****************  acumulador para M049O 
           COMPUTE BONDEDFC = A-HABER (I) + A-ANTIG (I). 

           IF (POS-1-4 = 'M118' AND POS-5-6 = '9G') OR
    ********  (POS-1-4 = 'M107' AND POS-5-6 = 'JR'      mov.set14 deshab pq se cargaron con cant de jorn en hisc.
    *****                       AND FECHA-MES < '201310') OR
              (POS-1-4 = 'M019' AND POS-5-6 = 'JO') OR
              (POS-1-4 = 'M020' AND POS-5 = 'J') OR                     
              (POS-1-4 = 'M024' AND POS-5 = 'J') OR                  
              (POS-1-4 = 'M088' AND                                  
                (POS-5-8 = 'JORT' OR  'JORE' OR 'JORC')) OR
              (POS-1-4 = 'M111' AND                                  
                (POS-5-8 = 'JOR1' OR 'JOR2' OR 'JOR3' OR
                               'JOR4'))
             THEN
                 COMPUTE A-HABER (I) = (H-HABER  * POS-9-10N)
                                             * PORC / 100
                 MOVE POS-9-10N TO POR9-10   
                 MOVE ZEROS TO  A-ANTIG (I) 
                 GO TO VER-SUPLE.          


           IF (POS-1-4 = 'M004' AND (POS-5-8 = '7645' OR '7047')) OR
              (POS-1-4 = 'M016' AND POS-5-8 = '7000') OR
              (POS-1-4 = 'M037' AND POS-5-8 = '7000') OR
              (POS-1-4 = 'M057' AND POS-5-8 = '7002') OR
              (POS-1-4 = 'M060' AND (POS-5-8 = '7002' OR '7000')) OR
              (POS-1-4 = 'M136' AND POS-5-8 = '7000')
                  THEN COMPUTE A-HABER (I) = (H-HABER  * POS-9-10N)
                                             * PORC / 100
                       COMPUTE A-ANTIG (I) = A-ANTIG (I) * POS-9-10N
                       MOVE POS-9-10N TO POR9-10   
                       GO TO VER-SUPLE.          

           IF POS-1-4 = 'M060'
                  THEN IF POS-5 = '7' AND POS-6 NOT = 'S'
                      THEN COMPUTE A-HABER (I) = (H-HABER * POS-6-8N)
                                             * PORC / 100
                           MOVE ZEROS TO  A-ANTIG (I) 
                           MOVE POS-6-8N TO POR6-8    
                           GO TO VER-POS-11-12-SEPA.

           IF (POS-1-4 = 'M081' AND POS-5-6 = '70')      
                  THEN COMPUTE A-HABER (I) = (H-HABER * POS-7-8N)
                                             * PORC / 100
                       COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-7-8N
                       MOVE POS-7-8N TO POR7-8    
                       GO TO VER-SUPLE.     

           IF POS-1-4 = 'M052' AND POS-5 = '9' AND POS-9-10 = '40'    
                  THEN COMPUTE A-HABER (I) = (H-HABER * 40 / 36)
                                             * PORC / 100
                       COMPUTE A-ANTIG (I) = A-ANTIG(I) * 40 / 36 
                       GO TO VER-SUPLE.     

       VER-SUPLE.     
           MOVE ZEROS TO HABER-M117. 
           IF  MAR-CMH = '*' THEN
                               GO TO VER-POS-11-12-SEPA.
           IF (POS-1-4 = 'M059' AND POS-5 = 'O')     
                             THEN
                               GO TO VER-POS-11-12-SEPA.
                          
           IF POS-1-4 = 'M079' AND FECHA-MES > '201304'
****************  porque esta sumado todo en la sumafij2
                      THEN GO TO NOPAGASUPL2.

           IF POS-1-4 = 'M004' AND (POS-5-8 = '7047' OR 
                                              '7645')   
            THEN
             IF SUPL2 > ZEROS                            
              THEN                                        
               ADD 1 TO I
               MOVE REG-MAE TO REGISTRO (I)
               MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
               MOVE '02' TO A-CONC (I) 
               COMPUTE A-HABER (I) = SUPL2 * POR9-10 * PORC / 100   
               MOVE '*BON.M004 DOC.' TO A-CODIGO (I) 
               GO TO NOPAGASUPL2.

           IF POS-1-4 = 'M005' AND FECHA-MES > '201307'
****************  muestro supl2 con leyendas               
            THEN
             IF SUPL2 > ZEROS                            
              THEN IF (POS-5-7 = '9TT' OR '20C' OR '20G'
                        OR '20T') OR POS-5-8 = 'S252'
                 THEN
                 ADD 1 TO I
                 MOVE REG-MAE TO REGISTRO (I)
                 MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                 MOVE '02' TO A-CONC (I) 
                 COMPUTE A-HABER (I) = SUPL2 * PORC / 100             
                 IF POS-5-7 = '9TT'                  
                  THEN MOVE '*BON.M005 T.I.' TO A-CODIGO (I) 
                       GO TO NOPAGASUPL2 
                  ELSE IF POS-5-7 = '20C'                 
                   THEN MOVE '*BON.M005 T.C.' TO A-CODIGO (I) 
                        GO TO NOPAGASUPL2 
                   ELSE IF POS-5-7 = '20G'                 
                    THEN MOVE '*BON.M005 A.G.' TO A-CODIGO (I) 
                         GO TO NOPAGASUPL2 
                    ELSE IF POS-5-7 = '20T'                 
                      THEN MOVE '*BON.M005 C.G.' TO A-CODIGO (I) 
                           GO TO NOPAGASUPL2 
                      ELSE IF POS-5-8 = 'S252'                
                      THEN MOVE '*BON.M005 GTOS' TO A-CODIGO (I) 
                           GO TO NOPAGASUPL2.

           IF POS-1-4 = 'M019'                           
            THEN
             IF SUPL2 > ZEROS                            
              THEN IF POS-5 = 'J'                          
                 THEN
                 ADD 1 TO I
                 MOVE REG-MAE TO REGISTRO (I)
                 MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                 MOVE '02' TO A-CONC (I) 
                 COMPUTE A-HABER (I) = SUPL2 * PORC / 100             
                 MOVE '*BON.MUN.SUPL2'  TO A-CODIGO (I) 
                 GO TO NOPAGASUPL2.

           IF SUPL2 > ZEROS                            
                      THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = (SUPL2 * POR9-10 * POR6-8       
                                          * POR7-8) * PORC / 100 
                          COMPUTE HABER-M117 = HABER-M117 + A-HABER (I)
                          MOVE '*BON.MUN.SUPL2'  TO A-CODIGO (I).

 ****************** VER SUMAFIJ2
       NOPAGASUPL2.                  
                          
           IF SUFI2 > ZEROS                            
                      THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                       COMPUTE A-HABER (I) = (SUFI2 * POR9-10 * POR6-8       
                                          * POR7-8) * PORC / 100 
                          COMPUTE HABER-M117 = HABER-M117 + A-HABER (I)
                          MOVE '*BON.MUN.SUMF2'  TO A-CODIGO (I).

       VER-POS-11-12-SEPA.
           IF POS-11-12 NUMERIC AND POS-11-12 GREATER '00'
              THEN
                   IF POS-1-4 = 'M061' AND POS-5 = 'B'
                     THEN NEXT SENTENCE
                     ELSE IF POS-1 = 'W'
                          THEN NEXT SENTENCE
                          ELSE
                          IF PORC = ZEROS 
                                THEN 
                   MOVE POS-11-12 TO PORC-AUX
                   COMPUTE A-HABER (1) = A-HABER (1) +
                   ((H-HABER + SUPL1 + SUMAFIJ1) * PORC-AUX / 100) 
                          GO TO SINBONS 
                                ELSE
                          MOVE POS-11-12 TO PORC-AUX
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE A-HABER (I) =
                   ((H-HABER + SUPL1 + SUMAFIJ1) * PORC-AUX / 100)
                          COMPUTE HABER-M117 = HABER-M117 + A-HABER (I)
                          MOVE '*BON.P.DIRECT'  TO A-CODIGO (I). 
  **************          GO TO SINBONS.

           IF  MAR-CMH = '*' THEN
                          GO TO SINBONS.
           IF (POS-1-4 = 'M059' AND POS-5 = 'O')     
                             THEN
                          GO TO SINBONS.
                          
           IF SUPL1 > ZEROS                            
                      THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = (SUPL1 * POR9-10 * POR6-8       
                                          * POR7-8) * PORC / 100 
                          COMPUTE HABER-M117 = HABER-M117 + A-HABER (I)
                          MOVE '*BON.MUN.SUPL1'  TO A-CODIGO (I).

    *****   AGREGO 12-09-14
           IF POS-1-4 = 'M079'                                     
                     THEN IF POS-5 = '9'
                          THEN 
                          MOVE '*B.M079 FUNCIO'  TO A-CODIGO (I).

 ****************** VER SUMAFIJ1
                          
           IF SUFI1 > ZEROS                            
                      THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                       COMPUTE A-HABER (I) = (SUFI1 * POR9-10 * POR6-8       
                                          * POR7-8) * PORC / 100 
                          COMPUTE HABER-M117 = HABER-M117 + A-HABER (I)
                          MOVE '*BON.MUN.SUMF1'  TO A-CODIGO (I).


       SINBONS.          
    *****   AGREGO 11-02-04
           IF POS-1-4 =  'M114'                            
              THEN   PERFORM SUMAHABER THRU FIN-SUMAHABER
                           VARYING J FROM 1 BY 1 UNTIL J > I
                     PERFORM VER-M114 THRU F-VER-M114.

           IF POS-1-4 = 'M002'                                     
                   THEN
                   IF SI-LETRA-ANTIG NOT = SPACE AND
                                             ANTIG-NB > ZEROS
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          COMPUTE A-HABER (I) = ANTIG-AUX * 2
                          MOVE '*BONESCA.M002   '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I).

 *********         agrego 21-4-15
            IF POS-1-4 = 'M071'  AND INDX > 10                          
                                 AND FECHA-MES > '201301' 
                      THEN
                 PERFORM VER-M071 THRU FIN-VER-M071.                 
 *********         agrego 21-4-15

       FIN-CALCULA-HABER-SEPA. EXIT.

****************
*****************
       SUMAHABER.     
           COMPUTE SUP-AUX = SUP-AUX + A-HABER (J).
       FIN-SUMAHABER.  EXIT.
****************

       CALCULA-HABER.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
    *****   AGREGO 04-04-08
  *******  IF POS-1-4 =  'M117' AND FECHA-MES > '200804'cambio fecha pq salio el camb de escalafon
           IF POS-1-4 =  'M117' AND FECHA-MES > '200711'                                   
                        THEN
              IF CONC = '01' THEN NEXT SENTENCE               
                             ELSE                            
   ************************************************agrego el 21-05-09
            IF CONC = '02' AND ((POS-5 = '2' OR '3' OR '4' OR '5' OR '6'                           
                 OR '7' OR '8' OR '9' OR 'A' OR 'I' OR 'T' OR 'R' OR 'M'                           
                       OR 'E' OR 'C' OR 'F' OR 'J') OR POS-5-6 = 'GM' 
 *********                                         agrego el 05-07-10
                       OR (POS-5-8 = 'S1S3' OR 'S103'  
                       OR 'S1N3' OR 'S1X3'))
 *********                                         agrego el 29-06-12
                       OR POS-5 = 'B'                 
                          THEN NEXT SENTENCE             
                          ELSE GO TO FIN-CALCULA-HABER. 
    *****   AGREGO 04-04-08
           MOVE ZEROS   TO A-IOMA (I)
                           A-HABER (I)  ANTIG-AUX ANTIG-NB
                           A-ANTIG (I) MARCAAMP.

            IF POS-1-4 = 'M049'
 *********    THEN IF FECHA-MES > '200510' AND POS-17 = 'A'
              THEN IF POS-17 = 'A'
                      THEN MOVE REG-MAE TO REGISTRO (I)
                           MOVE SPACE TO A-POS-17 (I)
                           PERFORM VER-DOCU THRU VER-DOCU-X
                           MOVE  1    TO MARCAAMP        
                           IF COD-RETRO = 'A' OR 'R'
                              THEN MOVE '*' TO A-POS-140 (I) 
                                   GO TO FIN-CALCULA-HABER 
                              ELSE GO TO FIN-CALCULA-HABER.

     *****  IF POS-1-4 = 'M065'     en mov.mayo 08 se aplica nuevamente,
     *****     THEN IF CONC = '02'  con reajuste desde que se congelaron
     *****            THEN MOVE REG-MAE TO REGISTRO (I)
     ******                GO TO FIN-CALCULA-HABER.

            IF ANTIG1 = ZEROS
               THEN GO TO SIGO.
 ***********  la M059O hasta el 31-5-08  antig = lo que hay en hisc
***********           Desde 1-6-08      antig = a tabla 1 %         
    ******  IF POS-1-4 = 'M059' AND POS-5 = 'O' AND FECHA-MES > '200901'
    ******     THEN IF SI-LETRA-ANTIG > 'A' AND            
    ********                  SI-LETRA-ANTIG < 'Q'                
    *******          THEN GO TO SALTA.
            IF POS-1-4 = 'M059' AND POS-5 = 'O'                                
                           THEN 
                           IF FECHA-MES > '200809'
     **********          va a tabla de antiguedades
                                THEN NEXT SENTENCE
                                ELSE 
     **********          va a HISCAR                        
                                    GO TO SALTA.


            IF POS-1-4 = 'M068'
               THEN IF SI-LETRA-ANTIG = 'A'
                     THEN GO TO SALTA
                     ELSE IF SI-LETRA-ANTIG = 'B' AND POS-5 = '9'
                             THEN GO TO SALTA.

   *********IF POS-1-4 = 'M010' OR 'M115' lo cambio 10-02-10
            IF POS-1-4 = 'M115'
                        THEN GO TO SALTA. 
            IF POS-1-4 = 'M010' AND FECHA-MES < '201001'
                        THEN GO TO SALTA. 
   **********                     28-01-09
            IF POS-1-4 = 'M081'
               THEN IF POS-5 = '9'
                   THEN IF ANTIG1 > 40 THEN                           
                                        MOVE 40 TO ANTIG1 
                                       ELSE NEXT SENTENCE
                   ELSE IF ANTIG1 > 24 THEN                               
                                        MOVE 25 TO ANTIG1 
                                        GO TO SALTA.
   **********                     28-01-09
            MOVE ZEROS TO INDX.
            IF SI-LETRA-ANTIG  NOT = SPACE
               THEN PERFORM SEPARA-ANTIG THRU FIN-SEPARA-ANTIG.


       SALTA.
             IF ANTIG1 = 00            
                   THEN MOVE ZEROS TO ANTIG-AUX
                         GO TO SIGO.
 
            IF ANTIG1 > 40
               THEN
                   COMPUTE ANTIG1 = ANTIG1 - 40 + 1
                   COMPUTE ANTIG-AUX =
                            H-ANTIG (41) + H-ANTIG (ANTIG1)
               ELSE
                    ADD 1 TO ANTIG1
                    IF H-ANTIG (ANTIG1) > ZEROS
                       THEN MOVE H-ANTIG (ANTIG1) TO ANTIG-AUX.
       SIGO.
           IF POS-1  = 'W' THEN GO TO WWWW.
  ****** 21-09-12 desde 201101 se carga SUP1 SUP2 para CMH salgan como boni        
           IF  FECHA-MES > '201101'               
                THEN
                IF (POS-1-4 = 'M090' AND POS-5 = '9') OR                                          
                   (POS-1-4 = 'M090' AND POS-5 NOT = 'S') OR                                      
                   (POS-1-4 = 'M090' AND POS-5-8 = 'S252')                                         
                    THEN COMPUTE A-HABER (I) = H-HABER                     
                         GO TO WWWW.           
  ****** 29-06-12 desde 201207 se carga SUP1 SUP2 para CMH salgan como boni        
           IF  FECHA-MES > '201207'               
                THEN
                IF (POS-1-4 = 'M132' AND POS-5 = '9') OR                                          
                   (POS-1-4 = 'M012' AND POS-5 = '9') OR                                          
                   (POS-1-4 = 'M065' AND POS-5 = '9') OR                                          
                   (POS-1-4 = 'M031' AND POS-5 = '9') OR                                           
                   (POS-1-4 = 'M037' AND POS-5 = '9') OR                                           
                   (POS-1-4 = 'M080' AND POS-5 = '9' AND                                           
                (POS-6 = 'B' OR 'E' OR 'H' OR 'I' OR 'K')) OR
                   (POS-1-4 = 'M060' AND                                                          
                (POS-5 = 'B' OR 'E' OR 'H')) 
                    THEN COMPUTE A-HABER (I) = H-HABER                     
                         MOVE '*' TO MAR-CMH
                         GO TO WWWW.           
           IF  FECHA-MES > '201208'               
                THEN
                IF POS-1-4 = 'M003' AND POS-5 = '9'                                           
                    THEN COMPUTE A-HABER (I) = H-HABER                     
                         MOVE '*' TO MAR-CMH
                         GO TO WWWW.           


  ******  8-11-10 desde 201005 se carga supl2 con diferencia a 200$                
           IF POS-1-4 = 'M079' AND ( FECHA-MES > '199810' AND
                                     FECHA-MES < '201005')
              THEN COMPUTE A-HABER (I) = H-HABER + SUPL1
                 IF FECHA-MES > '200203' AND FECHA-MES < '200404'
                  THEN COMPUTE A-IOMA (I) = H-HABER * PORC / 100              
                  ELSE NEXT SENTENCE 
              ELSE
                IF POS-1-4 = 'M012' AND FECHA-MES > '201203' AND                            
                    (POS-5 = '1' OR '2' OR 'P' OR '3' OR '4' OR '5'                                
                                 OR '6' OR 'A')                           
                    THEN COMPUTE A-HABER (I) = H-HABER                     
                    ELSE                     
                IF POS-1-4 = 'M060' AND POS-5 = '7' AND POS-6 NOT = 'S'                            
                    THEN NEXT SENTENCE
                    ELSE                     
                IF POS-1-4 = 'M019' AND POS-5 = 'J'                                                
                     THEN COMPUTE A-HABER (I) = H-HABER                     
                     ELSE COMPUTE A-HABER (I) = H-HABER + SUPL1 + SUPL2.


      ***********************                 ARREGLO 22/05/97
           IF SUMAFIJ1 NOT NUMERIC                               
              THEN MOVE ZEROS TO SUMAFIJ1. 
           IF SUMAFIJ2 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ2.

           IF POS-1-4 = 'M074' AND FECHA-MES > '200407'
              THEN COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1
                   IF CONC = '01'
                         THEN NEXT SENTENCE
                         ELSE            
      *******            IF FECHA-MES < '201002'
      *******             THEN
                    IF FECHA-MES > '200501' AND FECHA-MES < '200609'     
                        THEN COMPUTE A-IOMA (I) = SUMAFIJ2 * 2
                        ELSE MOVE SUMAFIJ2 TO A-IOMA (I)
      ******              ELSE NEXT SENTENCE
              ELSE
           IF POS-1-4 = 'M049' AND POS-5 = 'O'                      
              AND  FECHA-MES > '200501'
                 THEN
              COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 
                 ELSE
           IF POS-1-4 = 'M059' AND POS-5 = 'O'                      
              AND  FECHA-MES > '200601'
                 THEN
              COMPUTE A-HABER (I) = H-HABER                 
                 ELSE
           IF POS-1-4 = 'M068' AND FECHA-MES > '200809'                        
                               AND POS-5 NOT = '9'  
                         THEN
              COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 
                         ELSE
           IF POS-1-4 = 'M060' AND FECHA-MES > '200901'                        
                               AND (POS-5 = 'B' OR 'E' OR 'H')  
                         THEN
              COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 
                         ELSE
              COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 + SUMAFIJ2.
      ***********************C                ARREGLO 25/03/96
      ***          IF FECHA-MES NOT < '199606'
      ***              THEN IF POS-5 NOT = '9' THEN
      ***     COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 + SUMAFIJ2
      ***                                       ELSE
      ***                   IF FECHA-MES > '199606' THEN
      ***     COMPUTE A-HABER (I) = A-HABER (I) + SUMAFIJ1 + SUMAFIJ2.
      ***
      ***
      ************************F                ARREGLO 25/03/96
       WWWW.
    *****  DISPLAY A-NRO (I) ' ' A-APYN (I) ' PORC ' PORC ' ANTIG-AUX '
    *****     ANTIG-AUX.
           COMPUTE A-HABER (I) = A-HABER (I) * PORC / 100.
           COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
           MOVE ANTIG-AUX  TO  A-ANTIG (I).
      **
            IF POS-1-4 = 'M068'
               THEN IF SI-LETRA-ANTIG = 'A'
                     THEN GO TO SIN-RECALC-ANTIG
                     ELSE IF SI-LETRA-ANTIG = 'B' AND POS-5 = '9'
                          THEN GO TO SIN-RECALC-ANTIG.
    ********IF POS-1-4 = 'M010'  lo saco el 10-02-10
            IF POS-1-4 = 'M010' AND FECHA-MES < '201001'
               THEN GO TO SIN-RECALC-ANTIG.
            IF POS-1-4 = 'M069' AND SI-LETRA-ANTIG = 'L'
               THEN GO TO SIN-RECALC-ANTIG.
           IF SI-LETRA-ANTIG NOT = SPACE
              THEN IF ANTIG-NB > ZEROS  THEN
                   MOVE ZEROS TO ANTIG-AUX
                   PERFORM RECALCULA-ANTIG THRU FIN-RECALCULA-ANTIG
                   COMPUTE A-ANTIG (I) = A-ANTIG (I) + ANTIG-AUX.
      **
       SIN-RECALC-ANTIG.
*********** lo agrego el 28-3-14 para M049O
           COMPUTE BONDEDFC = (H-HABER * PORC / 100) + A-ANTIG (I).

  ******** AGREGADO  MOV. AGOS2011                                           
           IF POS-1-4 = 'M118' AND FECHA-MES > '200801'
             THEN 
              IF POS-5-6 = '9G'                     
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                               
                 MOVE ZEROS TO  A-ANTIG (I).

           IF POS-1-4 = 'M004'
             THEN IF POS-5-8 = '7645' OR '7047'
                  THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N
                       COMPUTE A-ANTIG (I) = A-ANTIG (I) * POS-9-10N 
                       GO TO VER-POS-11-12.

           IF POS-1-4 = 'M016'
             THEN IF POS-5-8 = '7000'               
                  THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N
                         COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N 
                         GO TO VER-POS-11-12.

           IF POS-1-4 = 'M037'
             THEN IF POS-5-8 = '7000'               
                  THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N
                         COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N 
                         GO TO VER-POS-11-12 
                  ELSE IF POS-5-8 = '7150' OR '7160' OR '7180'               
                      THEN COMPUTE A-HABER (I) = H-HABER * PORC / 100       
    *********              COMPUTE A-HABER (I) = A-HABER (I) * POS-6-8N   
   ************            COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-6-8N 
                           GO TO VER-POS-11-12.

           IF POS-1-4 = 'M060'
             THEN IF POS-5-8 = '7000' OR '7002'               
                  THEN COMPUTE A-HABER (I) = ((H-HABER + SUPL1 + SUPL2)                     
                                   * PORC / 100) * POS-9-10N
                       COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N 
                       GO TO VER-POS-11-12 
                  ELSE IF POS-5 = '7' AND POS-6 NOT = 'S'     
                      THEN COMPUTE A-HABER (I) = H-HABER * PORC / 100       
                           COMPUTE A-HABER (I) = A-HABER (I) * POS-6-8N   
                           COMPUTE A-ANTIG (I) = 0                       
                           GO TO VER-POS-11-12.
                  
           IF POS-1-4 = 'M081'
             THEN IF POS-5-6 = '70'               
                  THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-7-8N 
                       COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-7-8N  
                       GO TO VER-POS-11-12.
                       
           IF POS-1-4 = 'M057'
     ******  THEN IF POS-5-8 = 'J130' OR 'J140' OR 'J230' OR 
     ******                    'J240' OR 'J330' OR 'J340'                 
     ******   lo sacro dde mov jul12, ppto ene05 pq cargo en hiscar.
     *******      THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N
     ********            COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N 
                    THEN IF POS-5-8 = '7002'               
                     THEN COMPUTE A-HABER (I) = A-HABER(I) * POS-9-10N
                          COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N.           
  
  ******** AGREGADO PARA MOV. SET 2011                                           
           IF POS-1-4 = 'M020' AND FECHA-MES > '201101'
             THEN 
              IF POS-5 = 'J'                     
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                               
                 MOVE ZEROS TO  A-ANTIG (I).

  ******** AGREGADO PARA MOV. AGOS2010                                           
           IF POS-1-4 = 'M024' AND FECHA-MES > '200912'
             THEN 
              IF POS-5 = 'J'                     
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                               
                 MOVE ZEROS TO  A-ANTIG (I).

  ******** AGREGADO PARA MOV. JUL10                                              
           IF POS-1-4 = 'M088' AND FECHA-MES > '200801'
             THEN 
              IF POS-5-8 = 'JORT' OR  'JORE' OR 'JORC'                     
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                               
                 MOVE ZEROS TO  A-ANTIG (I).

  ******** AGREGADO PARA MOV. OCTUBRE DE 2007 CARGOS NUEVOS                      

           IF POS-1-4 = 'M111' AND FECHA-MES > '200707'
             THEN 
              IF POS-5-8 = 'JOR1' OR 'JOR2' OR 'JOR3' OR              
                               'JOR4'                 
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                                
                 MOVE ZEROS TO  A-ANTIG (I).
  ******** AGREGADO PARA MOV. ABR09                                              
           IF POS-1-4 = 'M107' AND FECHA-MES > '200804'
             THEN 
              IF POS-5-6 = 'JR'                                     
               THEN
                 COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N                               
                 MOVE ZEROS TO  A-ANTIG (I).
  ******** AGREGADO PARA MOV. ABR09                                              
                    
     ***** IF POS-1-4 = 'M019' AND FECHA-MES > '200810'  cambio en mov jun14.
           IF POS-1-4 = 'M019' AND FECHA-MES > '200701'
             THEN 
              IF POS-5-6 = 'JO'                                     
               THEN
                 COMPUTE A-HABER (I) = (A-HABER (I) * POS-9-10N)                              
                                       + SUPL2 * PORC / 100
                 MOVE ZEROS TO  A-ANTIG (I).
                    
    **** AGREGADO PARA MOV. 01/2007
   
           IF POS-1-4 = 'M136'
             THEN IF POS-5-8 = '7000'  AND  FECHA-MES > '200608'            
                  THEN COMPUTE A-HABER (I) = A-HABER (I) * POS-9-10N
                       COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-9-10N
                       GO TO VER-POS-11-12.                        

    *****  PERFORM VER-M061 THRU F-VER-M061.
       VER-POS-11-12.
           IF POS-11-12 NUMERIC AND POS-11-12 GREATER '00'
              THEN
                   IF POS-1-4 = 'M061' AND POS-5 = 'B'                   
                     THEN NEXT SENTENCE
                     ELSE IF POS-1 = 'W'
                          THEN NEXT SENTENCE
                          ELSE
                          MOVE POS-11-12 TO PORC-AUX
                          COMPUTE A-HABER (I) = A-HABER (I) +                   
                   ((H-HABER + SUPL1 + SUMAFIJ1) * PORC-AUX / 100).


    *****   AGREGO 11-02-04
           IF POS-1-4 =  'M114' AND FECHA-MES > '200301'
              THEN   PERFORM VER-M114 THRU F-VER-M114.    

    *****   AGREGO 13-08-12
           IF FECHA-MES < '201206' AND POS-1-4 = 'M098'     
                    AND  POS-5-6N = 'JR'                                       
               THEN COMPUTE A-HABER (I) = (H-HABER * POS-7-8N                  
                          + SUMAFIJ2) * PORC / 100
                    COMPUTE A-ANTIG (I) = A-ANTIG(I) * POS-7-8N. 

           IF POS-1-4 = 'M002' AND FECHA-MES > '200701'
                   THEN
                   IF SI-LETRA-ANTIG NOT = SPACE AND
                                             ANTIG-NB > ZEROS              
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          COMPUTE A-HABER (I) = ANTIG-AUX * 2                                   
                          MOVE '*BONESCA.M002   '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I).
     ******** agrego el 6-01-11
       VER-M009.            
           IF CONC = '01' AND EME = ' ' AND
              POS-1-4 = 'M009' AND (FECHA-MES = '200906'
                 OR '200907' OR '200908')
                     THEN 
                   IF (POS-5-8 = 'S050' OR 'S252')                      
                       THEN NEXT SENTENCE
                       ELSE COMPUTE A-HABER (I) = A-HABER (I) * 1.10.

 *********         agrego 21-4-15
            IF POS-1-4 = 'M071'  AND INDX > 10                          
                                 AND FECHA-MES > '201301' 
                      THEN
                 PERFORM VER-M071 THRU FIN-VER-M071.                 
 *********         agrego 21-4-15
       FIN-CALCULA-HABER. EXIT.
       VER-DOCU.            
      *     IF NRO = '1018214650'
      *        THEN IF A-HABER (I) = 136.28
      *                THEN MOVE 156.57 TO A-HABER (I).
       VER-DOCU-X. EXIT.
       VERTITU-M090.        
            IF POS-17 = '1'            
               THEN MOVE 30 TO TITUM090              
               ELSE
            IF POS-17 = '2'            
               THEN MOVE 23 TO TITUM090              
               ELSE
            IF POS-17 = '3'            
               THEN MOVE 23 TO TITUM090              
               ELSE
            IF POS-17 = '4'            
               THEN MOVE 17 TO TITUM090              
               ELSE
            IF POS-17 = '5'            
               THEN MOVE 10 TO TITUM090               
               ELSE
            IF POS-17 = '6'            
               THEN MOVE 5  TO TITUM090.              
       F-VERTITU-M090. EXIT.
       VERIFICA-HABER.            
            
           COMPUTE HABER-ANT = HABER + ANTIG.
           COMPUTE HABER-ACT = A-HABER (I) + A-ANTIG (I).
           IF HABER-ANT > HABER-ACT
              THEN DISPLAY  '******VERFICA     ****** ' REG-MAE
                   MOVE A-HABER (I) TO HABER-MUNI
                   MOVE A-ANTIG (I) TO ANTIG-MUNI
                   MOVE HABER       TO A-HABER (I)
                   MOVE ANTIG       TO A-ANTIG (I). 
       FIN-VERIFICA-HABER. EXIT.      
       SEPARA-ANTIG.
            MOVE 0 TO MARCA1 MARCA2 INDX.
            PERFORM BUSCA-IND-LETRA-1 THRU FIN-BUSCA-IND-LETRA-1
             VARYING J FROM 1 BY 1 UNTIL J > 23.
            IF MARCA1 = 0 THEN
            PERFORM BUSCA-IND-LETRA-2 THRU FIN-BUSCA-IND-LETRA-2
             VARYING J FROM 1 BY 1 UNTIL J > 23.
     ***   IF NRO = '7064817240'
     ***            THEN DISPLAY NRO ' I-LETRA ' I-LETRA ' INDX '
     ***                 INDX ' LETRA-ANTIG ' SI-LETRA-ANTIG. 
            IF J > 30
               THEN MOVE ANTIG1 TO ANTIG1-N ANTIG-NB
                    COMPUTE ANTIG1-N = ANTIG1-N - I-LETRA
                    IF ANTIG1-N NOT > ZEROS
   ********            THEN DISPLAY '** ANTIGUEDAD MENOR O IGUAL A 0 '
   ********                 A-NRO (1) ' ' A-APYN (1) ' ANTIG ' ANTIG1
   ********                 ' LETRA ' SI-LETRA-ANTIG
                       THEN COMPUTE JJB = I-LETRA - ANTIG1 + 1
                            MOVE 0 TO ANTIG1
                       ELSE MOVE ANTIG1-N TO ANTIG1
                            MOVE 1 TO JJB.
      **   IF NRO = '7064817240'
      ***    DISPLAY NRO ' ANTIG1  ' ANTIG1  ' JJB '
      ***                JJB.
       FIN-SEPARA-ANTIG. EXIT.
       BUSCA-IND-LETRA-1.
            IF SI-LETRA-ANTIG = LETRA1 (J)
               THEN MOVE ANIOS1 (J)  TO I-LETRA
                    MOVE  J   TO INDX    
                    MOVE  1   TO MARCA1    
                    MOVE 90 TO J.
       FIN-BUSCA-IND-LETRA-1. EXIT.
       BUSCA-IND-LETRA-2.
            IF SI-LETRA-ANTIG = LETRA2 (J)
               THEN MOVE ANIOS2 (J)  TO I-LETRA
                    MOVE  J   TO INDX    
                    MOVE  1   TO MARCA2    
                    MOVE 90 TO J.
       FIN-BUSCA-IND-LETRA-2. EXIT.
       RECALCULA-ANTIG.
           MOVE POS-2-4 TO I-MUN.
   *****   IF NRO = '7027285420' OR '8042596280'
   *****            THEN                                                                       
   *****   DISPLAY NRO ' I ' I-MUN ' ' PORC-MUN-X1 (I-MUN, I-LETRA)         
   ******        DISPLAY ' LETRA ' I-LETRA.                                       

           IF MARCA1 = 1 THEN
           IF PORC-MUN-X1 (I-MUN, I-LETRA) NUMERIC
              THEN PERFORM SUMA-PORCENTAJES-1 THRU F-SUMA-PORCENTAJES-1
                    VARYING K FROM JJB BY 1 UNTIL K > INDX          
              ELSE NEXT SENTENCE
                         ELSE
           IF PORC-MUN-X2 (I-MUN, I-LETRA) NUMERIC
              THEN PERFORM SUMA-PORCENTAJES-2 THRU F-SUMA-PORCENTAJES-2
                    VARYING K FROM JJB BY 1 UNTIL K > INDX.         
       FIN-RECALCULA-ANTIG. EXIT.
      *
       SUMA-PORCENTAJES-1.
           MOVE PORC-MUN-X1 (I-MUN, K) TO PORC-MUN-AUX.
   ******   agrego 19-05-09
           IF POS-1-4 = 'M066' AND K < 9  AND FECHA-MES > '200905'
                 THEN IF POS-5  = '9'                 
                          THEN NEXT SENTENCE
                          ELSE
                     COMPUTE ANTIG-AUX = ANTIG-AUX +
                               (H-ANTIG (2) * PORC / 100) 
                         GO TO F-SUMA-PORCENTAJES-1.
   ******   agrego 19-05-09
           IF POS-1-4 = 'M092'                 
                    THEN MOVE 2   TO PORC-MUN-AUX. 
   **************** THEN MOVE 1.5 TO PORC-MUN-AUX. MOV.MAY16: cambia al 2% toda la antig dde 1996, dde 2016, sin reajuste.
   ******   agrego 28-07-16
           IF POS-1-4 = 'M068'                 
                 THEN IF K = 21 AND FECHA-MES < '201608'       
                      THEN MOVE 1 TO PORC-MUN-AUX.
   ******   agrego 27-07-16
           IF POS-1-4 = 'M001'                 
                 THEN IF K = 20 AND FECHA-MES > '201507'       
                      THEN MOVE 3 TO PORC-MUN-AUX.
   ******   agrego 9-06-16
           IF POS-1-4 = 'M078'                 
                 THEN IF K = 21  AND FECHA-MES > '201606'       
                      THEN MOVE 2 TO PORC-MUN-AUX 
                      ELSE IF FECHA-MES > '201706'
                             THEN MOVE 3 TO PORC-MUN-AUX.
   ******   agrego 27-05-13
           IF POS-1-4 = 'M110'                 
                 THEN IF POS-5  = 'O' AND FECHA-MES > '201301'       
                      THEN MOVE 1.5 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M068' AND K = 1
                     THEN IF POS-5 NOT = '9'
                          THEN MOVE 3.0 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M041' AND K = 13
                     THEN IF POS-5 = '9'
                          THEN MOVE 2.5 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M010' AND K = 12
                          THEN MOVE 3.5 TO PORC-MUN-AUX.
   ********* agrego 1-6-15
           IF POS-1-4 = 'M056'                                     
                       THEN                                        
                       IF (FECHA-MES > '201504' AND 
                                    FECHA-MES < '201512')                  
                           THEN
                            MOVE 1.5 TO PORC-MUN-AUX 
                           ELSE                                        
                       IF  FECHA-MES > '201511'                 
                           THEN
                            MOVE 2.0 TO PORC-MUN-AUX.

           COMPUTE ANTIG-AUX = ANTIG-AUX +
                         ((SUMAFIJ3 * PORC-MUN-AUX / 100) * PORC / 100).
           IF POS-1-4 = 'M079' AND FECHA-MES > '201304'
                        THEN
                 COMPUTE ANTIANT = ANTIANT +
                         ((SUPL2 * PORC-MUN-AUX / 100) * PORC / 100).

       F-SUMA-PORCENTAJES-1. EXIT.
      *
       SUMA-PORCENTAJES-2.
           MOVE PORC-MUN-X2 (I-MUN, K) TO PORC-MUN-AUX.
   ******   agrego 19-05-09
           IF POS-1-4 = 'M066' AND K < 9  AND FECHA-MES > '200905'
                     THEN                            
                     COMPUTE ANTIG-AUX = ANTIG-AUX +
                               (H-ANTIG (2) * PORC / 100) 
                     GO TO F-SUMA-PORCENTAJES-2.
   ******   agrego 19-05-09
           IF POS-1-4 = 'M110'                 
                  THEN IF POS-5 = '9' OR 'O' OR '7'
                       THEN MOVE 1.0 TO PORC-MUN-AUX. 
           IF POS-1-4 = 'M110'                 
                  THEN IF POS-5 = 'O' AND FECHA-MES > '201301'                     
                        THEN MOVE 1.5 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M068' AND K = 1
                     THEN IF POS-5 NOT = '9'
                          THEN MOVE 3.0 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M041' AND K = 13
                     THEN IF POS-5 = '9'
                          THEN MOVE 2.5 TO PORC-MUN-AUX.
           IF POS-1-4 = 'M010' AND K = 12
                          THEN MOVE 3.5 TO PORC-MUN-AUX.
   ********* agrego 1-6-15
           IF POS-1-4 = 'M056'                                     
                       THEN                                        
                       IF (FECHA-MES > '201504' AND 
                                    FECHA-MES < '201512')                  
                           THEN
                            MOVE 1.5 TO PORC-MUN-AUX 
                           ELSE                                        
                       IF  FECHA-MES > '201511'                 
                           THEN
                            MOVE 2.0 TO PORC-MUN-AUX.
   ******   agrego 19-05-09
           IF POS-1-4 = 'M092'                 
                    THEN MOVE 2   TO PORC-MUN-AUX. 
   **************** THEN MOVE 1.5 TO PORC-MUN-AUX. MOV.MAY16: cambia al 2% toda la antig dde 1996, dde 2016, sin reajuste.
           COMPUTE ANTIG-AUX = ANTIG-AUX +
                         ((SUMAFIJ3 * PORC-MUN-AUX / 100) * PORC / 100).

           IF POS-1-4 = 'M079' AND FECHA-MES > '201304'
                        THEN
                 COMPUTE ANTIANT = ANTIANT +
                         ((SUPL2 * PORC-MUN-AUX / 100) * PORC / 100).

       F-SUMA-PORCENTAJES-2. EXIT.
       VER-M064.
      *     IF NRO = '2026908880' AND SEC = '4N'
      *               THEN MOVE 498.91  TO A-HABER (I) HABER
      *                    MOVE ZEROS   TO A-ANTIG (I) ANTIG.
       F-VER-M064.    EXIT.
      *
       VER-M071.
           IF (POS-5-8 = 'S052' OR 'S312' OR 'S463' OR 'RRRR')      
                              OR ( POS-11-12R > ZEROS)
                        THEN GO TO FIN-VER-M071.
            ADD 1 TO I.
            MOVE REG-MAE TO REGISTRO (I).
            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
            MOVE '02' TO A-CONC (I). 
            COMPUTE IND1176 = (INDX - 10) * 2.
            COMPUTE AUX1176 = (H-HABER * PORC / 100) * IND1176 / 100.
            MOVE AUX1176 TO A-HABER (I).
            MOVE '*BON DTO 1176/06'  TO A-CODIGO (I).
       FIN-VER-M071.    EXIT.
      *
       VER-M114.
           COMPUTE SUP-AUX = SUP-AUX  + A-ANTIG (1).
           IF POS-5-8 = 'S051'
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE SUP-AUX = SUP-AUX * 41 / 100
                          MOVE SUP-AUX TO A-HABER (I)
                          MOVE '*BON FUNC       '  TO A-CODIGO (I)
                     ELSE
           IF POS-5-8 = 'S302'
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE SUP-AUX = SUP-AUX * 13.75 / 100
                          MOVE SUP-AUX TO A-HABER (I)
                          MOVE '*BON FUNC       '  TO A-CODIGO (I)
                     ELSE
           IF POS-5-8 = 'S501'
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I)  
                          COMPUTE SUP-AUX = SUP-AUX * 27.50 / 100
                          MOVE SUP-AUX TO A-HABER (I)
                         MOVE '*BON FUNC       '  TO A-CODIGO (I).
       F-VER-M114.    EXIT.
       VER-M090.           
             MOVE ZEROS TO PORCM090.
             IF POS-18-19 = 'A0'                                      
                      THEN MOVE 100 TO PORCM090 
                      ELSE IF POS-18-19 IS NUMERIC
                            THEN
                            MOVE POS-18-19R TO PORCM090.

            IF POS-5  NOT = '9'                       
                   THEN GO TO NOES9.
 ************** CMH
             ADD 1 TO I.
             MOVE REG-MAE TO REGISTRO (I).
             MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).            
             COMPUTE A-HABER(I) = SUPL1 * PORC / 100.
             MOVE '*M090 FUNCION   '  TO A-CODIGO (I).
             MOVE '02' TO A-CONC (I).
 ************** si es > 201201  se generea registro adicional en el call
             IF FECHA-MES > '201101' AND FECHA-MES < '201203' 
                  THEN
                  IF POS-17 = 'T'                                      
                      THEN
                      ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
                      COMPUTE A-HABER(I) = H-HABER * PORC / 100
                      MOVE '*M090 TIEMP.COMP' TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I). 
             IF  PORCM090 > ZEROS                  
                   THEN
                   ADD 1 TO I
                   MOVE REG-MAE TO REGISTRO (I)
                   MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
                COMPUTE A-HABER(I) = H-HABER * PORCM090 *PORC / 100
                   MOVE '*M090 ZONA DESF.' TO A-CODIGO (I)
                   MOVE '02' TO A-CONC (I). 
             GO TO  F-VER-M090.

       NOES9.                 
             IF POS-5-8 = 'S252'                    
                    THEN
                    ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
                    COMPUTE A-HABER(I) = SUPL2 * PORC / 100
                    MOVE '*M090 GAST.REPRE' TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                     GO TO F-VER-M090
                    ELSE                                       
                 ADD 1 TO I
                 MOVE REG-MAE TO REGISTRO (I)
                 MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
                 COMPUTE A-HABER(I) = SUPL2 * PORC / 100
                 MOVE '*M090 COMPLEMENT' TO A-CODIGO (I)
                 MOVE '02' TO A-CONC (I).

           IF (POS-5 = '9' OR 'B') OR                    
              (POS-5-8  = 'S252' OR 'S050')                      
                   THEN GO TO F-VER-M090.                          
           IF POS-17 = '1' OR '2' OR '3' OR '4' OR 
                               '5' OR '6'                         
                            THEN
              PERFORM VERTITU-M090 THRU F-VERTITU-M090
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
              COMPUTE A-HABER(I) = (H-HABER *                         
                                 TITUM090 / 100 ) * PORC / 100
              MOVE '*M090 TITULO    ' TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I).
           IF PORCM090 > ZEROS                      
                            THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
              COMPUTE A-HABER(I) = (H-HABER *                          
                                 PORCM090 / 100) * PORC / 100
              MOVE '*M090 FUNCION   ' TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I).
       F-VER-M090. EXIT.

       VER-M063.      
           IF POS-17 = 'P' THEN NEXT SENTENCE                   
                           ELSE GO TO F-VER-M063.
           IF POS-5-8 = 'S103' OR 'S1S3' OR 'ST00' OR 'SU00'
              THEN IF FECHA-MES > '201601'
                      THEN NEXT SENTENCE
                      ELSE GO TO F-VER-M063.
           IF FECHA-MES > '201601'
              THEN MOVE 1236 TO TPM063
              ELSE IF FECHA-MES > '201507'
                      THEN MOVE 1030 TO TPM063
                      ELSE IF FECHA-MES > '201501'
                            THEN MOVE  936 TO TPM063
                            ELSE IF FECHA-MES > '201401'
                                  THEN MOVE  780 TO TPM063
                                  ELSE IF FECHA-MES > '201301'
                                        THEN MOVE  650 TO TPM063
                                        ELSE IF FECHA-MES > '201201'
                                              THEN MOVE 480 TO TPM063
                                              ELSE MOVE 400 TO TPM063.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I).                       
           MOVE '02' TO A-CONC (I).
           MOVE '*M063 TIEMPO PL.' TO A-CODIGO (I).
           COMPUTE A-HABER(I) = TPM063 * PORC / 100.                                    
           COMPUTE A-ANTIG (I) = A-ANTIG(1) / A-HABER(1) * A-HABER(I).
       F-VER-M063. EXIT.

       VER-M099.      
           IF POS-17 = '1' OR '2' OR '3' OR '4'            
              THEN                
           ADD 1 TO I 
           MOVE REG-MAE TO REGISTRO (I) 
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
           MOVE '02' TO A-CONC (I) 
           MOVE '*M099 TITULO    ' TO A-CODIGO (I) 
           IF POS-17 = '1'
            THEN COMPUTE A-HABER(I) = H-HABER * 5 / 100 * PORC / 100                                    
            ELSE
           IF POS-17 = '2'
            THEN COMPUTE A-HABER(I) = H-HABER * 10 / 100 * PORC / 100                                    
            ELSE
           IF POS-17 = '3'
            THEN COMPUTE A-HABER(I) = H-HABER * 15 / 100 * PORC / 100                                    
            ELSE COMPUTE A-HABER(I) = H-HABER * 20 / 100 * PORC / 100.
           IF  POS-18 = 'F' OR 'A' OR 'M'        
              THEN                
           ADD 1 TO I 
           MOVE REG-MAE TO REGISTRO (I) 
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
           MOVE '02' TO A-CONC (I) 
           IF POS-18 = 'F'                       
             THEN MOVE '*M099 FALLA C.  ' TO A-CODIGO (I) 
                 COMPUTE A-HABER(I) = H-HABER * 15 / 100 * PORC / 100                                    
             ELSE
           IF POS-18 = 'A'                       
             THEN MOVE '*M099 CONDUCTORT' TO A-CODIGO (I) 
                 COMPUTE A-HABER(I) = H-HABER * 8 / 100 * PORC / 100                                    
             ELSE
           IF POS-18 = 'M'                       
             THEN MOVE '*M099 CONDUCTORM' TO A-CODIGO (I) 
                 COMPUTE A-HABER(I) = H-HABER * 10 / 100 * PORC / 100.                                   
       F-VER-M099.    EXIT.

       VER-M087.      
           IF POS-17 = 'B' AND FECHA-MES > '200802'       
                     AND (POS-5 = 'S' OR 'N')
              THEN                
           ADD 1 TO I 
           MOVE REG-MAE TO REGISTRO (I) 
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)            
           MOVE '02' TO A-CONC (I) 
           MOVE '*M087 BLOQ.TITU.' TO A-CODIGO (I) 
           IF POS-5 = 'N'
            THEN COMPUTE A-HABER(I) = H-HABER * 25 / 100 * PORC / 100                                    
            ELSE
           IF FECHA-MES > '201001'
             THEN IF POS-5-8 = 'S051' OR 'S312'
            THEN COMPUTE A-HABER(I) = H-HABER * 50 / 100 * PORC / 100                                    
            ELSE COMPUTE A-HABER(I) = H-HABER * 25 / 100 * PORC / 100                                    
             ELSE COMPUTE A-HABER(I) = H-HABER * 25 / 100 * PORC / 100.                                   
       F-VER-M087.    EXIT.

       CALCULA-BONIF.
           MOVE ZEROS TO SUP-AUX.
           MOVE ZEROS TO TITU.   
 ********* agrego bonif CMH , ppto ENE2015, mov Nov15  10-11-15.                                    
           IF POS-1-4 = 'M110'  AND                                    
                   POS-5 = '9' AND FECHA-MES > '201501'
                  THEN
                  IF POS-7-8 = '28' OR '49'       
                     THEN 
                     IF POS-9-10N = 18 OR 24 OR 28 OR 30 OR 36
                        THEN                          
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
                          MOVE '*BM110 JEFSERV' TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE A-HABER(I) = H-HABER * PORC / 100
                        ELSE
                     IF POS-9-10N = 48                                       
                         THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
                          MOVE '*BM110 JEFSERV' TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE A-HABER(I) = (H-HABER / 48  * 36)                                
                                              * PORC / 100.


 ********* agrego bonif CMH para grado K seg antig, ppto ENE2015, mov abr15. Para cmh adheridas a pcia.
 ********* mov dic15 modif m0919 pq faltaba preg por k, mov ene16 idem para m012, los rea van dde fech de pptos aplicados.
 ********* mov set16 modif m0339 pq faltaba preg por k, los rea van dde fech de pptos aplicados.
           IF POS-5 = '9' AND FECHA-MES > '201501'
      *******THEN IF (POS-1-4 = 'M012' OR 'M035' OR 'M033') OR   
      ****** THEN IF (POS-1-4 = 'M035' OR 'M033') OR   
      ****** THEN IF POS-1-4 = 'M033' OR ((POS-1-4 = 'M091' OR  
             THEN IF (POS-1-4 = 'M033' OR 'M091' OR  
                     'M012' OR 'M035') AND POS-6 = 'K'      
                    THEN IF POS-13-14N < 25
                             THEN NEXT SENTENCE
                             ELSE
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
                          MOVE '*BMUN CMHK   '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          IF  POS-13-14N < 30     
                           THEN COMPUTE A-HABER(I) = H-HABER * 0.06 
                           ELSE COMPUTE A-HABER(I) = H-HABER * 0.11.
 ********* agrego bonif  Fcion, ppto ene13, ord.4533/13 art30, mov Feb15; los q tienen %% en pos 11-12 no lo cobran.
           IF POS-1-4 = 'M108'                            
              THEN IF (POS-5 = '1' OR POS-5-8 = 'S479')                            
                        AND FECHA-MES > '201301'
                    THEN IF POS-11-12 NUMERIC AND POS-11-12 > '00'
                         THEN GO TO FIN-CALCULA-BONIF 
                         ELSE                                 
                           ADD 1 TO I
                           MOVE REG-MAE TO REGISTRO (I)
                           MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
                           MOVE '*M108  FCION '  TO A-CODIGO (I)
                           MOVE '02' TO A-CONC (I) 
                           IF  POS-5-8 = '1001'
                            THEN COMPUTE A-HABER(I) = A-HABER(1) * 0.6 
                            ELSE
                             IF  POS-5-8 = '1002' OR 'S479'                
                                                  THEN
                               COMPUTE A-HABER(I) = A-HABER(1) * 0.55
                                                  ELSE
                             IF  POS-5-8 = '1003' THEN
                               COMPUTE A-HABER(I) = A-HABER(1) * 0.5
                                                  ELSE
                               COMPUTE A-HABER(I) = A-HABER(1) * 0.45 
                ELSE GO TO FIN-CALCULA-BONIF.
 ********* agrego bonif  Fcion, ppto jul09, dto.2880/09,mov Feb15.
           IF POS-1-4 = 'M124' AND FECHA-MES > '200907'
               AND POS-5-8 = 'S450'
                   THEN  IF POS-17-18 = 'AA' OR                      
                          (POS-17-18 IS NUMERIC AND                 
                           POS-17-18N > ZEROS)
                          THEN                   
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M124  FCION '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-17-18 = 'AA' THEN
              COMPUTE A-HABER(I) = A-HABER(1) 
              GO TO FIN-CALCULA-BONIF 
                                  ELSE
              COMPUTE A-HABER(I) = A-HABER(1) * POS-17-18N / 100
              GO TO FIN-CALCULA-BONIF.
 ********* genero sf para no jerarq, dde el anio 2011.  Para jorn y temp esta en hiscar.                   
           IF POS-1-4 = 'M019' AND (POS-5 = 'N' OR '9' OR '1' OR '2'       
                                   OR '3' OR '4' OR '5' OR '6')
             THEN IF FECHA-MES > '201101'                            
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '02' TO A-CONC (I) 
              MOVE '*M019 BONI      '  TO A-CODIGO (I)
              COMPUTE A-HABER(I) = 100 * PORC / 100.

 ********* genero sf para jerarq por el periodo agos11 a ener13, luego en gehi.Movim set13, reaj dde set11.     
           IF POS-1-4 = 'M115' AND POS-5 = 'S'           
             THEN IF  FECHA-MES > '201108' AND FECHA-MES < '201303'
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '02' TO A-CONC (I) 
              MOVE '*M115 BONI      '  TO A-CODIGO (I)
              COMPUTE A-HABER(I) = 500 * PORC / 100 
              GO TO ALAM014
                      ELSE GO TO ALAM014.

 ********* agrego 'bonif por tramo'DTO 59/11     M043, puede tener pos17='A' pero no se paga nada. 27-03-13.
 *********  mov.mayo16: actualizo valores pq son sobre la cat de ingreso, tomo el agrup administ pq son similares.
           IF POS-1-4 = 'M043' AND FECHA-MES > '201101'
              THEN IF POS-17 = 'B' OR 'C' OR 'D'
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '02' TO A-CONC (I) 
              IF FECHA-MES > '201607' THEN
                COMPUTE TRAMOB = 454.4
                COMPUTE TRAMOC = 454.4 * 2
                COMPUTE TRAMOD = 454.4 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201603' THEN
                COMPUTE TRAMOB = 416
                COMPUTE TRAMOC = 416 * 2
                COMPUTE TRAMOD = 416 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201509' THEN
                COMPUTE TRAMOB = 379
                COMPUTE TRAMOC = 379 * 2
                COMPUTE TRAMOD = 379 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201507' THEN
                COMPUTE TRAMOB = 352
                COMPUTE TRAMOC = 352 * 2
                COMPUTE TRAMOD = 352 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201503' THEN
                COMPUTE TRAMOB = 325
                COMPUTE TRAMOC = 325 * 2
                COMPUTE TRAMOD = 325 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201501' THEN
                COMPUTE TRAMOB = 295
                COMPUTE TRAMOC = 295 * 2
                COMPUTE TRAMOD = 295 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201409' THEN
                COMPUTE TRAMOB = 268
                COMPUTE TRAMOC = 268 * 2
                COMPUTE TRAMOD = 268 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201405' THEN
                COMPUTE TRAMOB = 247
                COMPUTE TRAMOC = 247 * 2
                COMPUTE TRAMOD = 247 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201303' THEN
                COMPUTE TRAMOB = 206
                COMPUTE TRAMOC = 206 * 2
                COMPUTE TRAMOD = 206 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201302' THEN
                COMPUTE TRAMOB = 196
                COMPUTE TRAMOC = 196 * 2
                COMPUTE TRAMOD = 196 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201301' THEN
                COMPUTE TRAMOB = 186
                COMPUTE TRAMOC = 186 * 2
                COMPUTE TRAMOD = 186 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201104' THEN
                COMPUTE TRAMOB = 137
                COMPUTE TRAMOC = 137 * 2
                COMPUTE TRAMOD = 137 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201103' THEN
                COMPUTE TRAMOB = 133
                COMPUTE TRAMOC = 133 * 2
                COMPUTE TRAMOD = 133 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
              IF FECHA-MES > '201102' THEN
                COMPUTE TRAMOB = 128  
                COMPUTE TRAMOC = 128 * 2
                COMPUTE TRAMOD = 128 * 3
                GO TO MUESTROM043 
                                      ELSE                                      
                COMPUTE TRAMOB = 122  
                COMPUTE TRAMOC = 122 * 2
                COMPUTE TRAMOD = 122 * 3 
                GO TO MUESTROM043 
                      ELSE                    
              GO TO FIN-CALCULA-BONIF 
                      ELSE                    
              GO TO ALAM014.   
       MUESTROM043.
              IF POS-17 = 'B'  THEN
              MOVE '*M043 BON TRAMOB'  TO A-CODIGO (I)
              COMPUTE A-HABER(I) = TRAMOB * PORC / 100
              COMPUTE A-ANTIG(I) = A-ANTIG (1) / A-HABER(1)
                                   * A-HABER (I)       
                               ELSE                    
              IF POS-17 = 'C'  THEN
              MOVE '*M043 BON TRAMOC'  TO A-CODIGO (I)
              COMPUTE A-HABER(I) = TRAMOC * PORC / 100
              COMPUTE A-ANTIG(I) = A-ANTIG (1) / A-HABER(1)
                                   * A-HABER (I)       
                               ELSE                    
              IF POS-17 = 'D'  THEN
              MOVE '*M043 BON TRAMOD'  TO A-CODIGO (I)
              COMPUTE A-HABER(I) = TRAMOD * PORC / 100
              COMPUTE A-ANTIG(I) = A-ANTIG (1) / A-HABER(1)
                                   * A-HABER (I)       
                               ELSE                    
              GO TO FIN-CALCULA-BONIF.
       ALAM014.                      
 ********* agrego bon insalubridad dde ppto enero 2009 - M014 mov julio13.
           IF POS-1-4 = 'M014' THEN                        
              IF FECHA-MES > '200901' AND POS-17 = 'I'
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M014 INSALUB.  '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              COMPUTE A-HABER(I) = H-HABER * 20 * PORC / 10000.
 ********* agrego bon titulos dde ppto enero 2011 - M014 mov mayo 14, cod validados en base.
           IF POS-1-4 = 'M014' THEN                        
              IF FECHA-MES > '201101' AND (POS-18 = 'U' OR 'T' OR 'S')
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M014 TITULO.   '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-18 = 'U' THEN
              COMPUTE A-HABER(I) = H-HABER * 15 * PORC / 10000
                              ELSE
              IF POS-18 = 'T' THEN
              COMPUTE A-HABER(I) = H-HABER * 10 * PORC / 10000
                              ELSE
              COMPUTE A-HABER(I) = H-HABER *  5 * PORC / 10000.
 ********* agrego titulo secund/terciario M051      5-02-13
           IF POS-1-4 = 'M051' AND FECHA-MES > '200001'
               AND POS-17 = 'T'
                      THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M051 TITULO    '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              COMPUTE A-HABER(I) = H-HABER * 15 * PORC / 10000
              GO TO FIN-CALCULA-BONIF.
 ********* agrego bonif por titulo, para agrupm 2 o 3 enfermerosR, mov oct15.
           IF POS-1-4 = 'M035' AND FECHA-MES > '201310'
               AND (POS-5 = '2' OR '3')
                      THEN
                    IF POS-19 = 'U' OR 'A' OR 'T' OR 'M'                        
                          THEN                   
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M035 TITULO    '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-19 = 'U' OR 'T' THEN
                COMPUTE A-HABER(I) = A-HABER(1) * 0.2
                                     ELSE
                COMPUTE A-HABER(I) = A-HABER(1) * 0.1.
 ********* agrego bonif Dedic Fcional, ppto mar14, dto.672/14, mov nov14.
           IF POS-1-4 = 'M035' AND FECHA-MES > '201403'
               AND POS-5 = 'S'
                      THEN
                    IF POS-17-18 = 'AA' OR                      
                          (POS-17-18 IS NUMERIC AND                 
                           POS-17-18N > ZEROS)
                          THEN                   
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M035 DED FCION '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-17-18 = 'AA' THEN
              COMPUTE A-HABER(I) = A-HABER(1) 
              GO TO FIN-CALCULA-BONIF 
                          ELSE
              COMPUTE A-HABER(I) = A-HABER(1) * POS-17-18N / 100
              GO TO FIN-CALCULA-BONIF.
 ********* agrego bonf funcion M018        17-07-14
           IF POS-1-4 = 'M018' AND FECHA-MES > '201001'
               AND POS-5 = 'S'
                      THEN
                    IF POS-17-18 = 'AA' OR                      
                          (POS-17-18 IS NUMERIC AND                 
                           POS-17-18N > ZEROS)
                          THEN                   
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M018 FUNCION   '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-17-18 = 'AA' THEN
              COMPUTE A-HABER(I) = A-HABER(1) 
              GO TO FIN-CALCULA-BONIF 
                                  ELSE
              COMPUTE A-HABER(I) = A-HABER(1) * POS-17-18N / 100
              GO TO FIN-CALCULA-BONIF.
 ********* agrego M082 -respons sist informatico- mov abr15
           IF POS-1-4 = 'M082' AND FECHA-MES > '201107'
                      THEN
                    IF POS-19 = 'B' THEN                    
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M082 RESP.INF. '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              COMPUTE A-HABER(I) = A-HABER(1).
 ********* agrego activ exclusiva M082     22-01-13
           IF POS-1-4 = 'M082' AND FECHA-MES > '200801'
               AND POS-5 = 'S'
                      THEN
                    IF POS-17-18 = 'AA' OR                      
                          (POS-17-18 IS NUMERIC AND                 
                           POS-17-18N > ZEROS)
                          THEN                   
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG(I)           
              MOVE '*M082 ACT.EXCL. '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF POS-17-18 = 'AA' THEN
              COMPUTE A-HABER(I) = A-HABER(1) 
              GO TO FIN-CALCULA-BONIF 
                                  ELSE
              COMPUTE A-HABER(I) = A-HABER(1) * POS-17-18N / 100
              GO TO FIN-CALCULA-BONIF.
 ********* agrego bloqueo de titulo             mov ago13             
           IF POS-1-4 = 'M087' AND FECHA-MES > '200802'
                      THEN
                    PERFORM VER-M087 THRU F-VER-M087                     
                    GO TO FIN-CALCULA-BONIF.            
 ********* agrego tiempo pleno para S      M063        11-02-16
           IF POS-1-4 = 'M063' AND FECHA-MES > '200801'
                               AND POS-5 = 'S'
                      THEN
                    PERFORM VER-M063 THRU F-VER-M063                     
                    GO TO FIN-CALCULA-BONIF.            
 ********* agrego titulos a la M099        14-01-13
 ********* agrego falla caja y conductores M099        25-03-13
           IF POS-1-4 = 'M099' AND FECHA-MES > '200703'
                      THEN
                    PERFORM VER-M099 THRU F-VER-M099                     
                    GO TO FIN-CALCULA-BONIF.            
 ********* agrego la M090        21-09-12
           IF POS-1-4 = 'M090' AND FECHA-MES > '201101'
                      THEN
                    PERFORM VER-M090 THRU F-VER-M090                     
                    GO TO FIN-CALCULA-BONIF.            
           IF POS-1-4 = 'M015'
             THEN IF FECHA-MES > '200802'                            
                   THEN IF POS-17 = 'F'                       
                          THEN NEXT SENTENCE
                          ELSE  GO TO FIN-CALCULA-BONIF             
                   ELSE  GO TO FIN-CALCULA-BONIF             
             ELSE  GO TO ALA-M058.                     
           IF POS-5-8 = '1479' THEN MOVE 45 TO FUN015
                               ELSE
               IF POS-5-8 = '1001' OR 'S103'                        
                                   THEN MOVE 40 TO FUN015
                                   ELSE
               IF POS-5-8 = '1002' OR 'S051'                
                                   THEN MOVE 30 TO FUN015
                                   ELSE
               IF POS-5-8 = 'S450' OR 'S472' OR 'S466'   
                                   THEN MOVE 25 TO FUN015
                                   ELSE MOVE 0 TO FUN015.
           IF FUN015 = 0  THEN GO TO FIN-CALCULA-BONIF 
                          ELSE
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I)            
              COMPUTE A-HABER(I) = A-HABER(1) * FUN015 / 100
              COMPUTE A-ANTIG(I) = A-ANTIG(1) * FUN015 / 100
              MOVE '*M015 FUNCION   '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              GO TO FIN-CALCULA-BONIF.
       ALA-M058.                     
     *************** en mov julio15 agr calculo con antig dde set14, dto 1262/14, vino en expte jubil.
           IF POS-1-4 = 'M058'
             THEN IF FECHA-MES > '200809'                            
                   THEN IF POS-17 = 'U' OR 'T' OR 'S' OR 'O'
                          THEN NEXT SENTENCE                        
                          ELSE GO TO FIN-CALCULA-BONIF              
                   ELSE GO TO FIN-CALCULA-BONIF              
             ELSE GO TO ALAM079.                       
           IF POS-17 = 'U' THEN MOVE 20 TO TITU
                           ELSE
                    IF POS-17 = 'T' THEN MOVE 15 TO TITU
                                    ELSE
                               IF POS-17 = 'S' THEN MOVE 10 TO TITU
                                               ELSE
                         IF POS-17 = 'O' THEN MOVE 5  TO TITU
                                         ELSE GO TO FIN-CALCULA-BONIF.             
            IF TITU > 0 THEN
              ADD 1 TO I
              MOVE REG-MAE TO REGISTRO (I)
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
              MOVE '*M058 TITULO    '  TO A-CODIGO (I)
              MOVE '02' TO A-CONC (I) 
              IF FECHA-MES > '201409' THEN
             COMPUTE A-HABER(I) = TITU * (A-HABER(1) + A-ANTIG(1)) / 100   
              GO TO FIN-CALCULA-BONIF 
                                      ELSE
              COMPUTE A-HABER (I) = TITU / 100 * H-HABER * PORC / 100
              GO TO FIN-CALCULA-BONIF.
       ALAM079.         
     ******** bonific por bloqueo de titulo para m117
           IF POS-1-4 = 'M117' AND FECHA-MES > '201109'
              THEN
                IF POS-17 = 'B'                         
                    THEN                                      
              COMPUTE PORANT-M117 = A-ANTIG (1) /                             
                                  ( A-HABER (1) + HABER-M117)                 
                      COMPUTE SUP-AUX = ((0.30 * H-HABER ) +   
                            (0.30 * (H-HABER * PORANT-M117))) *
                               PORC / 100
                      ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      MOVE SUP-AUX TO A-HABER (I)
                      MOVE '*M117 BLOQ TIT  '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I). 
     ******** bonific por Tiempo pleno      para m117
     ******IF POS-1-4 = 'M117' AND FECHA-MES > '201109' pidieron cambio de fecha en expte jub.
     ******IF POS-1-4 = 'M117' AND FECHA-MES > '200701'  """""
           IF POS-1-4 = 'M117' AND FECHA-MES > '200501'
                THEN
                  IF POS-18 = 'P' AND                     
                          (POS-5-8 = 'S450' OR 'S45F' OR 'S482'
                        OR 'S484' OR 'S48F' OR 'S0S1' OR 'S051'
                        OR 'S312' OR 'S103' OR 'S1S3' OR 'S105'
                        OR 'S1S5' OR 'S108' OR 'S1S8'                
                        OR 'S1N3' OR 'S1X3')               
                        THEN                                      
                 COMPUTE PORANT-M117 = A-ANTIG (1) /                             
                                  ( A-HABER (1) + HABER-M117)                 
                         COMPUTE SUP-AUX = ((0.35 * H-HABER ) +   
                              (0.35 * (H-HABER * PORANT-M117))) *
                               PORC / 100
                         ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE SUP-AUX TO A-HABER (I)
                         MOVE '*M117 TIEMPO P  '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I).
     ******** bonific por Mayor Antiguedad, antes en hiscar como BMA1 y BMA2 y 5004**%%=60%del porcde beneficio.
           IF POS-1-4 = 'M117' AND FECHA-MES > '201508'
                THEN
                  IF POS-19 NOT = 'M' THEN  GO TO FIN-CALCULA-BONIF
                                      ELSE
                    ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    MOVE SUP-AUX TO A-HABER (I)
                    MOVE '*M117 DTO 683/14'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                    IF FECHA-MES > '201603' THEN
     **************  COMPUTE H-500430 = 6678   
                     COMPUTE A-HABER (I) = 6678 * 0.6 * PORC / 100
                     GO TO FIN-CALCULA-BONIF 
                                            ELSE
                     COMPUTE A-HABER (I) = 5270.16 * 0.6 * PORC / 100
                     GO TO FIN-CALCULA-BONIF.
    ***ALAM079.         
           MOVE ZEROS TO HABERACT-M079 HABERANT-M079.
           IF POS-1-4 = 'M079'
             THEN IF FECHA-MES > '199810' AND FECHA-MES < '200404'
                   THEN IF POS-5-8 NOT = 'RRRR'
                         THEN IF SUPL2 > ZEROS
                          THEN ADD 1 TO I
                               MOVE REG-MAE TO REGISTRO (I)
                               MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                               COMPUTE SUP-AUX = SUPL2 * PORC / 100
                               MOVE SUP-AUX TO A-HABER (I)
                               MOVE '*BONI.M079      '  TO A-CODIGO (I)
                               MOVE '02' TO A-CONC (I) 
                               GO TO FIN-CALCULA-BONIF 
                          ELSE GO TO FIN-CALCULA-BONIF 
                         ELSE GO TO FIN-CALCULA-BONIF 
                   ELSE IF FECHA-MES = '201202' AND CONC = '01'
                          THEN IF POS-1 = '9' 
                                THEN GO TO FIN-CALCULA-BONIF 
                                ELSE
                               ADD 1 TO I
                               MOVE REG-MAE TO REGISTRO (I)
                               MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                               COMPUTE A-HABER (I) = 600 * PORC / 100
                               MOVE '*BONI.M079      '  TO A-CODIGO (I)
                               MOVE '02' TO A-CONC (I) 
                               GO TO FIN-CALCULA-BONIF 
                          ELSE                                         
                          IF FECHA-MES > '200903'                           
                          THEN IF POS-5-8 = '1002' OR '1003' 
                                         OR '10F2' OR '10F3' 
                                         OR 'S303' OR 'S304' 
      ****************************************** 1002 y 1003 se agr en mov 0ct14, reaj dde enero 14.
      ****************************************** S303 y S304 se agr en mov nov14, reaj dde enero 14.
                             THEN                              
                               ADD 1 TO I
                               MOVE REG-MAE TO REGISTRO (I)
                               MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                               COMPUTE A-HABER (I) = (H-HABER * 0.185)                      
                                                          * PORC / 100
                               COMPUTE HABERANT-M079 = (SUPL2 * 0.185)                      
                                                          * PORC / 100
                               MOVE A-HABER (I) TO HABERACT-M079 
                               MOVE '*DTO 389/09     '  TO A-CODIGO (I)
                               MOVE '02' TO A-CONC (I)  
                               GO TO COMPA79              
                             ELSE GO TO COMPA79                      
                          ELSE GO TO FIN-CALCULA-BONIF.
           IF POS-1-4 = 'M083' OR 'W083'                              
              THEN IF FECHA-MES > '200701' AND FECHA-MES < '200804'  
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I)                      
                          MOVE '02'    TO A-CONC (I)                      
                          MOVE '*DTO.51/2007'    TO A-CODIGO (I)                      
                          IF FECHA-MES > '200707' THEN                      
                          COMPUTE A-HABER (I) = A-HABER (1) * 0.17488
                          COMPUTE A-ANTIG (I) = A-ANTIG (1) * 0.17488
                                                  ELSE                                                  
                          COMPUTE A-HABER (I) = A-HABER (1) * 0.049
                          COMPUTE A-ANTIG (I) = A-ANTIG (1) * 0.049.
 
           IF POS-1-4 = 'M083' AND CONC = '01'                            
                  THEN
           IF  FECHA-MES = '200712' OR '200801' OR '200802'
                  THEN
              IF POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6'        
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02'    TO A-CONC (I)                      
                          MOVE '*DTO1491/07'    TO A-CODIGO (I)                      
                          COMPUTE A-HABER (I) = 200 * PORC / 100 
                           GO TO FIN-CALCULA-BONIF 
                     ELSE  GO TO FIN-CALCULA-BONIF 
                  ELSE                             
           IF  FECHA-MES = '201102' OR '201202'               
                        THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02'    TO A-CONC (I)                      
                          MOVE '*D2600/10-834/11'    TO A-CODIGO (I)                      
                          COMPUTE A-HABER (I) = 500 * PORC / 100.
     *****                GO TO FIN-CALCULA-BONIF 
     *****              ELSE  GO TO FIN-CALCULA-BONIF.

     ********* cargos de enfermeros con bonificaciones
           IF POS-1-4 = 'M083' AND POS-5 = 'T'                            
                  THEN
                    IF POS-7 = 'R' OR 'T' OR 'V'
                     THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '02'    TO A-CONC (I)                      
                         MOVE '*M083 ENFERM.   '    TO A-CODIGO (I)                      
                         IF POS-7 = 'R' THEN          
                   COMPUTE A-HABER (I) = H-HABER * 0.2 * PORC / 100  
                                        ELSE
                         IF POS-7 = 'T' OR 'V' THEN          
                   COMPUTE A-HABER (I) = A-HABER(1) * 0.2 * PORC / 100  
                                               ELSE
                                GO TO FIN-CALCULA-BONIF 
                      ELSE  GO TO FIN-CALCULA-BONIF.

           IF POS-1-4 = 'M049' AND POS-5 = 'O'
             THEN NEXT SENTENCE
             ELSE GO TO ALA59.               
           IF FECHA-MES = '201006'   
                       THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                  COMPUTE A-HABER (I) = H-HABER * 0.25 * (PORC / 100)
                            MOVE '*RES1.247/10     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                       ELSE
           IF (FECHA-MES = '201007' OR '201008') AND SUMAFIJ2 > 0
                       THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100
                            MOVE '*RES.247/10     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I). 
           IF (FECHA-MES = '201008' OR '201009' OR '201010' OR '201011')                        
                       THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                   COMPUTE A-HABER (I) = H-HABER * 0.125 * (PORC / 100)
                            MOVE '*RES1.247/10     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I). 
     ******IF (FECHA-MES = '201108' OR '201110')      la sf de set11 la paso a un gre.                  
     ***** IF FECHA-MES = '201108'. por expte 21557-213695-12 autorz los % no rem de mar11, 28%, y de jul11 el 14%, movoct13.       
           IF FECHA-MES > '201103' AND FECHA-MES < '201110'            
                       THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                   COMPUTE A-HABER (I) = H-HABER * 0.28  * (PORC / 100)
                            MOVE '*RES.3/11        '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I). 
           IF POS-6-7 = 'PN' OR 'PO' OR 'PP'
             THEN IF FECHA-MES > '200512'                   
                    THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = A-HABER (1) * 0.17   
                            MOVE '*BONI.M049O JORN'  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I) 
                            GO TO FIN-CALCULA-BONIF 
                    ELSE IF FECHA-MES > '200501' AND SUMAFIJ2 > 0
                          THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100
                            MOVE '*BONI.M049O JORN'  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I) 
                            GO TO FIN-CALCULA-BONIF.

       ALA59.               
           IF POS-1-4 = 'M059' AND POS-5 = 'O' AND FECHA-MES > '200601'
               THEN IF POS-19  = 'P'                 
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = SUPL2 * PORC / 100
                            MOVE '*BONI.M059O PELI'  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                     ELSE NEXT SENTENCE
               ELSE

           IF POS-1-4 = 'M092'                                          
              THEN
                IF  (POS-5 = '1' OR '2' OR '3' OR '4'  
                 OR '9' OR '5' OR '6')  AND FECHA-MES > '200807'
                                       AND FECHA-MES < '201005'
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 200 * PORC / 100
                            MOVE '*M092 JUL08     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                     ELSE NEXT SENTENCE
              ELSE

******horas fcionales para cargos de vte lopez.En mov jul12 agr K y cambio la codif a la pos 17.
     ******IF POS-1-4 = 'M119' AND FECHA-MES > '201107' AND  cambio vigencia en enero 2014.
           IF POS-1-4 = 'M119' AND FECHA-MES > '201101' AND                          
                   ((POS-17 = 'Q' OR 'K') OR POS-5-8 = '30Q1')
  ***********        THEN ADD 1 TO I
  ************            MOVE REG-MAE TO REGISTRO (I)
  ************            MOVE ZEROS   TO A-IOMA (I) HSFUNQ HSFUNK
 *************            MOVE '*M119 HS.FCIONS.'  TO A-CODIGO (I)
 **************           MOVE '02' TO A-CONC (I)
                     THEN IF (POS-7 = 'Q' OR POS-17 = 'Q') THEN
                          COMPUTE HSFUNQ  = (H-HABER + SUMAFIJ2
                                               + SUPL2)
                                               * PORC / 100 / 30 * 15
   *****************      COMPUTE A-ANTIG (I) = A-ANTIG (1) / 30 * 15
                          COMPUTE HSFUNQ = HSFUNQ  +                    
                                             (A-ANTIG (1) / 30 * 15)
                                                           ELSE
                          COMPUTE HSFUNK  = (H-HABER + SUMAFIJ2 
                                               + SUPL2)
                                               * PORC / 100 / 30 * 18
    ***************       COMPUTE A-ANTIG (I) = A-ANTIG (1) / 30 * 18 
                          COMPUTE HSFUNK = HSFUNK  +                 
                                            (A-ANTIG (1) / 30 * 18).

           IF POS-1-4 = 'M119' AND POS-5 = 'S' AND FECHA-MES > '200801'
                THEN
                     IF POS-20 = 'D'                                       
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I)
                          MOVE '*M119 DISPON.D. '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)
                          COMPUTE A-HABER (I) = (H-HABER * PORC / 100)
                                               * 0.30                        
                     ELSE
                     IF POS-20 = 'F'                                       
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I)
                          MOVE '*M119 DISPON.F. '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)
                          COMPUTE A-HABER (I) = (H-HABER * PORC / 100)
                                               * 0.70.                       
           IF POS-1-4 = 'M119' AND POS-5 = 'S' AND FECHA-MES > '200801'
                THEN
                 IF POS-18-19 IS NUMERIC                                 
                 THEN
                     IF POS-18-19R > '00' AND POS-18-19R < '61'              
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I)
                          MOVE '*M119 TIEMPO P. '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)
                          COMPUTE A-HABER (I) = (H-HABER * PORC / 100)
                                               * POS-18-19R / 100     
                          GO TO FIN-CALCULA-BONIF            
                      ELSE  GO TO FIN-CALCULA-BONIF            
                  ELSE  GO TO FIN-CALCULA-BONIF.           
          

    **** bonificacion por conduccion de vehiculos. mov abr12. Bon.Tarea riesgosa en pos 17=R mov mar13.
           IF POS-1-4 = 'M037' AND FECHA-MES > '201001'                                              
                     THEN
             IF POS-5 = '6' AND (POS-6 = 'C' OR POS-7 = 'C')          
                            AND POS-17 NOT = 'R'
    ******                POS-7 = 'C') AND FECHA-MES > '201111' cambio fecha por pedido
                            THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
                          COMPUTE A-HABER (I) = H-HABER * 0.15          
                                               * PORC / 100               
                          MOVE '02' TO A-CONC (I)  
                          MOVE '*M037 CHOFER    '  TO A-CODIGO (I)
                            ELSE 
             IF POS-17 = 'R'                                            
                        THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
                          COMPUTE A-HABER (I) = H-HABER * 0.15          
                                               * PORC / 100               
                          MOVE '02' TO A-CONC (I)  
                          MOVE '*M037 TRIESGO   '  TO A-CODIGO (I)
                        ELSE NEXT SENTENCE.
    **** bonificacion M031 por FUNCION                . mov Jun12.
           IF POS-1-4 = 'M031' AND FECHA-MES > '201203'     
                AND (POS-5 = 'S' OR '1' OR '2' OR '3')         
                  THEN IF POS-19 = 'F'
                          THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
                          COMPUTE A-HABER (I) = H-HABER * 0.50                       
                                               * PORC / 100               
                          MOVE '*M031 FUNCION   '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I).  
           IF POS-1-4 = 'M031' AND FECHA-MES > '201203'     
                AND (POS-5 = 'S' OR '1' OR '2' OR '3')         
              THEN
                IF POS-17-18 IS NUMERIC AND POS-17-18N > ZEROS
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
                          COMPUTE A-HABER (I) = A-HABER (1)                      
                                               * (POS-17-18N / 100)                              
                          MOVE '*M031 TITULO    '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)    
                          GO TO FIN-CALCULA-BONIF.           

    **** bonificacion M004 Extension extra-laboral    . mov jul2012
           IF POS-1-4 = 'M004' AND FECHA-MES > '200801'     
************  AND ((POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6'          
 ***********                            OR '7' OR '8' OR '9') OR               
************      ( POS-5-8 = 'S153' OR 'S154' OR 'S303 OR 'S353' 
 **********         OR 'S354' OR 'S304' OR 'S309' OR 'S310' OR  
 *********          OR 'S479' OR 'S480'))                              
               THEN
                IF POS-17-18 IS NUMERIC AND POS-17-18N > ZEROS
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
   *************      COMPUTE A-HABER (I) = A-HABER (1) + A-ANTIG (1)  cambio en mov oct14.   
   ******************              / 132 * (POS-17-18N / 100)                              
                      COMPUTE A-HABER (I) = ((A-HABER (1) + A-ANTIG (1))                            
                                   / 132 * POS-17-18N) +                             
                                   (SUPL2 + SUMAFIJ2) * PORC / 100
                                   / 132 * POS-17-18N
                          MOVE '*M004 EXTRA-LAB.'  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I).   
    **** bonificacion M004 para informatica, administrad o analista, solo el caso del 200%
           IF POS-1-4 = 'M004' AND FECHA-MES > '201001'     
               THEN
                IF POS-19 = 'A'                                 
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)  
                          COMPUTE A-HABER (I) = A-HABER (1) * 2                                       
                          MOVE '*M004 INFORM AN.'  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I).   
                                                  
           IF POS-1-4 = 'M060' AND (POS-5 = 'B' OR 'E' OR 'H') 
               THEN
                  IF (CONC = '01' OR (CONC = '02' AND PORC > '00'))
                   AND (FECHA-MES > '200901' AND FECHA-MES < '201208')
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100
                            MOVE '*M060CMED.SM    '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                            GO TO FIN-CALCULA-BONIF.           

           IF POS-1-4 = 'M016' AND POS-5 = '1' AND FECHA-MES > '200801'
               THEN
                  IF POS-17-18 NUMERIC AND POS-17-18N > ZEROS         
                   THEN ADD 1 TO I
                        MOVE REG-MAE TO REGISTRO (I)
                        MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        COMPUTE A-HABER (I) = H-HABER * POS-17-18N
                                              * PORC / 10000
                        MOVE '*M016 T PLENO   '  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                        GO TO FIN-CALCULA-BONIF            
                   ELSE GO TO FIN-CALCULA-BONIF.           

           IF POS-1-4 = 'M005' AND FECHA-MES > '201307'
                   THEN PERFORM BONI-M005 THRU F-BONI-M005
                        GO TO FIN-CALCULA-BONIF.           

************* agrego 05-11-14
           IF POS-1-4 = 'M106' AND FECHA-MES > '200801'
                  THEN
                  IF (POS-5 = 'S') AND POS-18-19 NUMERIC                                    
                                   AND POS-18-19 > '0'                   
                   THEN PERFORM BONI-M106-1 THRU F-BONI-M106-1
                        GO TO FIN-CALCULA-BONIF.            
           IF POS-1-4 = 'M106' AND FECHA-MES > '200911'
                 THEN
            IF (POS-5 = '3' OR '6') AND POS-17 NUMERIC AND POS-17 > '0'                   
                   THEN PERFORM BONI-M106 THRU F-BONI-M106
                        GO TO FIN-CALCULA-BONIF            
                   ELSE GO TO FIN-CALCULA-BONIF.           

     ********* POS-1-4 = 'M050' AND FECHA-MES > '201101' mov.junio14 cambio vig de cod 1-2-3-4 en baseeeeee.
            IF POS-1-4 = 'M050' AND FECHA-MES > '200801'
                 THEN
 ******************* Bonificacion por titulo 
            IF (POS-17 = '1' OR '2' OR '3' OR '4' OR '5' OR '6')                                     
                   THEN PERFORM BONI-M050 THRU F-BONI-M050
                        GO TO FIN-CALCULA-BONIF
                   ELSE GO TO FIN-CALCULA-BONIF.

            IF POS-1-4 = 'M065' AND FECHA-MES > '200509'
                 THEN
 ******************* Bonificacion por titulo 
            IF (POS-17 = '1' OR '2' OR '3' OR '4' OR '5')                                     
                   THEN PERFORM BONI-M065 THRU F-BONI-M065.

 ******************* Bonificacion por titulo Y funcion 09-05-14
     ****** IF POS-1-4 = 'M135' AND FECHA-MES > '201101' pidieron cambio de vig,mov ago14
            IF POS-1-4 = 'M135' AND FECHA-MES > '200601'
              THEN IF (POS-5 = 'B' OR '9' OR 'T') OR                                     
                      (POS-5-8 = 'ACEN')                                    
                   THEN NEXT SENTENCE
                   ELSE MOVE ZEROS TO TITU135                   
                        PERFORM BONI-M135 THRU F-BONI-M135.
            IF POS-1-4 = 'M135' AND FECHA-MES > '200807'
               THEN IF POS-5 = '9'                                            
                    THEN ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      COMPUTE A-HABER (I) = H-HABER * PORC / 100                     
                                    * 15 / 100                      
                      MOVE '*BON.FUNC.M1359  '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I).
            IF POS-1-4 = 'M135' AND FECHA-MES > '201101'
               THEN IF POS-5 = 'S'                                            
                 THEN                                                      
                 IF (POS-18-19 IS NUMERIC AND POS-18-19R > ZEROS)         
                    THEN ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      COMPUTE A-HABER (I) = ((H-HABER * PORC / 100)                     
                                    + TITU135 ) * POS-18-19R / 100 
                      MOVE '*BON.FUNC.M135   '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I) 
                     ELSE                                                      
                    IF POS-18-19  =  'AA'                                       
                      THEN ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      COMPUTE A-HABER (I) = (H-HABER * PORC / 100)                     
                                            + TITU135 
                      MOVE '*BON.FUNC.M135   '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I).

            IF MAR-CMH = '*'  
                 THEN ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      COMPUTE A-HABER (I) = SUPL1 * PORC / 100                    
                      MOVE '*FUNCION CMH    '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I)
 *************
                      ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100                    
                      MOVE '*SUM.FIJA CMH   '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I).
****************      GO TO FIN-CALCULA-BONIF.

           IF POS-1-4 = 'M003' AND FECHA-MES > '200804'
               THEN
                    IF  POS-5 = 'B' OR '9' OR 'C'                                        
                           THEN NEXT SENTENCE
                           ELSE
                  IF POS-17-18 NUMERIC AND POS-17-18N > ZEROS         
                   THEN ADD 1 TO I
                        MOVE REG-MAE TO REGISTRO (I)
                        MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        COMPUTE A-HABER (I) = H-HABER * POS-17-18N
                                              * PORC / 10000
                        MOVE '*BON.M003 FCION '  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                        GO TO FIN-CALCULA-BONIF            
                   ELSE GO TO FIN-CALCULA-BONIF.           
********** ****************   agrego 28-5-14
           IF POS-1-4 = 'M116' AND FECHA-MES > '200206'
                THEN
                  IF (POS-5 = 'N' OR 'M' OR '9') AND                                         
                        POS-17 = 'I' AND POS-9-10N < 40   
                  THEN  ADD 1 TO I
                       MOVE REG-MAE TO REGISTRO (I)
                       MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                       COMPUTE IMP-M116-40 = ((H-HABER / POS-9-10N * 40)
                                      - H-HABER) * PORC / 100
                        MOVE '*BON.M116 INSAL.'  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                        MOVE IMP-M116-40 TO A-HABER (I) 
                        GO TO FIN-CALCULA-BONIF            
                   ELSE GO TO FIN-CALCULA-BONIF.           

********** ****************   agrego 22-7-14
           MOVE ZEROS TO IMP-M009-FU.
           IF POS-1-4 = 'M009' AND                      
              POS-11-12 NUMERIC AND POS-11-12 > '00'
                 THEN GO TO FIN-CALCULA-BONIF.            
           IF POS-1-4 = 'M009' AND FECHA-MES > '201512'
             THEN IF POS-5-8 = '1N10'                                           
                 THEN  COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.15                           
                 ELSE IF POS-5-8 = '1N11'                                           
                       THEN
                       COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.18                           
                       ELSE IF POS-5-8 = '1N12'                                           
                              THEN
                       COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.2                            
                              ELSE                                             
           IF POS-1-4 = 'M009' AND FECHA-MES > '201404'
             THEN IF POS-5-8 = '1N10'                                           
                 THEN  COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.10                           
                 ELSE IF POS-5-8 = '1N11'                                           
                       THEN
                       COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.12                           
                       ELSE IF POS-5-8 = '1N12'                                           
                              THEN
                       COMPUTE IMP-M009-FU = (A-HABER (I) + A-ANTIG (I))
                                               * 0.15                           
                              ELSE NEXT SENTENCE.                              
           IF IMP-M009-FU > ZEROS 
                       THEN
                       ADD 1 TO I 
                       MOVE REG-MAE TO REGISTRO (I)
                       MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        MOVE '*BON.M009 FUNC. '  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                        MOVE IMP-M009-FU TO A-HABER (I) 
                        GO TO FIN-CALCULA-BONIF.            
       COMPA79.                     
  ******* AGREGO 28-9-16
            IF POS-1-4 = 'M079' AND FECHA-MES > '201601'
                        AND POS-5 = '9' AND POS-6 NOT = 'G'
                   THEN PERFORM BONI-11197  THRU FIN-BONI-11197. 

            IF POS-1-4 = 'M079' AND FECHA-MES > '201304'
                                AND SUPL2 > ZEROS 
                   THEN PERFORM BONI-15118  THRU FIN-BONI-15118 
                        GO TO FIN-CALCULA-BONIF.            
            IF POS-1-4 = 'M070' THEN PERFORM BONIS-M070 THRU             
                                     BONIS-M070-X.
       FIN-CALCULA-BONIF. EXIT.

       BONI-11197.                 
**************  CMD de PLANTA
              ADD 1 TO I. 
              MOVE REG-MAE TO REGISTRO (I).
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
              MOVE '*ORDE.11197-PTA '  TO A-CODIGO (I).
              MOVE '02' TO A-CONC (I). 
              COMPUTE A-HABER (I) = A-HABER (1) * 0.40.              
            IF POS-17 = 'E'       
**************  CMD ESPECIAL   
                      THEN
                      ADD 1 TO I 
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      MOVE '*ORDE.11197-ESP '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I)  
                      COMPUTE A-HABER (I) = A-HABER (1) * 0.10.             
       FIN-BONI-11197.       EXIT.   

       BONIS-M070.                 
            IF POS-5 = 'B' OR '9' OR 'G'
               THEN GO TO BONIS-M070-X       
               ELSE IF FECHA-MES < '200702'
                     THEN GO TO BONIS-M070-X          
                     ELSE
            IF POS-17 = '1' THEN
                      ADD 1 TO I 
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      MOVE '*BON.M070 TITU. '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I)  
                      COMPUTE A-HABER (I) = (A-HABER (1) + A-ANTIG (1))
                                             * 0.2.
            IF POS-5 = 'S' AND POS-18 = 'P' THEN
                      ADD 1 TO I 
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      MOVE '*BON.M070 FC PRO'  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I)  
                      COMPUTE A-HABER (I) = (A-HABER (1) + A-ANTIG (1))
                                             * 0.5.
       BONIS-M070-X.       EXIT.   

       BONI-15118.                 
 ********    DISPLAY REGISTRO (1).
***************  calcula antiguedad del supl2
             IF POS-5-8 =  'S050' THEN GO TO FIN-BONI-15118.
             IF ((POS-5-8 = 'S252' OR 'S316'
                      OR 'S317' OR 'S454' OR 'S463' OR 'S004' OR 'S007'
                      OR 'S499' OR 'S612' OR 'S504' OR 'S122' OR 'SCOU'
                      OR 'ACRI' OR '311A' OR '311B' OR '311C') OR
                           (POS-5 = 'T' OR '9' OR 'C' OR 'F'))
                      THEN MOVE ZEROS TO DTO423
                      ELSE IF FECHA-MES > '201603'
                            THEN COMPUTE DTO423 = 0                   
                            ELSE COMPUTE DTO423 = 200 * PORC / 100.
             IF POS-5 = 'T'                               
                    THEN MOVE ZEROS TO ANTIANT
                    ELSE COMPUTE ANTIANT = (ANTIANT + (SUPL2 * 0.03) 
                                    * (ANTIG1 - 1)) * PORC / 100.
***************  calcula antiguedad del supl2
*************** parche 13-11-15 para los tres cargos que no aumentaron sueldo
             IF POS-5-8 = 'C770' OR 'S122' OR 'S348'
                   THEN
                   COMPUTE DOSANT = SUPL2 * 0.02 
                   COMPUTE DOSACT = A-HABER (1) * 0.02 
                   ELSE
             IF FECHA-MES > '201508'
                      THEN
                   COMPUTE DOSANT = SUPL2 * 0.10  
                   COMPUTE DOSACT = A-HABER (1) * 0.10  
                      ELSE
             IF FECHA-MES > '201407'
                          THEN
                   COMPUTE DOSANT = SUPL2 * 0.07  
                   COMPUTE DOSACT = A-HABER (1) * 0.10  
                          ELSE
             IF FECHA-MES > '201307'
                             THEN
                   COMPUTE DOSANT = SUPL2 * 0.04  
                   COMPUTE DOSACT = A-HABER (1) * 0.07  
                             ELSE
             IF FECHA-MES > '201304'
                                   THEN
                   COMPUTE DOSANT = SUPL2 * 0.04  
                   COMPUTE DOSACT = A-HABER (1) * 0.04  
                                   ELSE
                   COMPUTE DOSANT = SUPL2 * 0.02 
                   COMPUTE DOSACT = A-HABER (1) * 0.02.
             COMPUTE  IMPOANT = ((SUPL2 + DOSANT) * PORC / 100)                       
 *************            + ((SUMAFIJ2 - DOSACT) * PORC / 100)                       
                               + ((SUMAFIJ2  * PORC / 100) - DOSACT)                       
                               + DTO423                                   
                               + ANTIANT + HABERANT-M079 .         
   ********* DISPLAY 'ante ' IMPOANT  '  antiant ' ANTIANT           
  **********          ' anos ' ANTIG1 'SUPL2  ' SUPL2.
             COMPUTE  IMPOACT = A-HABER (1) + A-ANTIG (1)                                         
                               + DTO423                                   
                               + (SUMAFIJ2 * PORC / 100)                       
                               + HABERACT-M079.
      ******                         + DOSACT  + HABERACT-M079.
     ******* DISPLAY 'aCtu ' IMPOACT  '  antiact ' A-ANTIG (1).
             COMPUTE AUM-M079 = IMPOACT - IMPOANT.
             IF FECHA-MES > '201308'
                     THEN
             COMPUTE DIF13-M079 = IMPOANT * 0.107
                     ELSE
             COMPUTE DIF13-M079 = IMPOANT * 0.13. 
             IF AUM-M079 > DIF13-M079
                     THEN GO TO FIN-BONI-15118.
             ADD 1 TO I
             MOVE REG-MAE TO REGISTRO (I)
             MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
             COMPUTE A-HABER (I) = DIF13-M079 - AUM-M079.               
             MOVE '*BON.M079 15118 '  TO A-CODIGO (I)
             MOVE '02' TO A-CONC (I).
       FIN-BONI-15118. EXIT.                 
       BONI-M005.                  
            IF  POS-17 NUMERIC AND POS-17 > '0'                   
             THEN IF POS-5  = 'S'                                                      
               THEN GO TO F-BONI-M005                                                    
               ELSE IF POS-17 = '1' OR '2' OR '3'                                        
                   THEN 
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I) 
              MOVE '02' TO A-CONC (I)  
              IF POS-17 = '1'                                                      
                   THEN 
                    COMPUTE A-HABER(I) = A-HABER(1) * 0.04                                 
                    MOVE '*M005 TITS' TO A-CODIGO (I) 
                   ELSE IF POS-17 = '2'                                                      
                     THEN 
                    COMPUTE A-HABER(I) = A-HABER(1) * 0.06                                 
                    MOVE '*M005 TITT' TO A-CODIGO (I) 
                     ELSE IF POS-17 = '3'                                                      
                       THEN 
                    COMPUTE A-HABER(I) = A-HABER(1) * 0.08                                 
                    MOVE '*M005 TITU' TO A-CODIGO (I).
            IF  POS-18 = 'C' OR 'P' OR 'A'                        
              THEN
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I) 
              MOVE '02' TO A-CONC (I)  
              IF POS-18 = 'C'
                  THEN 
                COMPUTE A-HABER (I) = (A-HABER(1) + A-ANTIG (1))                       
                                                 * 0.5                                 
                MOVE '*M005 ACTC' TO A-CODIGO (I) 
                  ELSE 
              IF POS-18 = 'P'
                  THEN 
                COMPUTE A-HABER (I) = A-HABER(1) * 0.04                               
                MOVE '*M005 PERM' TO A-CODIGO (I) 
                  ELSE 
                COMPUTE A-HABER (I) = (A-HABER(1) + A-ANTIG (1))                       
                                            * 0.56                                 
                MOVE '*M005 AC-PER' TO A-CODIGO (I).
            IF  POS-19 = '1' OR '2' OR '3'                        
              THEN
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I) 
              MOVE '02' TO A-CONC (I)  
              IF POS-19 = '1'
                  THEN 
                COMPUTE A-HABER (I) = A-HABER(1) * 0.04                               
                MOVE '*M005 VE-A' TO A-CODIGO (I) 
                  ELSE 
              IF POS-19 = '2'
                  THEN 
                COMPUTE A-HABER (I) = A-HABER(1) * 0.06                               
                MOVE '*M005 VE-T' TO A-CODIGO (I) 
                  ELSE 
                COMPUTE A-HABER (I) = A-HABER(1) * 0.08                               
                MOVE '*M005 VE-M' TO A-CODIGO (I).
     ******** bon jornada prolon, en hisc 40h cobra como 30, aca esta la bonif.
            IF FECHA-MES > '201310' AND POS-9-10 = '40'
                THEN
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I) 
              MOVE '02' TO A-CONC (I)  
              COMPUTE A-HABER (I) = (A-HABER(1) + A-ANTIG (1)                        
                      + (SUPL2 * PORC / 100)) * 0.3333
              MOVE '*M005 JOR.PR.' TO A-CODIGO (I). 
       F-BONI-M005.     EXIT.      

       BONI-M106.                  
             IF POS-17 = '9' AND POS-5 = '6'                                      
                     THEN
                     COMPUTE BONIM106  = ((H-HABER * PORC / 100) +                                 
                                     A-ANTIG (1)) * 0.15              
                     MOVE '*M106 CUADRILLA B.'  TO LEYENM106   
                     ELSE
             IF POS-17 = '8' AND POS-5 = '3'                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                                 
                                      A-ANTIG (1)) * 0.30              
                     MOVE '*M106 INCOMP.PRF'  TO LEYENM106   
                     ELSE
             IF POS-17 = '7' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                                 
                                          A-ANTIG (1)) * 0.48              
                     MOVE '*M106 ALUMB.C3HR'  TO LEYENM106   
                     ELSE
             IF POS-17 = '6' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                                 
                                            A-ANTIG (1)) * 0.38              
                     MOVE '*M106 ALUMB.C2HR'  TO LEYENM106   
                     ELSE
             IF POS-17 = '5' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                     
                                        A-ANTIG (1)) * 0.28         
                     MOVE '*M106 ALUMB.C1HR'  TO LEYENM106   
                     ELSE
             IF POS-17 = '4' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                                 
                                            A-ANTIG (1)) * 0.18              
                     MOVE '*M106 ALUMB.HR.D'  TO LEYENM106   
                     ELSE
             IF POS-17 = '3' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                     
                                            A-ANTIG (1)) * 0.30              
                     MOVE '*M106 ALUMB.CUR3'  TO LEYENM106   
                     ELSE
             IF POS-17 = '2' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                     
                                           A-ANTIG (1))  * 0.20              
                     MOVE '*M106 ALUMB.CUR2'  TO LEYENM106   
                     ELSE
             IF POS-17 = '1' AND (POS-5 = '3' OR '6')                                      
                     THEN
                     COMPUTE BONIM106 = ((H-HABER * PORC / 100) +                     
                                            A-ANTIG (1)) * 0.10              
                     MOVE '*M106 ALUMB.CUR1'  TO LEYENM106   
                     ELSE GO TO F-BONI-M106. 
              ADD 1 TO I.
              MOVE REG-MAE TO REGISTRO (I).
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
              MOVE BONIM106 TO A-HABER (I).
              MOVE LEYENM106   TO A-CODIGO (I).
              MOVE '02' TO A-CONC (I). 
      *
       F-BONI-M106. EXIT.                  

       BONI-M106-1.                        
              COMPUTE BONIM1061 = (H-HABER * POS-18-19R / 100)                  
                                      * PORC /100.
              ADD 1 TO I.
              MOVE REG-MAE TO REGISTRO (I).
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
              MOVE BONIM1061 TO A-HABER (I).
              MOVE '*DEDIC.EXCL. ' TO A-CODIGO (I).
              MOVE '02' TO A-CONC (I). 
      *
       F-BONI-M106-1. EXIT.                  
       BONI-M050.                  
           MOVE ZEROS TO PT0501 PT0502 PT0503 PT0504.
           IF FECHA-MES > '201601' THEN
             MOVE 0.3 TO PT0501
             MOVE 0.25 TO PT0502
             MOVE 0.15 TO PT0503
             MOVE 0.08 TO PT0504
                                   ELSE
             MOVE 0.2 TO PT0501
             MOVE 0.15 TO PT0502
             MOVE 0.1 TO PT0503
             MOVE 0.08 TO PT0504.
           IF POS-17 = '1'                                                      
             THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * PT0501                             
             ELSE IF POS-17 = '2'                                                      
                 THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * PT0502                                 
                 ELSE IF POS-17 = '3'                                                               
             THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * PT0503                          
             ELSE IF POS-17 = '4'                                                               
                 THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * PT0504                                
                 ELSE IF POS-17 = '5'                                                               
             THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * 0.1                 
             ELSE IF POS-17 = '6'                                                               
                   THEN COMPUTE BONIM050 = (H-HABER * PORC / 100) * 0.3                                  
                   ELSE GO TO F-BONI-M050. 
              ADD 1 TO I.
              MOVE REG-MAE TO REGISTRO (I).
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
              MOVE BONIM050 TO A-HABER (I).
              MOVE '*BON.TITUM050 ' TO A-CODIGO (I).
              MOVE '02' TO A-CONC (I). 
      *
       F-BONI-M050. EXIT.                  
 
       BONI-M065.                        
            ADD 1 TO I.
            MOVE REG-MAE TO REGISTRO (I).
            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
            MOVE '*BON.TITUM065 ' TO A-CODIGO (I).
            MOVE '02' TO A-CONC (I). 
            IF POS-17 = '1'                                                               
               THEN
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.25                           
               ELSE IF POS-17 = '2'                                                               
                    THEN
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.15                           
                    ELSE IF POS-17 = '3'                                                               
                          THEN
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.10                           
                          ELSE IF POS-17 = '4'                                                               
                                 THEN
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.05                           
                                 ELSE
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.03.                          
       F-BONI-M065. EXIT.                  

       BONI-M135.                        
            IF POS-17 = 'U' OR 'V' OR 'W'                                                               
                      THEN NEXT SENTENCE
                      ELSE GO TO F-BONI-M135.
                    
            ADD 1 TO I.
            MOVE REG-MAE TO REGISTRO (I).
            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
            MOVE '*BON.TITUM135 ' TO A-CODIGO (I).
            MOVE '02' TO A-CONC (I). 
            IF POS-17 = 'U'                                                               
             THEN
               COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.20                           
             ELSE IF POS-17 = 'V'                                                               
                    THEN
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.30                           
                    ELSE         
                 COMPUTE A-HABER (I) = (H-HABER * PORC / 100) * 0.35.                           
           COMPUTE A-ANTIG(I) = A-HABER(I) * A-ANTIG(1) / A-HABER(1).                 
           MOVE A-HABER (I) TO TITU135. 
       F-BONI-M135. EXIT.                  
       CALCULA-BONIF-SH.
           IF CONC = '01' THEN
                 IF FECHA-MES NOT < '199102'
                    THEN PERFORM CALCULA-20 THRU FIN-CALCULA-20.
   ******  IF POS-1-4 = 'M049' AND MARCAAMP = 1
   ******     THEN DISPLAY REG-MAE
   ******          DISPLAY REGISTRO (I).

      ******* M109 ORD 2246/2015 art2  mov set15.    
           IF CONC = '01' AND POS-1-4 = 'M109'                                          
                    AND FECHA-MES > '201508'
             THEN
               IF POS-5 = '9'  THEN GO TO FIN-CALCULA-BONIF-SH        
                               ELSE
               IF SI-LETRA-ANTIG = SPACES THEN NEXT SENTENCE          
                                          ELSE
               IF SI-LETRA-ANTIG = 'A' OR 'B' OR 'C'                  
                  THEN COMPUTE BONI109 = 200 * PORC / 100
                  ELSE IF SI-LETRA-ANTIG = 'D' OR 'E' OR 'F'                  
                    THEN COMPUTE BONI109 = 400 * PORC / 100
                    ELSE IF SI-LETRA-ANTIG = 'G' OR 'H' OR 'I'                  
                                OR 'K' OR 'L' OR 'J'
                  THEN COMPUTE BONI109 = 600 * PORC / 100
                  ELSE IF SI-LETRA-ANTIG = 'M' OR 'N' OR 'O'                  
               THEN COMPUTE BONI109 = 800 * PORC / 100
               ELSE IF SI-LETRA-ANTIG = 'P' OR 'Q' OR 'R'                  
                     THEN COMPUTE BONI109 = 1000 * PORC / 100
                     ELSE IF SI-LETRA-ANTIG = 'S' OR 'T' OR 'U'                  
                                              OR 'V' OR 'W'            
                     THEN COMPUTE BONI109 = 1200 * PORC / 100
                     ELSE GO TO FIN-CALCULA-BONIF-SH                                          
             ELSE GO TO ALAM130.
           IF BONI109 > ZEROS THEN 
                   ADD 1 TO I
                   MOVE REG-MAE TO REGISTRO (I)
                   MOVE ZEROS   TO A-IOMA (I)            
                   MOVE '02' TO A-CONC (I)  
                   MOVE '*M109 ORD2246-152'  TO A-CODIGO (I)
                   COMPUTE A-HABER(I) = BONI109                
                   MOVE 0 TO BONI109
                              ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

       ALAM130.
      ******* M130 ORD 2782/2011 art23 autor en expte 21557-42106/06, mov may15 !!!
           IF CONC = '01' AND POS-1-4 = 'M130'                                          
             THEN
               IF POS-5 = 'B' OR 'T'  THEN NEXT SENTENCE                     
                                      ELSE
               IF POS-13-14N < 20 THEN NEXT SENTENCE                     
                                  ELSE
               IF FECHA-MES > '201201'                                       
                 THEN  PERFORM TODOM130 THRU TODOM130-X.                                                                   

      ******* M044 genero bonif jorn prolong para 48 h, mov ene14. MOV.JUL16: genero boni p/todos los cod del 2015 *N**48.  
           IF  POS-1-4 = 'M044' AND POS-9-10 = '48'                      
            THEN
              IF ((POS-5-8 = '531B' OR '6S13' OR '5O05') AND                     
                  FECHA-MES > '201101') OR                                    
                 (POS-5-8 = '6121' AND FECHA-MES > '200812')
                  OR (POS-6 = 'N' AND FECHA-MES > '201501')
                 THEN                                                                     
                   ADD 1 TO I
                   MOVE REG-MAE TO REGISTRO (I)
                   MOVE ZEROS   TO A-IOMA (I)            
                   MOVE '02' TO A-CONC (I)  
                   MOVE '*M044 JOR PROL.  '  TO A-CODIGO (I)
                   COMPUTE A-HABER(I) = A-HABER(1) * 50 / 100
                   COMPUTE A-ANTIG(I) = A-ANTIG(1) * 50 / 100
                 ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M045 ORD 6/2006  sf=50 y Dto 104/2007 sf=100, expte 21557-50853-06, mov ago13 !!!
           IF CONC = '01' AND POS-1-4 = 'M045'                                          
             THEN
               IF FECHA-MES > '200605' AND                                   
                      FECHA-MES < '201002'                                       
                 THEN                                                                     
                       ADD 1 TO I
                       MOVE REG-MAE TO REGISTRO (I)
                       MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                       MOVE '02' TO A-CONC (I)  
                       MOVE '*M045 SF NOREM   '  TO A-CODIGO (I)
                       IF FECHA-MES < '200704'                                   
                        THEN COMPUTE A-HABER(I) = 50 * PORC / 100 
                        ELSE COMPUTE A-HABER(I) = 100 * PORC / 100 
                 ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M078 ORD 5244/12 sf por unica vez, mov abr13.                            
           IF CONC = '01' AND POS-1-4 = 'M078'                                          
             THEN
                   IF FECHA-MES = '201302'                                   
                    THEN                                                                     
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I)  
                          MOVE '*M078 DTO 5244/12'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 500 * PORC / 100 
                    ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M004 DTO 469/12 sf por unica vez, mov set12 y modificatorios, para 2014 agrego 'N'. EN ENE15 tb para Super.
      ******* M004: Mov.mayo16, agr S200-s153-s154 en esta preg pq es de pta pte. 
           IF CONC = '01' AND POS-1-4 = 'M004'                                          
             THEN
                  IF ((POS-5 = '2' OR '3' OR '4' OR '5' OR                       
                          '6' OR '8' OR '9' OR 'M' OR 'N') OR                 
                     (POS-5-8 = 'S3S3' OR '13S3' OR 'S303' OR
                      '1303' OR 'S3S4' OR 'S304' OR 'S310' OR
                      'S309' OR 'S480' OR '1480' OR 'S479' OR
                      '1479' OR '1304' OR 'S200' OR 'S154' OR 'S153'))                                   
                    THEN
                      IF FECHA-MES = '201202' OR '201203' OR                      
                                     '201302' OR '201303' OR                     
                                     '201402' OR '201403' OR                     
                                     '201502' OR '201503' OR '201504'                        
                                  OR '201602' OR '201603' OR '201604'                        
                        THEN                                                                     
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I)  
                          IF FECHA-MES = '201202' OR '201203'                       
                            THEN
                          MOVE '*M004 DTO 469/12'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 666 * PORC / 100 
                            ELSE
                          IF FECHA-MES = '201302' OR '201303'                       
                            THEN
                          MOVE '*M004 DTO5203/12'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 950 * PORC / 100 
                            ELSE
                          IF FECHA-MES = '201402' OR '201403'                        
                            THEN
                          MOVE '*M004 DTO5016/13'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 1235 * PORC / 100 
                            ELSE
                         IF FECHA-MES = '201502' OR '201503' OR '201504'                        
                            THEN
                          MOVE '*M004 DTO4497/14'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 1606 * PORC / 100 
                            ELSE
                          MOVE '*M004 DTO4222/15'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 1927 * PORC / 100 
                    ELSE GO TO FIN-CALCULA-BONIF-SH                                          
      *********************** en 2015 tb lo cobraron los S:
                 ELSE
                     IF POS-5 = 'S' AND                 
                        (FECHA-MES = '201502' OR '201503' OR '201504')                       
                            THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '02' TO A-CONC (I)  
                          MOVE '*M004 DTO4497/14'  TO A-CODIGO (I)
                          COMPUTE A-HABER(I) = 1606 * PORC / 100 
                            ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M122 bonificacion por actividad riesgosa para guardavidas               
           IF CONC = '01' AND POS-1-4 = 'M122'                                          
                          AND POS-5-8 = 'GUAR' AND                               
                FECHA-MES > '200901'  THEN
                 IF POS-9-10N > 0
                    THEN  ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '*M122 ACT.RIESG.'  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          COMPUTE A-HABER(I) = H-HABER * POS-9-10N
                                               / 100 * PORC / 100.

      ******* M089 DTO 305/12 pago aca, antes en gehi.                                
           IF CONC = '01' AND POS-1-4 = 'M089'                                          
                          AND (POS-5-8 = 'S103' OR 'S106'                         
                 OR 'S501' OR 'S484' OR 'S450')                         
             THEN
                   IF FECHA-MES < '201202'                        
                    THEN GO TO FIN-CALCULA-BONIF-SH                                          
                    ELSE
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '*M089 DTO 305/12'  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)  
                          IF FECHA-MES < '201204'                        
                     THEN COMPUTE A-HABER(I) = 1500 * PORC / 100 
                     ELSE COMPUTE A-HABER(I) = 1350 * PORC / 100.

      ******* M027 dde ppto ene13 cargo cod para bonf dispos pte y bloq de tit.     
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M027'                                          
                                    AND EME = ' '                                          
             THEN
                   IF FECHA-MES > '201301'                        
                    THEN
               PERFORM TODOM027 THRU TODOM027-X.                

      ******* M069 dde ppto ene12 cargo cod para bonf dispos pte y titulos.         
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M069'                                          
                                    AND EME = ' '                                          
             THEN
                   IF FECHA-MES > '201101'                        
                    THEN
               PERFORM TODOM069 THRU TODOM069-X.                

      ******* M012 dde ppto mar12 paso bonif de gehi para aca y genero otras nuevas.
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M012'                                          
                                    AND EME = ' '                                          
             THEN
                   IF FECHA-MES < '201204'                        
                    THEN GO TO FIN-CALCULA-BONIF-SH                                          
                    ELSE
               PERFORM TODOM012 THRU TODOM012-X                 
               GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M112  tiempo pleno para S463, movimiento nov13.     
           IF POS-1-4 = 'M112' AND POS-17 = 'P'                                          
             THEN IF POS-5-8 = 'S463'                                          
               THEN
                   IF FECHA-MES < '200601'                        
                    THEN GO TO FIN-CALCULA-BONIF-SH                                          
                    ELSE
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '*M112 T.PLENO  '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)  
                          IF FECHA-MES < '201006'
                              THEN
                      COMPUTE A-HABER(I) = A-HABER(1) * 0.4              
                      COMPUTE A-ANTIG(I) = A-ANTIG(1) * 0.4              
                              ELSE
                          IF FECHA-MES < '201106'
                              THEN
                      COMPUTE A-HABER(I) = A-HABER(1) * 0.46             
                      COMPUTE A-ANTIG(I) = A-ANTIG(1) * 0.46             
                              ELSE
                      COMPUTE A-HABER(I) = A-HABER(1) * 0.52             
                      COMPUTE A-ANTIG(I) = A-ANTIG(1) * 0.52              
               ELSE
      *********** agrego mov dic14.
                  IF POS-5-8 = 'S262'                                          
                   THEN
                    IF FECHA-MES < '201405'                        
                     THEN GO TO FIN-CALCULA-BONIF-SH                                          
                     ELSE
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '*M112 T.PLENO  '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)  
                          COMPUTE A-HABER(I) = A-HABER(1) * 0.2564              
                          COMPUTE A-ANTIG(I) = A-ANTIG(1) * 0.2564.             

      ******* M112 SF no rem expte 21557-180691-11 DTO 362/10     
           IF CONC = '01' AND POS-1-4 = 'M112'                                          
             THEN
                   IF FECHA-MES < '201005'                        
                    THEN GO TO FIN-CALCULA-BONIF-SH                                          
                    ELSE
                IF POS-5-8 = 'S450' OR 'S484' OR 'S100' OR 'S121' OR     
                           'S007' OR 'S051' OR 'S501' OR 'S302'        
                  THEN
                          ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE '*M112           '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I)  
                          IF FECHA-MES < '201008'
                              THEN
                      COMPUTE A-HABER(I) = H-HABER * 0.08 * PORC / 100
                              ELSE
                          IF FECHA-MES < '201106'
                              THEN
                      COMPUTE A-HABER(I) = H-HABER * 0.16 * PORC / 100
                              ELSE
                          IF FECHA-MES < '201108'
                              THEN
                      COMPUTE A-HABER(I) = H-HABER * 0.26 * PORC / 100
                              ELSE
                      COMPUTE A-HABER(I) = H-HABER * 0.36 * PORC / 100
                  ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M030 SF no rem expte 21557-70868-07  DTO 602/07, 21557-74358-07 DTO 3/07 hcd
      ******* M030 SF no rem expte 21557-74359-07  DTO 649/07     
           IF CONC = '01' AND POS-1-4 = 'M030'                                          
             THEN
                IF POS-5 = '9' OR 'S'                                  
                  THEN GO TO FIN-CALCULA-BONIF-SH                                          
                  ELSE
                   IF FECHA-MES < '200705' OR FECHA-MES > '200908'
                    THEN GO TO FIN-CALCULA-BONIF-SH                                          
                    ELSE
                   IF FECHA-MES = '200705'
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 106.87 * PORC / 100
                            MOVE '*M030 ABR07     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                          ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) =  50 * PORC / 100
                            MOVE '*M030 ABR07-2   '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                     ELSE
                   IF FECHA-MES > '200704' AND FECHA-MES < '200710'
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 50 * PORC / 100
                            MOVE '*M030 ABR07-2   '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
      ******* M030 SF no rem expte 21557-74360-07  DTO 720/07     
                      IF FECHA-MES = '200706' OR FECHA-MES = '200707'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                       COMPUTE A-HABER (I) = H-HABER * 0.18 * PORC / 100
                            MOVE '*M030 MAY07     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE   
      ******* M030 SF no rem  DTO 901/07     
                      IF FECHA-MES = '200708'                           
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                       COMPUTE A-HABER (I) = H-HABER * 0.12 * PORC / 100
                            MOVE '*M030 JUL07     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE   
      ******* M030 SF no rem  DTO 1168/07     
                      IF FECHA-MES = '200709'                           
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                       COMPUTE A-HABER (I) = H-HABER * 0.06 * PORC / 100
                            MOVE '*M030 AGO07     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE NEXT SENTENCE
                     ELSE   
      ******* M030 SF no rem  expte 21557-120459-09 DTO 1732/07     
                      IF FECHA-MES > '200710' AND FECHA-MES < '200905'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 38 * PORC / 100
                            MOVE '*M030 OCT07     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
      ******* M030 SF no rem  expte 21557-120459-09 DTO 1414/08     
      *****           IF FECHA-MES > '200809' OR FECHA-MES < '200905'
                      IF FECHA-MES > '200809' AND FECHA-MES < '200905'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 300 * PORC / 100
                            MOVE '*M030 SET08     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE   
                      IF FECHA-MES = '200905'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 220 * PORC / 100
                            MOVE '*M030 SET08     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE   
                      IF FECHA-MES = '200906' OR FECHA-MES = '200908'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 110 * PORC / 100
                            MOVE '*M030 SET08     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                  ELSE GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M092 SF no rem expte 21557-159866-10 DTO 804/09     
           IF CONC = '01' AND POS-1-4 = 'M092'                                          
             THEN
                IF POS-5 = '9' OR (POS-5-8 = 'S050' OR 'S450'
                       OR 'S451' OR 'S484' OR 'S100' OR 'S463'                        
                       OR 'S101' OR 'S102' OR 'S108' OR 'S501'                        
                       OR 'S106')                                                     
                  THEN GO TO FIN-CALCULA-BONIF-SH                                          
                  ELSE
                   IF FECHA-MES > '200912'
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 200 * PORC / 100
                            MOVE '*M092 DIC09     '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                            GO TO FIN-CALCULA-BONIF-SH.                                         

      ******* M055 SF no rem expte 21557-183619-11 DTO 1142/10    
           IF CONC = '01' AND POS-1-4 = 'M055'
              THEN
              IF FECHA-MES = '201102' OR '201106' OR '201108'                                        
                          OR '201109' OR '201110' OR '201111'                                        
                THEN
                IF POS-5-8 = 'S050' OR 'S252'               
                    THEN GO TO FIN-CALCULA-BONIF-SH
                    ELSE ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M055      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         IF FECHA-MES = '201102'                                                                
                         THEN COMPUTE A-HABER (I) = 450 * PORC / 100
                              GO TO FIN-CALCULA-BONIF-SH 
                         ELSE COMPUTE A-HABER (I) = 400 * PORC / 100
                              GO TO FIN-CALCULA-BONIF-SH 
                ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M002 SF no rem expte 21557-163370-10 DTO 524/09; doy de baja en mov jun16 pq no se informa.     
           IF CONC = '01' AND POS-1-4 = 'M002'
              THEN
           IF FECHA-MES > '200909' AND FECHA-MES < '201604'                                                      
                  THEN
                IF POS-5-8 = 'S050' OR POS-5 = '2'          
                    THEN GO TO FIN-CALCULA-BONIF-SH
                    ELSE ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M002      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         COMPUTE A-HABER (I) = 180 * PORC / 100
                         GO TO FIN-CALCULA-BONIF-SH.

      ******* M066 SF no rem expte 21557-37597-06 ORD 1140/05    
      ******* M066 SF no rem expte 21557-62716-07 ORD 1265/06 ratif la q venian cobra, dde oct09 varia s/cat.,amplio la ant 
           IF CONC = '01' AND POS-1-4 = 'M066'
              THEN
 **********IF FECHA-MES > '200601' AND FECHA-MES <'200802'                               
           IF FECHA-MES > '200601' AND FECHA-MES < '200911'                               
                  THEN
                IF POS-5-8 = ('S001' OR 'S501' OR 'S302') OR 
                 (POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6')  
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M066      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         COMPUTE A-HABER (I) = 100 * PORC / 100
                    ELSE GO TO FIN-CALCULA-BONIF-SH
                  ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M097 SF no rem expte 21557-135821-09 DTO 348/09     
           IF CONC = '01' AND POS-1-4 = 'M097'
              THEN
           IF FECHA-MES = '200902' OR '200903' OR '200904'                               
                       OR '200905' OR '200906' OR '200907'                               
                  THEN
                  IF POS-5 = 'S' THEN GO TO FIN-CALCULA-BONIF-SH
                                 ELSE
                   IF POS-5-8 ='5018' OR '5019' OR '5020' OR '6017'
                            OR '6012' OR '5014' OR '6011'           
                     THEN MOVE 78 TO BASE-097
                     ELSE
                   IF POS-5-8 ='2008' OR '4013' OR '5011' OR '5012'
                            OR '6010' OR '3006' OR '5009' OR '5010' 
                            OR '3004' OR '5008' OR '6006'              
                     THEN MOVE 60 TO BASE-097
                     ELSE
                   IF POS-5-8 ='6005' OR '5005' OR '5006' OR '4004'
                            OR '5003' OR '5004' OR '6002' OR '5001' 
                            OR '5002'                                 
                     THEN MOVE 54 TO BASE-097
                     ELSE
                   IF POS-5-8 ='1011' OR '1009' OR '1007' OR '1008'                     
                            OR '1005'                                 
                     THEN MOVE 42 TO BASE-097
                     ELSE
                   IF POS-7-8 > '15' THEN GO TO FIN-CALCULA-BONIF-SH
                                     ELSE
                      IF POS-7-8 < '04' THEN MOVE 78 TO BASE-097
                                        ELSE
                      IF POS-7-8 < '07' THEN MOVE 60 TO BASE-097
                                        ELSE
                      IF POS-7-8 < '10' THEN MOVE 54 TO BASE-097
                                        ELSE MOVE 42 TO BASE-097
                  ELSE GO TO ALAM038               
              ELSE GO TO ALAM038.
           IF BASE-097 > ZEROS THEN
                      ADD 1 TO I
                      MOVE REG-MAE TO REGISTRO (I)
                      MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                      MOVE '*BONI.M097      '  TO A-CODIGO (I)
                      MOVE '02' TO A-CONC (I)  
                      IF POS-9-10 = '12' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 2 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '20' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 3.3 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '24' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 4 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '31' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 5 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '35' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 5.8 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '40' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 6.7 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '42' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 7 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '44' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 7.3 * PORC / 100
                                         ELSE                                            
                      IF POS-9-10 = '48' THEN          
                      COMPUTE A-HABER (I) = BASE-097 * 8 * PORC / 100.
           GO TO FIN-CALCULA-BONIF-SH.
       ALAM038.
      ******* M097 SF no rem expte 21557-186095-11 DTO 2/11     
           IF CONC = '01' AND POS-1-4 = 'M097'
              THEN
           IF FECHA-MES = '201102' OR '201103' OR '201104'                               
                       OR '201105' OR '201106' OR '201107'                               
                THEN
           IF POS-9-10 = '48'                                                           
                 THEN
                  IF POS-5-8 = 'T007' OR 'T008' OR 'T009' OR 'M007'
                             OR 'A007' OR 'A008' OR 'A009' OR 'O008'
                   OR '5001' OR 'O009' OR '6005' OR '5005' OR '5006'
                   OR '5002' OR '4004' OR '5003' OR '5004' OR '6002'
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M097      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         COMPUTE A-HABER (I) = 414 * PORC / 100.
     **ALAM038.
      ******* M038 SF no rem expte 21557-158092-10 ORD 270/09 art4
      ******* Mov feb14 doy baja a los S dde set13                
           IF CONC = '01' AND POS-1-4 = 'M038'
              THEN
           IF FECHA-MES > '201001' AND FECHA-MES < '201403'                                                  
                  THEN
                  IF (POS-5 = '2' OR '3' OR '4' OR '5' OR '6'
                      OR '9') OR (POS-5 = 'N' AND POS-7-8 > '01') OR
                     ((POS-5-8 = 'S051' OR 'S501' OR 'S302') AND
                       FECHA-MES < '201310')                                                 
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M038      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         COMPUTE A-HABER (I) = 100 * PORC / 100
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
                  ELSE GO TO FIN-CALCULA-BONIF-SH.
 
      ******* M052 SF no rem expte 21557-174891-10 DTO 1745/10
           IF CONC = '01' AND POS-1-4 = 'M052'
              THEN
           IF FECHA-MES > '200905' AND FECHA-MES < '201009'                          
                  THEN
                  IF (POS-5 = 'A' OR 'E' OR 'C') OR (POS-5 = 'N' 
                      AND POS-7-8 > '05') OR (POS-5-8 = 'S303' OR  
                       'S304' OR 'S600')  
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M052      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         IF FECHA-MES < '201005'                                                   
                           THEN
                         COMPUTE A-HABER (I) = 200 * PORC / 100
                           ELSE
                         COMPUTE A-HABER (I) = 500 * PORC / 100
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
                  ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M041 SF no rem expte 21557-134172-09 DTO 430/09 
           IF CONC = '01' AND POS-1-4 = 'M041'
              THEN
           IF FECHA-MES = '200906' OR FECHA-MES = '200908'                           
             THEN IF (POS-5-8 = 'S501' OR 'S302') OR               
                   (POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6')
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         COMPUTE A-HABER (I) = 100 * PORC / 100
                         MOVE '*BONI.M041 UNICA'  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
             ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M007 SF no rem expte 21557-134175-09  DTO 32/09 
      ******* M007 SF no rem expte 21557-162395-10  DTO 350/10 sf por abr10.
      ******* M007 SF no rem expte 21557-182328-11  DTO 1652/11 sfs por ene11.
           IF CONC = '01' AND POS-1-4 = 'M007'
              THEN
           IF FECHA-MES = '200902' OR '201003' OR '201005' OR '201102'
             THEN IF POS-5-8 = 'S104' OR (POS-5 = '1' OR '2' OR '3' 
                                      OR '4' OR '5' OR '6' OR '9')
                    THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M007 UNICA'  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         IF FECHA-MES = '200902'                             
                          THEN COMPUTE A-HABER (I) = 360 * PORC / 100
                          ELSE
                         IF FECHA-MES = '201003'                             
                          THEN COMPUTE A-HABER (I) = 450 * PORC / 100
                          ELSE
                         IF FECHA-MES = '201005'                             
                          THEN COMPUTE A-HABER (I) = 225 * PORC / 100
                          ELSE COMPUTE A-HABER (I) = 434 * PORC / 100
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
             ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M028 SF no rem expte 21557-266148-13 1000 por mar13 a jun13 inclusive.
      ******* M028 SF no rem expte 21557-127956-09            
      ******* M028 SF no rem expte 21557-144720-09, set09     
           IF CONC = '01' AND POS-1-4 = 'M028'
                 THEN
           IF FECHA-MES = '201304' OR '201305' OR '201306' OR '201308'   
             THEN IF (POS-5-8 = 'S252' OR 'S450' OR 'S463' OR 'S050' 
                     OR 'S484' OR 'N00A' OR 'N00B' OR 'S300' OR 'S301' 
                     OR 'S147' OR 'S509')                                    
                    THEN NEXT SENTENCE
                    ELSE ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M028 UNICA'  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         COMPUTE A-HABER (I) = 1000 * PORC / 100
             ELSE
           IF FECHA-MES = '201102' OR '200910'                           
             THEN IF (POS-5-8 = 'S252' OR 'S450' OR 'S463' OR 'S050' 
                     OR 'S484' OR 'N00A' OR 'N00B' OR 'S300' OR 'S301')
                    THEN NEXT SENTENCE
                    ELSE ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         MOVE '*BONI.M028 UNICA'  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
                         IF FECHA-MES = '201102'                                       
                          THEN COMPUTE A-HABER (I) = 370 * PORC / 100
                          ELSE COMPUTE A-HABER (I) =                  
                                (A-HABER(1) + A-ANTIG(1)) * 0.1
             ELSE
           IF FECHA-MES = '200902'                             
             THEN IF (POS-5-8 = 'S252' OR 'S450' OR 'S463' OR 'S050')
                    THEN NEXT SENTENCE
                    ELSE ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         COMPUTE A-HABER (I) = 500 * PORC / 100
                         MOVE '*BONI.M028 UNICA'  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I)  
             ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M108 SF no rem expte 21557-132469-09. Ord 4133/09,4172/09,dtos 64/11,260/11
      ******* M108 SF no rem expte 21557-148018-10. Ord 4149/09,4196/09,para cmh,movjul14.
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M108'
              THEN
           IF FECHA-MES > '200904' AND FECHA-MES < '201002'    
             THEN IF (POS-5 = '3' OR '4' OR '5' OR '6')
                   THEN ADD 1 TO I
                        MOVE REG-MAE TO REGISTRO (I)
                        MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        COMPUTE A-HABER (I) = 200 * PORC / 100
                        MOVE '*BONI.M108      '  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                   ELSE IF POS-5 = '2' AND FECHA-MES > '200907'
                         THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 200 * PORC / 100
                            MOVE '*BONI.M108      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                         ELSE IF POS-5 = '9'                           
                          THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            MOVE '*BONI.M108      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                      IF (POS-6 = 'H' OR 'I' OR 'K') AND POS-9-10 = '36'
                            THEN COMPUTE A-HABER (I) = 400 * PORC / 100
                            ELSE
                      IF (POS-6 = 'H' OR 'I' OR 'K') AND POS-9-10 = '24'
                            THEN COMPUTE A-HABER (I) = 300 * PORC / 100
                            ELSE
                      IF (POS-6 = 'B' OR 'E') AND POS-9-10 = '36'
                            THEN COMPUTE A-HABER (I) = 350 * PORC / 100
                            ELSE
                      IF (POS-6 = 'B' OR 'E') AND POS-9-10 = '24'
                            THEN COMPUTE A-HABER (I) = 250 * PORC / 100
                            ELSE GO TO FIN-CALCULA-BONIF-SH 
                          ELSE GO TO FIN-CALCULA-BONIF-SH 
             ELSE                              
           IF (FECHA-MES = '201102' OR  FECHA-MES = '201103')   
             AND  (POS-5 = '2' OR '3' OR '4' OR '5' OR '6')
                          THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 200 * PORC / 100
                            MOVE '*BONI.M108      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                          ELSE                                
           IF (FECHA-MES = '201104' OR  FECHA-MES = '201105')   
             AND  (POS-5 = '2' OR '3' OR '4' OR '5' OR '6')
                          THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 220 * PORC / 100
                            MOVE '*BONI.M108      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                          ELSE                                
           IF FECHA-MES = '201204' AND                           
                  (POS-5 = '2' OR '3' OR '4' OR '5' OR '6')
                          THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = 400 * PORC / 100
                            MOVE '*BONI.M108      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
                          ELSE GO TO FIN-CALCULA-BONIF-SH.

      *****  mov.jun16 agr 2 bonif x unica vez son ppto dic pero pongo como enero por el sac.   
           IF CONC = '01' AND POS-1-4 = 'M074'
              THEN IF FECHA-MES = '201502' OR '201602'     
                    THEN IF (POS-5-8 = 'C005' OR 'CJ00' OR 'N005' OR
                            'S252' OR 'S470' OR 'S450' OR 'S484' OR
                            'S451' OR 'S471' OR 'S485' OR 'S050' OR
                           'S463') OR (POS-5 = '7' OR '9' OR 'B' OR 'T')
                           THEN NEXT SENTENCE
                           ELSE
                            ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            MOVE '02' TO A-CONC (I)  
                            IF FECHA-MES = '201502' THEN
                            COMPUTE A-HABER (I) = 1500 * PORC / 100
                            MOVE '*BONI.M074 D3742'  TO A-CODIGO (I)
                                                    ELSE
                            COMPUTE A-HABER (I) = 2000 * PORC / 100
                            MOVE '*BONI.M074 D3952'  TO A-CODIGO (I).

           IF CONC = '01' AND POS-1-4 = 'M074'
             THEN IF SUMAFIJ2 > ZEROS
              THEN IF (FECHA-MES > '200407' AND FECHA-MES < '200502')     
                     OR FECHA-MES > '200608'                              
     ******          OR (FECHA-MES > '200608' AND FECHA-MES < '201002'))dde ago09 no caduca por anio
  **************     no tenemos fecha de caducidad de la bonificacion
                     THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100
                            MOVE '*BONI.M074      '  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I)  
  ***********saco todo el else porque lo pagaria doble en codigo y nvo sist
                     ELSE                                                      
                      IF FECHA-MES > '200501' AND FECHA-MES < '200609'     
                         THEN ADD 1 TO I
                         MOVE REG-MAE TO REGISTRO (I)
                         MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                         COMPUTE A-HABER (I) = SUMAFIJ2 * 2 * PORC / 100 
                         MOVE '*BONI.M074      '  TO A-CODIGO (I)
                         MOVE '02' TO A-CONC (I).

      ******* M056 bonificacion = al 10% del haber liquido   
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M056'
              THEN
           IF FECHA-MES > '200703'                             
                      THEN GO TO NOVAMAS
                      ELSE
           IF FECHA-MES > '200407'                             
             THEN IF POS-5-8 = 'S252' OR 'S050'
                    THEN NEXT SENTENCE
                    ELSE
                     IF POS-5 NOT = 'S' AND FECHA-MES > '200603'
                      THEN NEXT SENTENCE
                      ELSE
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = (((H-HABER + SUMAFIJ2) *                 
                                        PORC / 100) + A-ANTIG (1))
                                       * 0.0952                        
                     MOVE '*BONI.M056      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I).

       NOVAMAS.              
      ******* M102 bonificacion = al  4% del haber basico de jun09, en tab
           IF (CONC = '01' AND POS-1-4 = 'M102') OR
              (CONC = '02' AND POS-1-4 = 'M102' AND PORC > '00')
              THEN
           IF FECHA-MES > '200908'                             
                 THEN PERFORM SU-M102 THRU SU-M102-X          
                      VARYING T FROM 1 BY 1 UNTIL T > 44 
                      IF S-M102 > ZEROS
                    THEN              
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = S-M102 * PORC / 100                                      
                     MOVE '*BONI.M102      '  TO A-CODIGO (I)
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
                 ELSE
      ******* M102 bonificacion = al 10% del haber nominal   
           IF FECHA-MES > '200407' AND FECHA-MES < '200707'    
             THEN IF (POS-5-6 = 'N0' AND POS-7 < '2')
                    OR (POS-5-6 = '25' OR '26' )                   
                    THEN              
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = (A-HABER (1) + A-ANTIG (1)) *                 
                                        0.10                               
                     MOVE '*BONI.M102      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
                 ELSE
      ******* M102 bonificacion = al 15% del haber basico, expte 21557-82487-07, pago entre jul07 y dic08 pq no encuentro datos.
           IF FECHA-MES > '200707' AND FECHA-MES < '200902'    
             THEN IF (POS-5-6 = 'N0' AND POS-7 < '2')
                    OR (POS-5-6 = '25' OR '26' )                   
                    THEN              
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = A-HABER (1) * 0.15                           
                     MOVE '*BONI.M102      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

      ******* M091 bonificacion = 110$                          
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M091'
              THEN
                  IF FECHA-MES > '200812' AND POS-5 NOT = 'J'
                                    THEN NEXT SENTENCE
                                    ELSE                     
                  IF (POS-5-8 = 'S252' OR 'S450' OR 'S484' OR 'S470'
                    OR 'S007' OR 'S127' OR 'S604' OR 'S312' OR 'S100'
                    OR 'S603' OR 'S050' OR 'S463' OR 'S454' OR 'S017'
                    OR 'S010' OR 'S499' OR 'M00S' OR 'M00M' OR 'M00B')                
                    OR (POS-5 = '9')                 
                    THEN NEXT SENTENCE
                    ELSE IF FECHA-MES > '200701'
                      THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = 220.00 *                 
                                        PORC / 100                     
                     MOVE '*BONI.M091      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                    ELSE IF FECHA-MES > '200602'
                      THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     COMPUTE A-HABER (I) = 168.00 *                 
                                        PORC / 100                     
                     MOVE '*BONI.M091      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I)  
 * **************************************************************                 
                    ELSE IF FECHA-MES > '200503'
                      THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = 160.00 *                 
                                        PORC / 100                     
                     MOVE '*BONI.M091      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                      ELSE                               
                     IF FECHA-MES > '200408'
                         THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = 110.00 *                 
                                        PORC / 100                     
                     MOVE '*BONI.M091      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I).

      ******* M091 bonificacion permanencia en el cargo, no es p/docentes ni cmh, mov feb14.
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M091'
            THEN
             IF FECHA-MES > '200607' AND (POS-5 NOT = 'J' OR '9' OR 'T')
                THEN                
                   IF POS-17-18 IS NUMERIC AND POS-17-18N > ZEROS                
                         THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = SUMAFIJ3 * POS-17-18N * 2 *
                                        PORC / 10000                   
                     MOVE '*BONI.M091 PERM '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I).

      ******* M091 bonificacion por fcion y gtos rep para 1002 y 1003 aca pq son muchos pptos a cargar, mov marzo14.
      ******* EN DIC14 saco las bon fcion y gtos rep para 1002 y 1003 de aca  pq quieren codigos '02'.                     
      ******* EN JUL15 calculo deuda por el periodo febrero12 a noviembre14 inclusive y elimino estas 2 bonificaciones.
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M091'
            THEN NEXT SENTENCE
             IF FECHA-MES > '200607' AND FECHA-MES < '201501'         
                                     AND (POS-5-8 = '1002' OR '1003') 
                THEN                
                     IF POS-5-8 = '1002' THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '*BONI.M091 FCION'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = 220 * PORC / 100
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '*BONI.M091 GTOS.'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = 345 * PORC / 100
                                         ELSE
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '*BONI.M091 FCION'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = 185 * PORC / 100
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '*BONI.M091 GTOS.'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = 290 * PORC / 100.
               

      ******* Bonif.Hospitalaria p/agrup 3-4-5-6 con 40 h.q trabajan en el htal materno, dde vig de benef q reclama,mov.junio15. 
           IF CONC = '01' AND POS-1-4 = 'M091'
            THEN
             IF FECHA-MES > '200404' THEN                             
                IF POS-9-10 = '40' THEN             
                   IF POS-5 = '3' OR '4' OR '5' OR '6'                           
                                      THEN               
                    IF POS-19 = 'M' THEN              
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE '*B.M091 FC HOSP.'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     COMPUTE A-HABER (I) = A-HABER (1) * 0.08.

      ******* M035 expte 21557-120073-09 sf unica vez = 5% ppto dic08 (nov08)
      ******* M035 bonificacion = 45$  para un cargo de cmh     
           IF CONC = '01' AND POS-1-4 = 'M035'
              THEN
           IF FECHA-MES > '200408'
                AND POS-5 = '9'  AND POS-7-8 = '37'              
                    THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = 45.00 * PORC / 100       
                     MOVE '*BONI.M035      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                    ELSE                                 
           IF (FECHA-MES > '200709' AND FECHA-MES < '200812')
              AND ((POS-5-6 = 'N3' OR 'S4' OR 'S1' OR 'S5')        
                  OR (POS-5-8 = 'S052' OR 'S021'))
                    THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = H-HABER * 0.08 * PORC / 100       
                     MOVE '*BONI.M035S     '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                    ELSE                                 
    ****** IF FECHA-MES = '200812' AND ((POS-5-8 = 'S050' OR 'S252') OR POS-5 = '9') cambio en movjun15 pq lo estaban cobrando...
           IF FECHA-MES = '200812' THEN                                 
                 IF (POS-5-8 = 'S050' OR 'S252') OR POS-5 = '9'
                    THEN NEXT SENTENCE                 
                    ELSE                               
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER(I) = H-HABER * 0.05 * PORC / 100       
                     MOVE '*BONI.M035      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I).

      ******* M035 expte 21557-198366-11 sf unica vez = 400 ppto jun11 (jul11), dto.879/11,mov.jul14.
           IF CONC = '01' AND POS-1-4 = 'M035'
              AND FECHA-MES = '201108'
                THEN IF POS-5 = '9'  OR  POS-5-8 = 'S252'                       
                    THEN NEXT SENTENCE
                    ELSE
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     COMPUTE A-HABER (I) = 40.00 * PORC / 100       
                     MOVE '*BONI.M035      '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I).

           IF POS-1-4 = 'M049' AND POS-5-8  NOT = 'S050'
             AND POS-5-8  NOT = 'C003' AND POS-5-8 NOT = 'C006'
             AND POS-5-8  NOT = 'C007' AND POS-5-8 NOT = 'C008'
             AND POS-5  NOT = 'O'
             THEN IF FECHA-MES < '200408'                      
                      THEN GO TO FIN-CALCULA-BONIF-SH 
                      ELSE
                       IF PORC > 00 AND 
                        (FECHA-MES < '200511'  OR
  ***********           POS-5 = 'V' OR 
  **********              POS-5-8 = 'V028' OR 
  **********            (POS-5='V' AND POS-9-10 = '35') OR
  *************          OR 'S330' 'S061' 'S117'
  *************          OR 'S252' OR 'S007'                 
                        POS-5-8 = 'S481'             
                                 OR MARCAAMP = 1)
                           THEN
                             PERFORM BON-M049 THRU BON-M049-X 
                           ELSE
                       IF PORC > 00 AND FECHA-MES > '200703'
                                    AND FECHA-MES < '200808'
                             THEN PERFORM BON1-M049 THRU BON1-M049-X 
                             ELSE
                       IF PORC > 00 AND (FECHA-MES = '200808'
                                OR '201103' OR '201108' OR '201206' 
                                OR '201306' OR '201309')
                             THEN PERFORM BON1-M049 THRU BON1-M049-X.

      *****GO TO A-LA-059.

           IF (CONC = '01' OR '02') AND POS-1-4 = 'M049'
             THEN IF FECHA-MES NOT < '200502'
                    THEN IF POS-5 = 'O'                                  
             THEN PERFORM BON-OBS-M049 THRU BON-OBS-M049-X.

   ******* A-LA-059.

           IF  POS-1-4 = 'M059' AND POS-5 = 'O'
                                AND FECHA-MES >  '200601' 
             THEN IF POS-20 = 'F'                                  
                THEN ADD 1 TO I
                        MOVE REG-MAE TO REGISTRO (I)
                        MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        COMPUTE A-HABER (I) = 100   * PORC / 100
                        MOVE '*BONI.M059O QUEB'  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)   
                   PERFORM BON-M059-OB THRU BON-M059-OB-X                        
                ELSE
                   PERFORM BON-M059-OB THRU BON-M059-OB-X.                       

       A-M110.                            
   *********    para reajuste por excepcion
           MOVE ZEROS TO M110-2961.
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M110'
                THEN IF POS-5 = 'O' OR 'S'                                  
                     THEN                    
                     IF (POS-5-8 = 'S252' OR 'S050')                     
                           AND FECHA-MES = '201012'
                           THEN
                           MOVE 115 TO M110-2961
                           ELSE
                     IF (POS-5-8 = 'S252' OR 'S050')                     
                           AND FECHA-MES = '201101'
                           THEN
                           MOVE 230 TO M110-2961
                           ELSE NEXT SENTENCE
                     ELSE
                IF FECHA-MES = '201012'                         
                         THEN
                         MOVE 115 TO M110-2961
                         ELSE
                IF FECHA-MES = '201101'                         
                         THEN
                         MOVE 230 TO M110-2961
                         ELSE
                IF FECHA-MES > '200609' AND FECHA-MES < '200611'
                         THEN
                ADD 1 TO I      
                MOVE REG-MAE TO REGISTRO (I) 
                COMPUTE A-HABER (I) = (A-HABER (1) + A-ANTIG (1))                  
                                                  * 0.10
                MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                MOVE '02'  TO A-CONC (I) 
                MOVE '*DTO.M110-2289   ' TO A-CODIGO (I) 
                          ELSE
                  IF FECHA-MES > '200610' AND FECHA-MES < '200702'
                         THEN
                ADD 1 TO I      
                MOVE REG-MAE TO REGISTRO (I) 
                COMPUTE A-HABER (I) = (A-HABER (1) + A-ANTIG (1))                  
                                                  * 0.15
                MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                MOVE '02'  TO A-CONC (I) 
                MOVE '*DTO.M110-2640   ' TO A-CODIGO (I). 
            IF M110-2961 > ZEROS 
                THEN
                ADD 1 TO I      
                MOVE REG-MAE TO REGISTRO (I) 
                COMPUTE A-HABER (I) = M110-2961 * PORC / 100                                                   
                MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                MOVE '02'  TO A-CONC (I) 
                MOVE '*DTO.M110-2961/10' TO A-CODIGO (I). 
   *********    para reajuste por excepcion
   **********
   ********** bonific 2007
           IF (CONC = '01' OR '02') AND POS-1-4 = 'M060'
             THEN IF POS-5-6 = 'TS' OR 'TT' OR 'TU' OR 'BF' OR 'BC'
                     THEN GO TO FIN-CALCULA-BONIF-SH  
                     ELSE IF FECHA-MES > '200707'
                 THEN             
               IF POS-5-8 = 'S303' OR 'SCOB' OR 'S305'
                   THEN IF FECHA-MES < '200902'
                     THEN MOVE 117.00 TO IMPORTE60
                     ELSE IF FECHA-MES = '200902'
                           THEN MOVE  58.50 TO IMPORTE60
                           ELSE IF FECHA-MES < '200906'
                             THEN MOVE 29.25  TO IMPORTE60
                             ELSE IF FECHA-MES = '201205'
                              THEN MOVE 200    TO IMPORTE60
                              ELSE MOVE ZEROS  TO IMPORTE60
                   ELSE
                    IF POS-5-8 = 'S141' OR 'S350' OR 'N120'
                   THEN IF FECHA-MES < '200902'
                     THEN MOVE 99.00 TO IMPORTE60
                     ELSE IF FECHA-MES = '200902'
                           THEN MOVE 49.50 TO IMPORTE60
                           ELSE IF FECHA-MES < '200906'
                               THEN MOVE 24.75 TO IMPORTE60
                               ELSE IF FECHA-MES = '201205'
                                THEN MOVE 200    TO IMPORTE60
                                ELSE MOVE ZEROS TO IMPORTE60
                   ELSE
                        IF POS-5-8 = 'N219' OR 'S142'
                            THEN IF FECHA-MES < '200902'
                              THEN MOVE 95.00 TO IMPORTE60
                              ELSE IF FECHA-MES = '200902'
                                THEN MOVE 47.50 TO IMPORTE60
                                ELSE IF FECHA-MES < '200906'
                                   THEN MOVE 23.75 TO IMPORTE60
                                   ELSE IF FECHA-MES = '201205'
                                    THEN MOVE 200    TO IMPORTE60
                                    ELSE MOVE ZEROS TO IMPORTE60
                            ELSE
                           IF POS-5-8 = 'S474' OR 'N316' OR 'S335'
                                     OR 'S027'
                            THEN IF FECHA-MES < '200902'
                               THEN MOVE 85.00 TO IMPORTE60
                               ELSE IF FECHA-MES = '200902'
                                    THEN  MOVE 42.50 TO IMPORTE60
                                    ELSE IF FECHA-MES < '200906'
                                      THEN  MOVE 21.25 TO IMPORTE60
                                      ELSE IF FECHA-MES = '201205'
                                        THEN MOVE 200    TO IMPORTE60
                                        ELSE  MOVE ZEROS TO IMPORTE60
                            ELSE
                             IF  POS-5-8 = 'S414'
                              THEN IF FECHA-MES < '200902'
                                 THEN MOVE 80.00 TO IMPORTE60
                                 ELSE IF FECHA-MES = '200902'
                                    THEN MOVE 40.00 TO IMPORTE60
                                    ELSE IF FECHA-MES < '200906'
                                      THEN MOVE 20.00 TO IMPORTE60
                                      ELSE IF FECHA-MES = '201205'
                                        THEN MOVE 200    TO IMPORTE60
                                        ELSE MOVE ZEROS TO IMPORTE60
                              ELSE
                               IF POS-5-8 = 'TE24' OR POS-5 = 'N'
                                 THEN IF FECHA-MES < '200904'
                                   THEN MOVE 69.70 TO IMPORTE60
                                   ELSE IF FECHA-MES < '200907'
                                     THEN MOVE 74.93 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201007'
                                       THEN MOVE 80.16 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201009'
                                        THEN  MOVE 88.18 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201104'
                                        THEN MOVE 96.19 TO IMPORTE60
                                        ELSE IF FECHA-MES < '201204'
                                         THEN MOVE 121.2 TO IMPORTE60
                                         ELSE IF FECHA-MES = '201205'
                                      THEN MOVE 333.32 TO IMPORTE60
                                         ELSE IF FECHA-MES < '201207'
                                      THEN MOVE 133.32 TO IMPORTE60
                                         ELSE IF FECHA-MES < '201209'
                                      THEN MOVE 138.17 TO IMPORTE60
                                         ELSE IF FECHA-MES < '201304'
                                      THEN MOVE 150.29 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201309'
                                       THEN MOVE 165.32 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201404'
                                     THEN MOVE 184.86 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201407'
                                      THEN MOVE 203.35 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201408'
                                     THEN MOVE 221.83 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201504'
                                      THEN MOVE 234.77 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201507'
                                     THEN MOVE 258.25 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201508'
                                      THEN MOVE 281.72 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201511'
                                     THEN MOVE 305.20 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201604'
                                       THEN MOVE 152.60 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201607'
                                         THEN MOVE 167.86 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201608'
                                         THEN MOVE 183.12 TO IMPORTE60
                                         ELSE MOVE 196.85 TO IMPORTE60
                                 ELSE
                                  IF POS-5 = 'H' OR 'E' OR 'B'
                                   THEN IF FECHA-MES < '200904'
                                    THEN MOVE 85.00 TO IMPORTE60
                                    ELSE IF FECHA-MES < '200907'
                                      THEN MOVE 91.38 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201101'
                                       THEN MOVE 97.75 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201104'
                                       THEN MOVE 124.2 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201204'
                                        THEN MOVE 147.8 TO IMPORTE60
                                        ELSE IF FECHA-MES = '201205'
                                      THEN MOVE 362.58 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201207'
                                        THEN MOVE 162.58 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201209'
                                        THEN MOVE 168.49 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201304'
                                        THEN MOVE 183.27 TO IMPORTE60
                                        ELSE IF FECHA-MES < '201309'
                                      THEN MOVE 201.60 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201404'
                                    THEN MOVE 225.42 TO IMPORTE60
                                    ELSE IF FECHA-MES < '201407'
                                      THEN MOVE 247.96 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201408'
                                     THEN MOVE 270.51 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201504'
                                      THEN MOVE 286.29 TO IMPORTE60
                                      ELSE IF FECHA-MES < '201507'
                                     THEN MOVE 314.92 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201508'
                                    THEN MOVE 343.55 TO IMPORTE60
                                    ELSE IF FECHA-MES < '201511'
                                     THEN MOVE 372.18 TO IMPORTE60
                                     ELSE IF FECHA-MES < '201604'
                                       THEN MOVE 186.09 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201607'
                                        THEN MOVE 204.70 TO IMPORTE60
                                       ELSE IF FECHA-MES < '201608'
                                        THEN MOVE 223.31 TO IMPORTE60
                                        ELSE MOVE 240.06 TO IMPORTE60
                                   ELSE
                                  IF POS-5 = '7' OR 'M'         
                                   THEN IF FECHA-MES = '201205'
                                        THEN MOVE 200    TO IMPORTE60
                                        ELSE MOVE ZEROS TO IMPORTE60 
                                   ELSE MOVE ZEROS TO IMPORTE60 
                                        GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH. 
       VERIMPOR060.
           IF IMPORTE60 > ZEROS AND PORC > ZEROS
                          THEN
                    ADD 1 TO I      
                    MOVE REG-MAE TO REGISTRO (I) 
                    MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                    MOVE '02'  TO A-CONC (I) 
                    MOVE '*ORD. 10325/07   ' TO A-CODIGO (I) 
                    COMPUTE A-HABER (I) = IMPORTE60 * PORC / 100 
                    GO TO FIN-CALCULA-BONIF-SH. 
    ***GO TO SEGUIR-A.                  


            IF (CONC = '01' OR '02') AND POS-1-4 = 'M068'
             THEN
                 IF FECHA-MES > '201201' AND POS-5 = 'S' 
                    AND NOT (POS-5-8 = 'S103' OR 'S303' OR
                                      'S309' OR 'S304')  
                   THEN GO TO SEGUIR-A 
                   ELSE
      *          IF FECHA-MES > '200809'
                 IF FECHA-MES > '200801'
                    THEN
      *               IF POS-5-8 = 'S050' OR POS-5 = '9'
                      IF POS-5-8 = 'S050'                   
                           THEN GO TO FIN-CALCULA-BONIF-SH
                           ELSE NEXT SENTENCE
                    ELSE
      *               IF FECHA-MES > '200801'
      *                 THEN
      *               IF POS-5-8 = 'S463' OR 'S485' OR 'S103'
      *                           OR 'S303' OR 'S304'
      *                       THEN NEXT SENTENCE
      *                       ELSE
                               IF POS-5 = 'S'
                           THEN GO TO CALCULA-NVA-068 
                           ELSE NEXT SENTENCE
      *                 ELSE GO TO FIN-CALCULA-BONIF-SH
              ELSE GO TO SEGUIR-A.                 

      *     GO TO FIN-CALCULA-BONIF-SH.

            IF FECHA-MES > '200908' AND POS-5 = '9'
                         THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE  A-HABER (I) = 250 * PORC / 100
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*ORD.12859     ' TO A-CODIGO (I)  
                  GO TO FIN-CALCULA-BONIF-SH
                         ELSE
            IF FECHA-MES > '201007' AND POS-5 NOT = '9'
                         THEN
                    COMPUTE IMPORTE68 = SUMAFIJ2                                  
                    COMPUTE IMPORTE68 = IMPORTE68 * PORC / 100 
                         ELSE
            IF FECHA-MES > '200809' AND POS-5 NOT = '9'
                         THEN
                    COMPUTE IMPORTE68 = SUMAFIJ2 * 0.20                           
                    COMPUTE IMPORTE68 = IMPORTE68 * PORC / 100 
                         ELSE
                    COMPUTE IMPORTE68 =  A-HABER (1) * 0.20.                          

            IF IMPORTE68 NOT > ZEROS THEN NEXT SENTENCE
                                     ELSE               
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE IMPORTE68 TO A-HABER (I)
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BON.M068 SF.  ' TO A-CODIGO (I). 
     ***********  MOVE '*DTO.201/08    ' TO A-CODIGO (I).mov may14 le cambio el nombre pq no esta vigente este dto.

       CALCULA-NVA-068.          
            IF PORC = ZEROS                        
                            THEN GO TO SEGUIR-AA.
            MOVE 0 TO SF68 SF68-1 SF68-2.
            IF FECHA-MES > '201008'                           
                         THEN PERFORM 068-ENE09 THRU 068-ENE09-X
                         VARYING T FROM 1 BY 1 UNTIL T > 7 AFTER  
                         X FROM 1 BY 1 UNTIL X > 10 
                         ELSE
            IF FECHA-MES > '200901'
                         THEN PERFORM 068-ENE09 THRU 068-ENE09-X
                         VARYING T FROM 1 BY 1 UNTIL T > 18 AFTER  
                         X FROM 1 BY 1 UNTIL X > 10.
              IF SF68 > 0 THEN 
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = SF68 * PORC / 100
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.ENE/09      ' TO A-CODIGO (I).  

            IF FECHA-MES > '201001'                         
                    THEN PERFORM 068-ENE09 THRU 068-ENE09-X
                         VARYING T FROM 1 BY 1 UNTIL T > 18 AFTER  
                         X FROM 1 BY 1 UNTIL X > 10 
                         IF  SF68-1 > ZEROS
                         THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = SF68-1 * PORC / 100
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.ENE/10      ' TO A-CODIGO (I)  
                  GO TO FIN-CALCULA-BONIF-SH.

            IF FECHA-MES > '201106'                         
                    THEN PERFORM 068-ENE09 THRU 068-ENE09-X
                         VARYING T FROM 8 BY 1 UNTIL T > 18 AFTER  
                         X FROM 1 BY 1 UNTIL X > 10 
                         IF  SF68-2 > ZEROS
                         THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = SF68-2 * PORC / 100
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.JUN/11      ' TO A-CODIGO (I). 


       SEGUIR-A.                 
 ************* agrego el 11-6-12 Dto 743/12. Dado de baja en ene13 por dto 211/13
            IF FECHA-MES > '201203' AND FECHA-MES < '201302'              
                                    AND POS-1-4 = 'M068'              
                 THEN
                 IF (CONC = '01' AND EME = ' ') OR          
                    (CONC = '02' AND PORC NOT = ZEROS)                                
                      THEN
                      IF (POS-5 = 'C' OR '2' OR '3' OR '4'                         
                                      OR '5' OR '6' OR '7')
                      OR (POS-5-8 = 'S304' OR 'S303' OR 'S309' 
                                           OR 'S103') 
                             THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = SUMAFIJ2 * PORC / 100
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.M068-MAR12  ' TO A-CODIGO (I)  
                  GO TO FIN-CALCULA-BONIF-SH 
                            ELSE GO TO FIN-CALCULA-BONIF-SH 
                      ELSE GO TO FIN-CALCULA-BONIF-SH.

       SEGUIR-AA.                 
    ***** expte 2350-102630-01 de un jub, autoriz pago consultado en expte 21557-44596-06
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M106'
             THEN
                 IF FECHA-MES > '200607' AND
                    FECHA-MES < '200702'
                    THEN
                      IF (POS-5-8 = 'S252' OR '6024' OR 'JO24'             
                          OR '4024') OR POS-5 = '9'
                           THEN GO TO FIN-CALCULA-BONIF-SH
                           ELSE                  
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = 70 * PORC / 100      
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.846/06      ' TO A-CODIGO (I).  

            IF (CONC = '01' OR '02') AND POS-1-4 = 'M106'
             THEN
                 IF POS-5-8 = 'S252' OR 'S050' OR 'RRRR'                       
                         THEN GO TO FIN-CALCULA-BONIF-SH
                         ELSE
                 IF FECHA-MES = '201002' OR '201003' OR
    ******    '201011' OR '201012' OR '201102' OR FECHA-MES > '201201' vino autor el expte 21557-190329-11 y cambio vigencia
                         '201011' OR '201012' OR (FECHA-MES > '201101'          
                          AND FECHA-MES < '201411')
                    THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                 IF FECHA-MES = '201002' OR '201003'    
                  THEN COMPUTE A-HABER (I) = 150 * PORC / 100      
                       MOVE '*DTO.M106-442/10 ' TO A-CODIGO (I)  
                  ELSE
                 IF FECHA-MES = '201011' OR '201012'               
      *********  IF FECHA-MES = '201011' OR '201012' OR '201102'
                    THEN
                  COMPUTE A-HABER (I) = 125 * PORC / 100      
                  MOVE '*DTO.M106-442/10 ' TO A-CODIGO (I)  
                    ELSE
      *********   MOVE '*DTO.M106 ENE12  ' TO A-CODIGO (I)  
                  MOVE '*DTO.M106 ENE11  ' TO A-CODIGO (I)  
                  IF POS-5 = '9' AND                                              
                             (POS-6 = 'E' OR 'B' OR 'H' OR 'I' OR 'K')
                                  THEN
                  COMPUTE A-HABER (I) =  96 * PORC / 100      
                                  ELSE
                  COMPUTE A-HABER (I) = 555 * PORC / 100      
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** dde mov.feb10 paso ref=130 y sf=200 aca para que no los tome el pagamun en la bonif %
    ***** dde mov.feb10 paso ref=130 y sf=200 aca para que no los tome el pagamun en la bonif %
    *****  mov.JUN11 pptos. feb-mar-abr-may11 los 200 (Dto 1612/09)los van pasando de a 50 al basico.                     
    ********IF (CONC = '01' OR '02') AND POS-1-4 = 'M008' lo saco en mov oct11
            IF CONC = '01' AND POS-1-4 = 'M008'
             THEN
              IF PORC = ZEROS                                      
                  THEN GO TO FIN-CALCULA-BONIF-SH
                  ELSE
              IF FECHA-MES > '201002'                                   
                                 THEN IF POS-5-8 = 'S050' OR                              
                        (POS-5-6 = '9H' OR '9I' OR '9B' OR '9K' 
                                   OR '9G')
                         OR (POS-5-7 = '9E2' OR '9E3' OR '9E4')
                           THEN GO TO VERMAS-M008                  
                           ELSE                  
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI M008       ' TO A-CODIGO (I)  
                  IF POS-5-8 = 'S463' OR 'S454' OR 'S403'                             
                        THEN 
                COMPUTE A-HABER (I) = (130 + 120) * PORC / 100      
                        ELSE                  
             IF FECHA-MES > '201105' THEN MOVE ZEROS TO SUM008         
                                          GO TO CALCU008
                                     ELSE
             IF FECHA-MES = '201105' THEN MOVE 50 TO SUM008         
                                          GO TO CALCU008
                                     ELSE
             IF FECHA-MES = '201104' THEN MOVE 100 TO SUM008         
                                          GO TO CALCU008
                                     ELSE
             IF FECHA-MES = '201103' THEN MOVE 150 TO SUM008         
                                          GO TO CALCU008
                                     ELSE MOVE 200 TO SUM008 
                                          GO TO CALCU008
                             ELSE GO TO SIGOM008 
            ELSE GO TO ALAM075.
       CALCU008.                                                                  
                IF POS-5 = 'S'                                                
                     THEN 
                COMPUTE A-HABER (I) = (SUM008 + 120) * PORC / 100      
                     ELSE                  
                COMPUTE A-HABER (I) = (SUM008 * POS-9-10N / 30 + 120) *                 
                                       PORC / 100.      
       SIGOM008.
    ***** dde mov.ENE10  a dic10 dto 1272/10 y dde ene11 dto 744/11 lo modifica
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M008'
                                     AND FECHA-MES < '201501'
             THEN
              IF PORC = ZEROS                                      
                  THEN GO TO FIN-CALCULA-BONIF-SH
                  ELSE
                     IF (POS-5-6 = '9H' OR '9I' OR '9B' OR '9K')
                         OR (POS-5-7 = '9E2' OR '9E3' OR '9E4')
                OR (POS-5-8 = 'S050' OR 'S463' OR 'S454' OR 'S403')                             
                           THEN GO TO VERMAS-M008                   
                           ELSE                  
              IF FECHA-MES > '201108' AND POS-5 = 'S'                                                                      
                         THEN GO TO MASM008                        
                         ELSE                  
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI M008-1272/10 ' TO A-CODIGO (I)  
              IF FECHA-MES > '201001' AND FECHA-MES < '201009'          
                     THEN                                                     
                COMPUTE A-HABER (I) = 300 * PORC / 100      
                     ELSE                  
              IF FECHA-MES > '201008' AND FECHA-MES < '201011'          
                     THEN                                                     
                COMPUTE A-HABER (I) = 250 * PORC / 100      
                     ELSE                  
              IF FECHA-MES = '201011'                                             
                     THEN                                                     
                COMPUTE A-HABER (I) = 200 * PORC / 100      
                     ELSE                  
              IF FECHA-MES = '201012'                                             
                     THEN                                                     
                COMPUTE A-HABER (I) = 150 * PORC / 100      
                     ELSE                  
              IF FECHA-MES = '201012'                                             
                     THEN                                                     
                COMPUTE A-HABER (I) = 100 * PORC / 100      
                     ELSE                  
              IF FECHA-MES > '201101' AND FECHA-MES < '201109'                                             
                     THEN                                                     
                COMPUTE A-HABER (I) = 200 * PORC / 100      
                     ELSE                  
              IF FECHA-MES = '201109'                                                                      
                     THEN                                                     
                COMPUTE A-HABER (I) = 150 * PORC / 100      
                     ELSE                  
              IF FECHA-MES > '201109' AND FECHA-MES < '201201'                                             
                     THEN                                                     
                COMPUTE A-HABER (I) = 100 * PORC / 100      
                     ELSE                  
              IF FECHA-MES > '201112'                                                                      
                     THEN                                                     
                COMPUTE A-HABER (I) =  50 * PORC / 100      
                     ELSE GO TO FIN-CALCULA-BONIF-SH.          
************** ELSE GO TO FIN-CALCULA-BONIF-SH.          

       VERMAS-M008.
**************   se agrega en pto Set/14
            IF CONC = '01'  AND POS-1-4 = 'M008'
             THEN
                 IF FECHA-MES < '201103' OR FECHA-MES > '201412'                                                                      
                 THEN GO TO FIN-CALCULA-BONIF-SH
                 ELSE
                 IF (POS-5-8 = 'S050' OR 'S463' OR 'S454' OR 'S403'                              
                                     OR 'S252')
                     OR (POS-5 = 'B')
                           THEN GO TO MASM008                           
                           ELSE
                   IF (POS-5-6 = '9H' OR '9I' OR '9B' OR '9K' 
                                     OR '9E')
                   THEN                  
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI M008-1744/11 ' TO A-CODIGO (I)  
                  IF  FECHA-MES < '201109'                                                             
                         THEN
                  COMPUTE A-HABER (I) = 350 * PORC / 100      
                         ELSE   
                  IF FECHA-MES = '201109'                                   
                         THEN                                                     
                  COMPUTE A-HABER (I) = 300 * PORC / 100      
                         ELSE   
                  IF FECHA-MES = '201110'                                   
                         THEN                                                     
                  COMPUTE A-HABER (I) = 250 * PORC / 100      
                         ELSE                                                     
                  COMPUTE A-HABER (I) = 200 * PORC / 100      
                   ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI M008-744/11 ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = (450 * POS-9-10N / 30 ) *                 
                                       PORC / 100.      

       MASM008.
**************   se agrega en  Oct/14 para pto Oct/12
            IF CONC = '01'  AND POS-1-4 = 'M008'
             THEN
                 IF FECHA-MES < '201211'
                 THEN GO TO FIN-CALCULA-BONIF-SH
                 ELSE
                 IF POS-5 = 'S'                                     
                   THEN
                       IF POS-17-18 IS NUMERIC AND POS-17-18 > ZEROS
                            THEN
                       ADD 1 TO I
                       MOVE REG-MAE TO REGISTRO (I)
                       MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I)
                       MOVE '02'  TO A-CONC (I)
                       MOVE '*BON.M008-DEDIC.EX.' TO A-CODIGO (I)
                  COMPUTE A-HABER (I) = (H-HABER  * POS-17-18N / 100)
                                                  * PORC / 100 
                       GO TO FIN-CALCULA-BONIF-SH
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                   ELSE GO TO FIN-CALCULA-BONIF-SH.


    ***** dde mov.Jul07  a jun08 300$ no remun
       ALAM075.
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M075'
             THEN
              IF PORC = ZEROS
                  THEN GO TO FIN-CALCULA-BONIF-SH
                  ELSE
              IF FECHA-MES > '200707' AND FECHA-MES < '200810' 
                   THEN
*************   para que no pague aguinaldo
              IF FECHA-MES = '200801' AND FECHA-MES = '200807'
                     THEN GO TO FIN-CALCULA-BONIF-SH 
                     ELSE 
                     IF (POS-5 = '2' OR '3' OR '4' OR '5' OR '6')
                     OR (POS-5-8 = 'S303' OR 'S304')
                           THEN
                  ADD 1 TO I      
                  MOVE REG-MAE TO REGISTRO (I) 
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I)
                  MOVE '02'  TO A-CONC (I)
                COMPUTE A-HABER (I) = 100 * PORC / 100
                  MOVE '*BONI M075-558/07  ' TO A-CODIGO (I)
                          ELSE GO TO FIN-CALCULA-BONIF-SH 
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

   ***** dde mov.ENE08  a Dic09 200$ y 260$ no remun
   ***** expte 21557-75421-07 99,93 may07 a nov07 dto.722/07.
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M050'
             THEN
              IF PORC = ZEROS
                  THEN GO TO FIN-CALCULA-BONIF-SH
                  ELSE
              IF (POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6')
                     OR (POS-5-8 = 'S303' OR 'S304' OR 'S479')
                           THEN
              IF FECHA-MES > '200705' AND FECHA-MES < '200805'
                                THEN
                  ADD 1 TO I
                  MOVE REG-MAE TO REGISTRO (I)
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I)
                  MOVE '02'  TO A-CONC (I)
              IF FECHA-MES < '200712'                             
                THEN COMPUTE A-HABER (I) = 99.93 * PORC / 100
                     MOVE '*BONI M050-722/07  ' TO A-CODIGO (I)
                ELSE
                COMPUTE A-HABER (I) = 200 * PORC / 100
                  MOVE '*BONI M050-794/08  ' TO A-CODIGO (I)
                                ELSE
              IF FECHA-MES > '200804' AND FECHA-MES < '201006'
                                THEN
              IF (FECHA-MES = '200807' OR '200901' OR '200907'
                                                OR '201001')
                               THEN GO TO FIN-CALCULA-BONIF-SH
                               ELSE
                  ADD 1 TO I
                  MOVE REG-MAE TO REGISTRO (I)
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I)
                  MOVE '02'  TO A-CONC (I)
                COMPUTE A-HABER (I) = 260 * PORC / 100
                  MOVE '*BONI M050-794/08  ' TO A-CODIGO (I)
                               ELSE GO TO FIN-CALCULA-BONIF-SH
                           ELSE GO TO FIN-CALCULA-BONIF-SH.


    ***** expte 21557-142138-09                                                            
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M136'
             THEN
                 IF FECHA-MES > '200906' AND
                    FECHA-MES < '201002'
                    THEN
                      IF (POS-5-8 = 'S303' OR 'S304' OR 'S316'             
                          OR 'S317') OR (POS-5 = '9' OR '2'
                          OR '3' OR '4' OR '5' OR '6' OR 'I')
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*DTO.390/09 M136 ' TO A-CODIGO (I)  
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** agre bonif en mayo 11 para reaj adic..para descontar de deuda calcula anteriorm.      
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M119'
             THEN
               IF FECHA-MES > '200801' AND FECHA-MES < '200807'
                    THEN
                      IF (POS-5-8 = 'S252' OR 'S050')                      
                        THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M119        ' TO A-CODIGO (I)  
                  IF POS-5-8 = 'S252'                                 
                     THEN COMPUTE A-HABER (I) = 265 * 16 * PORC / 100      
                     ELSE COMPUTE A-HABER (I) = 265 * 5 * PORC / 100.     

    ***** exptes 21557-142142-09 y 21557-150838-10  oct09 (200) y ene10                               
    ***** exptes 21557-101692-08 mayo (100) y junio 08 (200)                                          
    ***** ppto jul11 la boni pasa a remunerativa y no es ppnal, y doy alta a otra de 300.                  
    ***** mov.feb 2014 agr 'Z' en la excepcion por indic del 3ro seg recibo, reaj dde enero 2011.               
    ***** mov.MAY 2014 saco sf=300 a algs S y C del hcd, en prox aum de S mpales sacarla para esos pq es hta jefe dpto.
    ***** mov.AGO 2014 saco sf=300 a algs S de mdad, pq es hta jefe dpto.
    ***** mov.AGO 2015 creo variables M119FIJA y M119REM  M119REMJ                   

 *************** agregado agosto 2015
            MOVE ZEROS TO M119FIJA M119REM M119REMJ.
            IF FECHA-MES > '201610'     
                           THEN MOVE 1335.75 TO M119FIJA
                                MOVE 1781   TO M119REMJ 
                                MOVE 2137.2 TO M119REM 
                           ELSE
            IF FECHA-MES > '201605'     
                           THEN MOVE 1181.63 TO M119FIJA
                                MOVE 1575.5 TO M119REMJ 
                                MOVE 1890.6 TO M119REM 
                           ELSE
            IF FECHA-MES > '201510'     
                           THEN MOVE 1027.50 TO M119FIJA
                                MOVE 1370 TO M119REMJ 
                                MOVE 1644 TO M119REM 
                           ELSE
            IF FECHA-MES > '201507'     
                           THEN MOVE 937.50 TO M119FIJA
                                MOVE 1250 TO M119REMJ 
                                MOVE 1500 TO M119REM 
                           ELSE
            IF FECHA-MES > '201505'     
                           THEN MOVE 847.50 TO M119FIJA
                                MOVE 1130 TO M119REMJ 
                                MOVE 1356 TO M119REM 
                           ELSE
            IF FECHA-MES > '201405'     
                          THEN  MOVE 750 TO M119FIJA
                                MOVE 1000 TO M119REMJ 
                                MOVE 1200 TO M119REM.
 *************** agregado agosto 2015
            IF (CONC = '01' AND POS-1-4 = 'M119') OR
                 (CONC = '02' AND POS-1-4 = 'M119' AND PORC > '00')
             THEN
                 IF FECHA-MES > '200805'     
                 THEN
                      IF (POS-5-8 = 'S252' OR 'S050')                      
                           OR (POS-5 = 'T' OR 'F' OR 'B'
                        OR 'E' OR 'H' OR 'I' OR 'K' OR 'Z')          
                           THEN  NEXT SENTENCE              
                           ELSE
                      ADD 1 TO I       
                      MOVE REG-MAE TO REGISTRO (I)  
                      MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                      MOVE '02'  TO A-CONC (I) 
                      MOVE '*BONI.M119      ' TO A-CODIGO (I)  
                  IF FECHA-MES > '201405'     
                  THEN
                         IF POS-5-8 = 'S103' OR 'S1S3' OR 'S4S9'            
                                   OR 'S479' OR 'S121' OR 'S123'
                            THEN                                                  
                         COMPUTE A-HABER(I) = M119FIJA * PORC / 100      
                            ELSE
                       IF POS-5-8 =  'S303' OR 'S3S3' OR 'S3D3'  
                             THEN                                                                
                             COMPUTE A-HABER(I) = (M119FIJA + M119REMJ)                     
                                                          * PORC / 100      
                             ELSE
                       IF POS-5-8 =  '1001'                 
                        THEN
                        COMPUTE A-HABER (I) = ((M119FIJA * POS-9-10N                         
                                        / 30) + M119REMJ) * PORC / 100      
                        ELSE
                       IF POS-5-8 =  'S304' OR 'S3S4' OR 'S153'             
                            THEN
                            COMPUTE A-HABER (I) = (M119FIJA + M119REM)                              
                                                          * PORC / 100                 
                            ELSE
                       IF POS-5 = '1' OR '2' OR '3' OR '4' OR 'M'                           
                                                    OR '5' OR '6'
                            THEN  COMPUTE A-HABER (I) = ((M119FIJA                                     
                              * POS-9-10N / 30) + M119REM) * PORC / 100      
                            ELSE
                         IF POS-5-8 = 'S125' OR 'S122' OR 'S489' OR       
                            'S454' OR 'S485' OR 'C485' OR 'S4F5' OR                            
                            'S463' OR 'C463' OR 'S317'                                           
                      THEN COMPUTE A-HABER(I) = 600 * PORC / 100 
                      ELSE IF POS-5-8 = 'C601' OR 'C302' OR 'C402' OR
                                        'C301' OR 'C602'
                           THEN COMPUTE A-HABER(I) = (600 + 300)                  
                                                      * PORC / 100
                           ELSE COMPUTE A-HABER(I) = 0
  ************* ELSE COMPUTE A-HABER(I)=(600 + 300)*PORC/100 saco los 300 pq son hta jefe de depto.y los 600 dice en expte hta dtor.
                  ELSE
                  IF FECHA-MES > '201309'     
                  THEN IF POS-5-8 = 'S103' OR 'S1S3'                               
                    THEN COMPUTE A-HABER(I) = 750 * PORC / 100      
                    ELSE
                     IF POS-5-8 =  'S303' OR 'S3S3' OR 'S3D3' OR 'S4S9'
                   THEN COMPUTE A-HABER(I) = (750 + 625) * PORC / 100      
                   ELSE
                       IF POS-5-8 =  '1001' THEN                 
                          COMPUTE A-HABER (I) = ((750 * POS-9-10N / 30)                   
                                                   + 625) * PORC / 100      
                                            ELSE
                       IF POS-5-8 =  'S304' OR 'S3S4' THEN       
                          COMPUTE A-HABER(I) = (750 + 875) * PORC / 100                 
                                                      ELSE
                IF POS-5 = '1' OR '2' OR '3' OR '4' OR 'M' OR '5' OR '6'
                    THEN  COMPUTE A-HABER (I) = ((750 * POS-9-10N / 30)                   
                                                   + 875) * PORC / 100      
                    ELSE IF POS-5-8 = 'S125' OR 'S122' OR 'S489' OR       
                'S454' OR 'S485' OR 'C485' OR 'S4F5' OR 'S463' OR 'C463'
                      THEN COMPUTE A-HABER(I) = 600 * PORC / 100 
                      ELSE COMPUTE A-HABER(I) = (600 + 300) * PORC / 100 
                   ELSE

                  IF FECHA-MES > '201307'     
                  THEN IF POS-5-8 = 'S103' OR 'S1S3'                      
                    THEN COMPUTE A-HABER(I) = 675 * PORC / 100      
                    ELSE
                      IF POS-5-8 =  'S303' OR 'S3S3' OR 'S3D3' OR 'S4S9'  
                    THEN COMPUTE A-HABER(I) = (675 + 562.5) * PORC / 100      
                    ELSE
                       IF POS-5-8 =  '1001' THEN                 
                          COMPUTE A-HABER (I) = ((675 * POS-9-10N / 30)                   
                                                   + 562.5) * PORC / 100      
                                            ELSE
                       IF POS-5-8 =  'S304' OR 'S3S4' THEN       
                          COMPUTE A-HABER (I) = (675                                     
                                                   + 787.5) * PORC / 100      
                                                      ELSE
                IF POS-5 = '1' OR '2' OR '3' OR '4' OR 'M' OR '5' OR '6'
                    THEN  COMPUTE A-HABER (I) = ((675 * POS-9-10N / 30)                   
                                                   + 787.5) * PORC / 100      
                    ELSE IF POS-5-8 = 'S125' OR 'S122' OR 'S489' OR       
                'S454' OR 'S485' OR 'C485' OR 'S4F5' OR 'S463' OR 'C463'
                      THEN COMPUTE A-HABER(I) = 600 * PORC / 100 
                      ELSE COMPUTE A-HABER(I) = (600 + 300) * PORC / 100 

                   ELSE
                  IF FECHA-MES > '201207'     
                  THEN IF POS-5-8 = 'S103' OR 'S1S3'                      
                    THEN COMPUTE A-HABER(I) = 600 * PORC / 100      
                    ELSE
                      IF POS-5-8 = 'S303' OR 'S3S3' OR 'S3D3' OR 'S4S9'
                     THEN COMPUTE A-HABER(I) = (600 + 500) * PORC / 100      
                     ELSE
                       IF POS-5-8 =  '1001' THEN                 
                          COMPUTE A-HABER (I) = ((600 * POS-9-10N / 30)                   
                                                   + 500) * PORC / 100      
                                            ELSE
                       IF POS-5-8 =  'S304' OR 'S3S4'            
                      THEN COMPUTE A-HABER(I) = (600 + 700) * PORC / 100      
                      ELSE IF POS-5 = '1' OR '2' OR '3' OR '4' OR 'M'
                                          OR '5' OR '6'                                  
                       THEN COMPUTE A-HABER(I) = ((600 * POS-9-10N / 30)     
                                                     + 700) * PORC / 100      
                       ELSE IF POS-5-8 = 'S125' OR 'S122' OR 'S489' OR       
                'S454' OR 'S485' OR 'C485' OR 'S4F5' OR 'S463' OR 'C463'
                      THEN COMPUTE A-HABER(I) = 600 * PORC / 100 
                      ELSE COMPUTE A-HABER(I) = (600 + 300) * PORC / 100 
                   ELSE
                  IF FECHA-MES > '201107'     
                    THEN IF POS-5-8 = 'S125' OR 'S122' OR 'S489' OR       
                'S454' OR 'S485' OR 'C485' OR 'S4F5' OR 'S463' OR 'C463'
                     THEN COMPUTE A-HABER(I) = 600 * PORC / 100 
                     ELSE IF POS-9-10 = '00'                                 
                      THEN COMPUTE A-HABER(I) = (600 + 300) * PORC / 100      
                      ELSE COMPUTE A-HABER(I) = ((600 * POS-9-10N / 30)      
                                                     + 300) * PORC / 100      
                    ELSE                                             
                  IF FECHA-MES > '201001'     
                       THEN
                    IF POS-9-10N < 31
                     THEN COMPUTE A-HABER (I) = 400 * PORC / 100      
                     ELSE                                             
             COMPUTE A-HABER (I) = 400 * POS-9-10N / 30 * PORC / 100      
                       ELSE
                  IF FECHA-MES > '200806'     
                   THEN
                     IF POS-9-10N < 31
                       THEN
                  COMPUTE A-HABER (I) = 200 * PORC / 100      
                       ELSE                                             
             COMPUTE A-HABER (I) = 200 * POS-9-10N / 30 * PORC / 100      
                   ELSE
                     IF POS-9-10N < 31
                       THEN
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                       ELSE                                             
             COMPUTE A-HABER (I) = 100 * POS-9-10N / 30 * PORC / 100.      

**************************** agrego 24-9-13. LO SACO EN MOV MAR14 por indicacion del 3ro, esta bon no va p/HS fcionales.
      ****   IF HSFUNQ > ZEROS
      ****          THEN COMPUTE HSFUNQ = HSFUNQ + A-HABER (I) / 30 * 15 
      ****               ADD 1 TO I       
      ****               MOVE REG-MAE TO REGISTRO (I)  
      ***                MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
      *                  MOVE '02'  TO A-CONC (I) 
      *                  MOVE '*M119 HSFUNCQ    ' TO A-CODIGO (I)  
      *                  MOVE HSFUNQ TO A-HABER (I)
      *             ELSE
      *      IF HSFUNK > ZEROS
      *             THEN COMPUTE HSFUNK = HSFUNK + A-HABER (I) / 30 * 18  
      *                  ADD 1 TO I       
      *                  MOVE REG-MAE TO REGISTRO (I)  
      *                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
      *                  MOVE '02'  TO A-CONC (I) 
      *                  MOVE HSFUNK TO A-HABER (I)
      *                  MOVE '*M119 HSFUNCK    ' TO A-CODIGO (I). 
****************************

**************************** hasta feb14 estaba en pagamun, dde mar14 la paso aca pq se usa p/hs funcionales.
           IF POS-1-4 = 'M119' AND (( POS-5 = '1' OR '2' OR '3'
                   OR '4' OR 'M' OR '5' OR '6' ) OR
                   (POS-5-8 ='S304' OR 'S303' OR 'S3S3' OR 'S3D3'
                     OR 'S3S4')) AND CONC = '01'
              THEN IF (FECHA-MES > '200707' AND FECHA-MES < '201506')
                   THEN  ADD 1 TO I       
                         MOVE REG-MAE TO REGISTRO (I)  
                         MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                         MOVE '02'  TO A-CONC (I) 
                         COMPUTE A-HABER (I) = 250.00 * PORC / 100 
                         MOVE '*DTO2876/07     ' TO A-CODIGO (I).
**************************** agrego 17-3-14.                                                                              
             IF POS-1-4 = 'M119' AND HSFUNQ > ZEROS
                    THEN COMPUTE HSFUNQ = HSFUNQ + A-HABER (I) / 30 * 15 
                         ADD 1 TO I       
                         MOVE REG-MAE TO REGISTRO (I)  
                         MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                         MOVE '02'  TO A-CONC (I) 
                         MOVE '*M119 HSFUNCQ    ' TO A-CODIGO (I)  
                         MOVE HSFUNQ TO A-HABER (I)
                    ELSE
             IF POS-1-4 = 'M119' AND HSFUNK > ZEROS
                    THEN COMPUTE HSFUNK = HSFUNK + A-HABER (I) / 30 * 18  
                         ADD 1 TO I       
                         MOVE REG-MAE TO REGISTRO (I)  
                         MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                         MOVE '02'  TO A-CONC (I) 
                         MOVE HSFUNK TO A-HABER (I)
                         MOVE '*M119 HSFUNCK    ' TO A-CODIGO (I). 


            IF (CONC = '01' AND POS-1-4 = 'M119') OR
                 (CONC = '02' AND POS-1-4 = 'M119' AND PORC > '00')
             THEN
              IF (POS-5-6 = 'B0' OR 'B1' OR 'B2' OR 'B3' OR 'B4')          
                 OR (POS-5 = 'E' OR 'H' OR 'I' OR 'K')          
                THEN
                 IF FECHA-MES > '200301' AND FECHA-MES < '200702'     
                       THEN                                                       
                       PERFORM VERCARGO-M119 THRU F-VERCARGO-M119
                         IF FUN119 > ZEROS
                             THEN 
                         ADD 1 TO I       
                         MOVE REG-MAE TO REGISTRO (I)  
                         MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                         MOVE '02'  TO A-CONC (I) 
                         MOVE '*M119 RESP.FUNC. ' TO A-CODIGO (I)  
                         COMPUTE A-HABER (I) = FUN119 * PORC / 100       
                         GO TO FIN-CALCULA-BONIF-SH 
                             ELSE
                         GO TO FIN-CALCULA-BONIF-SH 
                       ELSE                                                       
                 IF FECHA-MES > '200703'                              
                             THEN 
                         ADD 1 TO I       
                         MOVE REG-MAE TO REGISTRO (I)  
                         MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                         MOVE '02'  TO A-CONC (I) 
                         MOVE '*M119 ORD.25300  ' TO A-CODIGO (I)  
                         COMPUTE A-HABER (I) = A-HABER (1) * 20 / 100       
                         GO TO FIN-CALCULA-BONIF-SH 
                             ELSE
                         GO TO FIN-CALCULA-BONIF-SH.

    ***** exptes 21557-121421-09 y 21557-161063-10                                             
    ***** DESDE ENE12 se pagan las no rem sin tramite previo, pero luego se mandan a recaudac y fiscaliz.

            IF (CONC = '01' OR '02') AND POS-1-4 = 'M098'
             THEN
                 IF FECHA-MES > '200810'     
                    THEN
     *******              IF POS-5 = 'N' OR '4' OR '5' a los jer tb les corresponde, mod jun13.
                          IF POS-5 = 'R' OR 'T' OR 'J' OR 'B'
                           THEN NEXT SENTENCE                             
                           ELSE                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M098       ' TO A-CODIGO (I)  
                  IF FECHA-MES > '201309'     
                        THEN
                  COMPUTE A-HABER (I) = 925 * PORC / 100      
                        ELSE
                  IF FECHA-MES = '201309'     
                        THEN
                  COMPUTE A-HABER (I) = 870 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201305'     
                        THEN
                  COMPUTE A-HABER (I) = 815 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201304'     
                        THEN
                  COMPUTE A-HABER (I) = 760 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201209'     
                        THEN
                  COMPUTE A-HABER (I) = 705 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201207'     
                        THEN
                  COMPUTE A-HABER (I) = 605 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201205'     
                        THEN
                  COMPUTE A-HABER (I) = 505 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201204'     
                        THEN
                  COMPUTE A-HABER (I) = 455 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '201107'     
                        THEN
                  COMPUTE A-HABER (I) = 390                              
                        ELSE
                  IF FECHA-MES > '201003'     
                        THEN
                  COMPUTE A-HABER (I) = 170 * PORC / 100      
                        ELSE
                  IF FECHA-MES > '200811'     
                             THEN
                  COMPUTE A-HABER (I) =  85 * PORC / 100      
                             ELSE
                  COMPUTE A-HABER (I) = 120 * PORC / 100      
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** exptes 21557-142140-09 y  21557-169816-10                                                            
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M088'
             THEN
                 IF (FECHA-MES > '200908' AND FECHA-MES < '201001')
                    OR
                    (FECHA-MES > '201007' AND FECHA-MES < '201101')            
                    THEN
                      IF POS-5-8 = 'S252' OR 'S050'                       
                           THEN NEXT SENTENCE               
                           ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M088       ' TO A-CODIGO (I)  
                  IF FECHA-MES > '200908' AND
                    FECHA-MES < '201001'
                  THEN COMPUTE A-HABER (I) =  50 * PORC / 100      
                  ELSE COMPUTE A-HABER (I) =  70 * PORC / 100      
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

      ***** se pago reaj agos09 a jul11, en mov ago11.     
            IF CONC = '01' AND POS-1-4 = 'M081'
             THEN
                 IF FECHA-MES > '199602'                     
                    THEN
                      IF POS-5-8 = 'M281'                                    
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.GUARDAV    ' TO A-CODIGO (I)  
                  COMPUTE A-HABER(I) = H-HABER * 0.687 * PORC / 100.     

    ***** expte 21557-174881-10; expte 21557-269946-13 sf set13-oct13-nov13-ene14.         
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M081'
             THEN
                 IF FECHA-MES = '201008' OR '201010'
                       OR '201101' OR '201310' OR '201311'    
                       OR '201312' OR '201402'                   
                    THEN
                      IF POS-5 = '1' OR 'P' OR '2' OR 'T'                  
                          OR '3' OR '4' OR 'A' OR '5' OR                   
                             'O' OR '6' OR 'M'                             
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M081       ' TO A-CODIGO (I)  
                 IF FECHA-MES = '201008'            
                  THEN COMPUTE A-HABER (I) = 100 * PORC / 100      
                  ELSE
                 IF FECHA-MES = '201010'            
                  THEN COMPUTE A-HABER (I) = 150 * PORC / 100      
                  ELSE
                 IF FECHA-MES = '201101'            
                  THEN COMPUTE A-HABER (I) = 150 * PORC / 100      
                  ELSE
                 IF FECHA-MES = '201310' OR '201311'
                  THEN COMPUTE A-HABER (I) = 400 * PORC / 100      
                  ELSE
                       COMPUTE A-HABER (I) = 800 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-174355-10                                                            
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M077'
             THEN
                 IF FECHA-MES = '201009' OR '201010' OR
                        '201011' OR '201012' OR '201101'
                  THEN
                      IF POS-5 = '1' OR '2' OR '3'                  
                          OR '4' OR '5' OR '6' OR '7'                      
                        THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M077       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                         ELSE GO TO FIN-CALCULA-BONIF-SH 
                  ELSE                                 
                 IF FECHA-MES > '201208'                       
                    THEN
                      IF (POS-5 = '1' OR '2' OR '3'                  
                       OR '4' OR '5' OR '6' OR '7') OR                     
                       POS-5-8 = 'S302' OR 'S501'
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M077-1336/12   ' TO A-CODIGO (I)  
                  IF FECHA-MES > '201308'                       
                   THEN COMPUTE A-HABER (I) = 550 * PORC / 100      
                        IF FECHA-MES = '201402'                       
                          THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M077-2398/13   ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 500 * PORC / 100      
                          ELSE GO TO FIN-CALCULA-BONIF-SH 
                   ELSE COMPUTE A-HABER (I) = 400 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.
    ***** expte 21557-142145-09 SF=200                                                           
    ***** expte 21557-196247-11 SF=300                                                           
    ******* IF (CONC = '01' OR '02') AND POS-1-4 = 'M069'
            IF CONC = '01' AND POS-1-4 = 'M069'
             THEN
                 IF FECHA-MES > '200907'                  
                    THEN
                      IF POS-5 = '1' OR '2' OR '3'                  
                          OR '4' OR '5' OR '6'                             
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M069       ' TO A-CODIGO (I)  
                  IF FECHA-MES > '201107' AND                  
                     FECHA-MES < '201202'                      
                    THEN
              COMPUTE A-HABER(I) = (200 + 300) * PORC / 100      
                    ELSE
              COMPUTE A-HABER (I) = 200 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-138541-09                                                            
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M039'
             THEN
                 IF FECHA-MES = '200906' OR '200907'      
                    THEN
                      IF POS-5 = 'N' OR '5' OR '6' OR '3'                 
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M039       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 120 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                    ELSE                      
    ***** expte 21557-205406-11 - ORD 1643/2011                                            
                 IF FECHA-MES = '201102'                  
                    THEN
                      IF POS-5-8 = 'S252' OR 'S050'              
                                   OR 'S450' OR 'S455'
                         THEN GO TO FIN-CALCULA-BONIF-SH 
                         ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M039       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 250 * PORC / 100      
                    ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-108414-08 abr-may08 sf=150 para todos.                   
    ***** expte 21557-157969-10                                                
    ***** expte 21557-144727-09 ppto may-jun09                                 
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M029'
             THEN
                 IF FECHA-MES = '200805' OR '200806'      
                    THEN          
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M029       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 150 * PORC / 100      
                    ELSE
                 IF FECHA-MES = '201001' OR '201003'      
                             OR '200906' OR '200907'      
                    THEN
                      IF POS-5-8 = 'S252' OR 'S050' OR                  
                        'S006' OR 'S312' OR 'S463' OR 'S001'                  
                           THEN NEXT SENTENCE               
                           ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M029       ' TO A-CODIGO (I)  
                  IF FECHA-MES = '200906' OR '200907' OR '201001'              
                                THEN
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                                ELSE
                  COMPUTE A-HABER (I) =  50 * PORC / 100       
                    ELSE NEXT SENTENCE.              
*******************
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M029'
             THEN
                IF POS-5 = '1' OR '2' OR '3' OR '4' OR '5'                   
                     OR 'N' OR '6'                  
                  THEN
                 IF FECHA-MES = '201005'                  
                        THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  COMPUTE A-HABER (I) = 160 * PORC / 100      
                  MOVE '*BONI.M029-494/10' TO A-CODIGO (I)  
                       ELSE
                 IF FECHA-MES = '201006'                  
                        THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  COMPUTE A-HABER (I) = 120 * PORC / 100      
                  MOVE '*BONI.M029-494/10' TO A-CODIGO (I)  
                        ELSE GO TO FIN-CALCULA-BONIF-SH 
                   ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-142144-09  aplico mov dic10.                                               
    ***** expte 21557-142724-09 agr bon dto 478/08 por feb09 a mar09 para noj, y feb09 a abr09 para jer.
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M017'
             THEN
                 IF FECHA-MES > '200902'                  
                    THEN
                      IF POS-5 = 'N'                                   
                         THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M017       ' TO A-CODIGO (I)  
                 IF FECHA-MES > '200904'                  
                  THEN COMPUTE A-HABER (I) =  75 * PORC / 100      
                  ELSE COMPUTE A-HABER (I) =  37 * PORC / 100      
                       PERFORM BUS-M017 THRU BUS-M017-X            
                         IF B-M017 > 0
                           THEN                             
                    ADD 1 TO I       
                    MOVE REG-MAE TO REGISTRO (I)  
                    MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                    MOVE '02'  TO A-CONC (I) 
                    MOVE '*BONI.M017       ' TO A-CODIGO (I)  
                    COMPUTE A-HABER (I) = B-M017 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                         ELSE                            
                      IF POS-5 = 'S'                                   
                           THEN                             
                 IF FECHA-MES > '200902' AND FECHA-MES < '201106'
                     THEN PERFORM BUS-M017 THRU BUS-M017-X            
                         IF B-M017 > 0
                           THEN                             
                    ADD 1 TO I       
                    MOVE REG-MAE TO REGISTRO (I)  
                    MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                    MOVE '02'  TO A-CONC (I) 
                    MOVE '*BONI.M017       ' TO A-CODIGO (I)  
                    COMPUTE A-HABER (I) = B-M017 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-140995-09                                                            
    ***** expte 21557-180424-11 nov10                                                      
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M011'
             THEN
                 IF FECHA-MES = '200906' OR '201012'                 
                    THEN
                      IF POS-5 = 'F'                                   
                           THEN NEXT SENTENCE               
                           ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M011       ' TO A-CODIGO (I)  
                 IF FECHA-MES = '200906'                             
                  THEN COMPUTE A-HABER (I) = 200 * PORC / 100      
                  ELSE COMPUTE A-HABER (I) =                                  
                                 (A-HABER(1) + A-ANTIG(1)) / 2
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

            IF (CONC = '01' OR '02') AND POS-1-4 = 'M010'
             THEN PERFORM BONI-M010 THRU BONI-M010-X.

    ***** exptes 21557-142141-09  y 21557-170711-10                                                           
    *****  dto 768/10 modif al 513/10 y extiende el pago de los 250 por 4 meses mas                                      
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M010'
             THEN
                 IF FECHA-MES = '200909' OR '201009'                 
                  OR '201010' OR '201011' OR '201012'                 
                  OR '201102'                                         
                    THEN
                      IF POS-5 = 'N'                                   
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M010       ' TO A-CODIGO (I)  
                  IF FECHA-MES = '200909'                             
                   THEN COMPUTE A-HABER (I) = 150 * PORC / 100      
                   ELSE COMPUTE A-HABER (I) = 250 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-159546-10 aplico en dic10 para cargos mples                          
    ***** expte 21557-162073-10 aplico en abr11 para cargos 'C' del HCD                    
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M003'
             THEN
                 IF FECHA-MES = '201004'                  
                    THEN
                      IF POS-5 = '1' OR '2' OR '3' OR '4'                                   
                                     OR '5' OR '6' OR 'C'                                   
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M003       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 130 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

            IF (CONC = '01' OR '02') AND POS-1-4 = 'M118'
             THEN
              IF (POS-5-8 = 'S303' OR 'S304' OR '9303' OR                              
                           '9304') OR                                                  
             (POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '6')                                    
                           THEN                             
                 IF FECHA-MES > '201009' AND FECHA-MES < '201211'                  
                    THEN
                    IF FECHA-MES = '201101'                                           
                         THEN GO TO FIN-CALCULA-BONIF-SH 
                         ELSE
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M118-3051/10' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                    ELSE                            
                 IF FECHA-MES > '200706' AND FECHA-MES < '200802'                                          
                         THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M118-2007' TO A-CODIGO (I)  
                 IF FECHA-MES < '200711'  THEN                                                              
                  COMPUTE A-HABER (I) = H-HABER * 0.2 * PORC / 100      
                                         ELSE                            
                  COMPUTE A-HABER (I) = H-HABER * 0.22 * PORC / 100      
                         ELSE                            
                 IF FECHA-MES > '201305' AND FECHA-MES < '201309'                                          
                    THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M118-2013' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 115 * PORC / 100      
                    ELSE GO TO FIN-CALCULA-BONIF-SH 
                           ELSE                            
              IF POS-5-8 = 'S302' OR 'S501' OR 'S051' OR 'S312'                       
                  THEN
                 IF FECHA-MES > '200706' AND FECHA-MES < '200802'                                          
                         THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M118-2007' TO A-CODIGO (I)  
                 IF FECHA-MES < '200711' THEN                                                              
                  COMPUTE A-HABER (I) = H-HABER * 0.2 * PORC / 100      
                                         ELSE
                  COMPUTE A-HABER (I) = H-HABER * 0.22 * PORC / 100      
                         ELSE GO TO FIN-CALCULA-BONIF-SH 
                  ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-120458-09                                                            
            IF (CONC = '01' OR '02') AND POS-1-4 = 'M001'
             THEN
                 IF FECHA-MES = '200810' OR '200811' OR   
                                '200812' OR '200901'      
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M001       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = H-HABER * 0.15 * PORC / 100      
                           ELSE                            
                 IF (FECHA-MES > '200707' AND FECHA-MES < '200801')
                    OR FECHA-MES = '200802'      
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M001-985/07' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = H-HABER * 0.2 * PORC / 100      
                  IF POS-5 = 'S' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M001-984/07' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                          ELSE GO TO FIN-CALCULA-BONIF-SH 
                          ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-186714-11                                                            
            IF CONC = '01' AND POS-1-4 = 'M014'
             THEN
               IF FECHA-MES = '201102' AND              
                     ((POS-5 = '3' OR '4' OR '5' OR '6')
                    OR (POS-5-8 = 'S303' OR 'S304'))
                     THEN                              
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M014       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 200 * PORC / 100      
                     ELSE
               IF FECHA-MES > '201009' AND FECHA-MES < '201102'                  
                      AND POS-5 = '9'
                     THEN                              
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M014       ' TO A-CODIGO (I)  
                  IF POS-6 = 'E'   
                     THEN   IF POS-9-10 = '36'
                       THEN COMPUTE A-HABER (I) = 242.36 * PORC / 100      
                       ELSE COMPUTE A-HABER (I) = 161.58 * PORC / 100      
                     ELSE
                  IF POS-6 = 'B'   
                     THEN IF POS-9-10 = '36'
                       THEN COMPUTE A-HABER (I) = 256.73 * PORC / 100      
                       ELSE COMPUTE A-HABER (I) = 171.15 * PORC / 100      
                     ELSE
                  IF POS-6 = 'H'   
                     THEN IF POS-9-10 = '36'
                       THEN COMPUTE A-HABER (I) = 272.88 * PORC / 100      
                       ELSE COMPUTE A-HABER (I) = 181.92 * PORC / 100      
                     ELSE
                  IF POS-6 = 'I'   
                     THEN IF POS-9-10 = '36'
                       THEN COMPUTE A-HABER (I) = 287.25 * PORC / 100      
                       ELSE COMPUTE A-HABER (I) = 191.50 * PORC / 100      
                     ELSE
                  IF POS-6 = 'K'   
                     THEN IF POS-9-10 = '36'
                       THEN COMPUTE A-HABER (I) = 303.40 * PORC / 100      
                       ELSE COMPUTE A-HABER (I) = 202.27 * PORC / 100      
                     ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-108413-08                                                            
            IF CONC = '01' AND POS-1-4 = 'M006'
             THEN
               IF FECHA-MES < '200808' OR FECHA-MES > '200901'    
                     THEN GO TO FIN-CALCULA-BONIF-SH 
                     ELSE                                   
                  IF (POS-5-8 = 'S303' OR 'S358' OR 'S359')
                   OR (POS-5 = '2' OR '3' OR '4' OR '5' OR 'E'  
                   OR '6' OR '7' OR 'A' OR 'B' OR 'C' OR 'D')
                     THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M006       ' TO A-CODIGO (I)  
                  IF FECHA-MES < '200811'                             
                   THEN COMPUTE A-HABER (I) = 130 * PORC / 100      
                   ELSE COMPUTE A-HABER (I) = 150 * PORC / 100      
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-060030-06 ord.5615/06,dtos.249/07,259/09,187/10,438/10,88/11.
            IF CONC = '01' AND POS-1-4 = 'M107'
             THEN
               IF FECHA-MES < '200612'                             
                 THEN GO TO FIN-CALCULA-BONIF-SH 
                 ELSE                                   
              IF POS-5 = '9' OR (POS-5-8 = 'S252' OR 'S050')
                     THEN GO TO FIN-CALCULA-BONIF-SH 
                     ELSE                                   
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M107       ' TO A-CODIGO (I)  
                  IF FECHA-MES < '200706' THEN                               
                        COMPUTE A-HABER (I) = 100 * PORC / 100      
                                          ELSE
                  IF FECHA-MES < '200905'                                    
                   THEN COMPUTE A-HABER (I) = 120 * PORC / 100      
                   ELSE
                  IF FECHA-MES < '201004'                                    
                   THEN COMPUTE A-HABER (I) = (200 + 150) * PORC / 100      
                   ELSE
                  IF FECHA-MES < '201009'                                    
                   THEN COMPUTE A-HABER (I) = (200 + 350) * PORC / 100      
                   ELSE
                  IF FECHA-MES < '201103'                                    
                   THEN COMPUTE A-HABER (I) =  631 * PORC / 100      
                   ELSE COMPUTE A-HABER (I) =  731 * PORC / 100.     

    ***** expte 21557-060029-06 dto.1358/06 (el expte 21557-62717/07 es por lo mismo).                                  
            IF CONC = '01' AND POS-1-4 = 'M132'
             THEN
               IF FECHA-MES < '200612'                             
                 THEN GO TO FIN-CALCULA-BONIF-SH 
                 ELSE                                   
              IF POS-5 = 'S'  THEN                            
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M132       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                             ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-182107-11 dto.2399/10.                                
            IF CONC = '01' AND POS-1-4 = 'M051'
             THEN
               IF FECHA-MES = '201102'                             
                THEN                              
              IF POS-5-8 = 'SENF' OR 'SLAV' OR 'BGM1'               
                            OR 'BAG1' OR 'BAG2'
                             THEN GO TO FIN-CALCULA-BONIF-SH 
                             ELSE                                      
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M051       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 200 * PORC / 100      
                ELSE
    ***** expte 21557-128916-09 dto.640/09                                  
               IF FECHA-MES = '200905'                             
                  THEN                              
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M051-100   ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 100 * PORC / 100      
                  ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-062974-07 ord. 1673/06                                
            IF CONC = '01' AND POS-1-4 = 'M043'
             THEN
               IF FECHA-MES < '200702' OR FECHA-MES > '200705'                            
                THEN                              
                    GO TO FIN-CALCULA-BONIF-SH 
                ELSE
              IF (POS-5-8 = 'S303' OR 'S304') OR POS-5 = 'N'       
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M043       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) =  80 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-055998-06 dto.409/06                                
            IF CONC = '01' AND POS-1-4 = 'M121'
             THEN
               IF FECHA-MES = '200702'                                                    
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M121       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) =  50 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-087773-07 dto.609/07                                
            IF CONC = '01' AND POS-1-4 = 'M047'
             THEN IF POS-5 = 'S'
                     THEN GO TO FIN-CALCULA-BONIF-SH 
                     ELSE
               IF FECHA-MES = '200705'                                                    
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M047       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) =  50 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-049582-06 dto.522/06                                
            IF CONC = '01' AND POS-1-4 = 'M027'
             THEN IF POS-5 = '1' OR '2' OR '3' OR '4' OR     
                             '5' OR '6'                        
                     THEN                              
               IF FECHA-MES = '200606' OR '200608' OR                                                  
                              '200609' OR '200610'                                                     
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M027       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) =  30 * PORC / 100      
                           ELSE GO TO FIN-CALCULA-BONIF-SH 
                     ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-075422-07 dto.522/07                                
            IF CONC = '01' AND POS-1-4 = 'M095'
             THEN IF (POS-5 = '1' OR '2' OR '3' OR '4' OR     
                             '5' OR '6' OR 'P' OR 'T' OR       
                      'N' OR 'A' OR 'O' OR 'M' OR 'J') OR       
                     (POS-5-8 = 'S303' OR 'S304')               
                     THEN                              
               IF FECHA-MES = '200705'                                                                 
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095       ' TO A-CODIGO (I)  
                  COMPUTE A-HABER (I) = 150 * PORC / 100      
                           ELSE                            
               IF FECHA-MES > '200705'                                                                 
                           THEN     
                       IF POS-17 = 'A' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.2                   
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095 CONDUC' TO A-CODIGO (I)  
                                       ELSE
                       IF POS-17 = 'B' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.35                  
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095 CONDUC' TO A-CODIGO (I)  
                                       ELSE
                       IF POS-17 = 'C' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.5                   
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095 CONDUC' TO A-CODIGO (I)  
                                       ELSE
                       IF POS-17 = 'F' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.5                   
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095 FALLAC' TO A-CODIGO (I)  
                                       ELSE
                       IF POS-17 = 'R' THEN
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.1                   
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M095 ACTIVR' TO A-CODIGO (I)  
                     ELSE GO TO FIN-CALCULA-BONIF-SH 
               ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-161064-10 dto.774/10                                
            IF CONC = '01' AND POS-1-4 = 'M016'
                     THEN                              
               IF FECHA-MES > '201004' AND FECHA-MES < '201008'                                        
                           THEN     
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  COMPUTE A-HABER (I) = H-HABER * 0.1 * PORC / 100      
                  MOVE '02'  TO A-CONC (I) 
                  MOVE '*BONI.M016       ' TO A-CODIGO (I)  
                           ELSE
               IF FECHA-MES > '201007' AND FECHA-MES < '201102'                                        
                   THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 COMPUTE A-HABER (I) = H-HABER / 1.1 * 0.05 * PORC / 100      
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M016       ' TO A-CODIGO (I)  
                   ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-187705-11 dto.403/10                                
            IF CONC = '01' AND POS-1-4 = 'M096'
                     THEN                              
               IF FECHA-MES = '201102'                                                                 
                           THEN     
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 COMPUTE A-HABER (I) = 300 * PORC / 100      
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M096       ' TO A-CODIGO (I)  
                          ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-192279-11 dto.1650/10 SF=900 unica vez                    
    ***** expte 21557-195747-11 dto.39/08 SF=200 dde ene08, mov oct11.        
            IF CONC = '01' AND POS-1-4 = 'M084'
                     THEN                              
             IF POS-5 = 'S' THEN GO TO FIN-CALCULA-BONIF-SH                                                       
                            ELSE
             IF FECHA-MES < '200802'                                                                 
                  THEN GO TO FIN-CALCULA-BONIF-SH                                                       
                  ELSE
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M084       ' TO A-CODIGO (I)  
                 IF FECHA-MES = '201102'                                                                 
                           THEN     
                   COMPUTE A-HABER (I) = 1100 * PORC / 100      
                   GO TO FIN-CALCULA-BONIF-SH 
                           ELSE     
                     IF FECHA-MES < '201203'                                                                 
              THEN COMPUTE A-HABER (I) =  200 * PORC / 100      
                   GO TO FIN-CALCULA-BONIF-SH 
              ELSE 
                     IF FECHA-MES < '201208'                                                                 
              THEN COMPUTE A-HABER (I) =  170 * PORC / 100      
                   GO TO FIN-CALCULA-BONIF-SH 
              ELSE 
                     IF FECHA-MES < '201211'                                                                 
              THEN COMPUTE A-HABER (I) =  140 * PORC / 100      
                   GO TO FIN-CALCULA-BONIF-SH 
              ELSE COMPUTE A-HABER (I) =  110 * PORC / 100      
                   GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-192349-11 dtos.806/10 y 168/11                       
            IF CONC = '01' AND POS-1-4 = 'M033'
                     THEN                              
               IF POS-5 = '9' THEN NEXT SENTENCE                                                       
                              ELSE
               IF FECHA-MES = '201102' OR '201105'                                                     
                           THEN     
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M033       ' TO A-CODIGO (I)  
                 IF FECHA-MES = '201102'                                                                 
                  THEN COMPUTE A-HABER (I) = 500 * PORC / 100      
                  ELSE COMPUTE A-HABER (I) = 250 * PORC / 100      
                          ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-198467-11 ord.1740/09, dto.841/10 y dto 832/11       
            IF CONC = '01' AND POS-1-4 = 'M103'
                     THEN                              
               IF (POS-5 = '1' OR '2' OR '3' OR '4' OR '5'                                              
                     OR '8' OR '6' OR 'N') OR (POS-1-4 =                                        
                     'S051' OR 'S501' OR 'S302')
                              THEN
                 IF FECHA-MES = '200909' OR '201009' OR '201108'                                         
                 THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M103       ' TO A-CODIGO (I)  
                 IF FECHA-MES = '200909'                                                                 
                  THEN COMPUTE A-HABER (I) = 100 * PORC / 100      
                  ELSE IF FECHA-MES = '201009'                                                                 
                    THEN COMPUTE A-HABER(I) = 200 * PORC / 100      
                    ELSE COMPUTE A-HABER(I) = 200 * PORC / 100      
                              ELSE GO TO FIN-CALCULA-BONIF-SH 
                 ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** exptes 21557-9169-05 dto.930/04 y 21557-196250-11 dto 394/11    
            IF CONC = '01' AND POS-1-4 = 'M061'
                     THEN                              
              IF (POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR '9'                                       
                               OR 'C' OR 'G' OR 'I' OR 'M') OR                                           
                  (POS-1-4 = 'S303' OR 'S304' OR 'S479')         
                THEN
                IF FECHA-MES = '200408' OR '200409' OR '200410' OR                                        
                      '200411' OR '200412' OR '201104' OR '201105'
                  THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M061       ' TO A-CODIGO (I)  
                IF FECHA-MES = '200408' OR '200409' OR '200410' OR                                        
                      '200411' OR '200412'
                    THEN
                 COMPUTE A-HABER (I) =  50 * PORC / 100      
                    ELSE
                 COMPUTE A-HABER (I) = 150 * PORC / 100      
                  ELSE GO TO FIN-CALCULA-BONIF-SH
                ELSE
              IF (POS-5 = 'S' OR 'T') AND                                                               
                 (FECHA-MES = '201104' OR '201105')                             
                THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M061       ' TO A-CODIGO (I)  
                 COMPUTE A-HABER (I) = 150 * PORC / 100      
                ELSE GO TO FIN-CALCULA-BONIF-SH.

    ***** expte 21557-157971-10 ord.186/09. En mov enero 13 agr los cod de agrup nuevos pq agrego escalaf mar07.
            IF CONC = '01' AND POS-1-4 = 'M099'
                     THEN                              
               IF POS-5 = 'N' OR '1' OR '2' OR '3' OR                                                  
                          '4' OR '5' OR '6' OR '8'                                                     
                              THEN
                 IF FECHA-MES = '201002'                                                                 
                  THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M099       ' TO A-CODIGO (I)  
                 COMPUTE A-HABER (I) = 200 * PORC / 100      
                  ELSE GO TO FIN-CALCULA-BONIF-SH 
                              ELSE GO TO FIN-CALCULA-BONIF-SH.

    *****  En mov ABRIL14  agrego BONIF. DEDICACION FUNC. DE LA M049O                  
            IF POS-1-4 = 'M049' AND POS-5 = 'O' AND FECHA-MES > '201101'
                                AND BONDEDFC > ZEROS AND                            
                                POS-17-18  NUMERIC AND POS-17-18 > '00' 
                  THEN
                 ADD 1 TO I       
                 MOVE REG-MAE TO REGISTRO (I)  
                 MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                 MOVE '02'  TO A-CONC (I) 
                 MOVE '*BONI.M049O D.FNC' TO A-CODIGO (I)  
                 COMPUTE A-HABER (I) = (BONDEDFC / 147 * 1.5 ) *  
                                        POS-17-18N
                  ELSE GO TO FIN-CALCULA-BONIF-SH .

       FIN-CALCULA-BONIF-SH. EXIT.

       BONI-M010.                      
            IF POS-5 = 'N' THEN NEXT SENTENCE
                           ELSE GO TO BONI-M010-X.
            IF FECHA-MES > '201603'                             
                 THEN MOVE 5837.78 TO BASE-BM010
                 ELSE MOVE ZEROS   TO BASE-BM010.
            IF FECHA-MES > '200705'                             
                    THEN
                      IF POS-17 = 'C' OR 'E' OR 'I' OR 'S'                                  
                           THEN                             
                  ADD 1 TO I       
                  MOVE REG-MAE TO REGISTRO (I)  
                  MOVE ZEROS TO  A-ANTIG (I) A-IOMA (I) 
                  MOVE '02'  TO A-CONC (I) 
                  IF POS-17 = 'C'                                                       
                   THEN MOVE '*BONI.M010 FCAJA ' TO A-CODIGO (I)  
                      IF FECHA-MES < '201604'                             
                         THEN
                        COMPUTE A-HABER(I) = A-HABER (1) * 50 / 100               
                         ELSE
                        COMPUTE A-HABER(I) = A-HABER (1) * 20 / 100               
                   ELSE
                  IF POS-17 = 'I'                                                       
                   THEN MOVE '*BONI.M010 INSAL ' TO A-CODIGO (I)  
                        IF FECHA-MES < '201604'                             
                         THEN
                        COMPUTE A-HABER(I) = A-HABER(1) * 33 / 100               
                         ELSE
                        COMPUTE A-HABER(I) = A-HABER(1) * 0.33                     
                                    + SUMAFIJ2 * 0.33 * PORC / 100
                   ELSE
                  IF POS-17 = 'E'                                                       
                   THEN MOVE '*BONI.M010 ENFER ' TO A-CODIGO (I)  
                        IF FECHA-MES < '201604'                             
                         THEN COMPUTE A-HABER(I) = H-HABER * 0.22                     
                                    * PORC / 100
     *************************** dde ppto mar16 la base es el bas del n02330:
                         ELSE COMPUTE A-HABER(I) = 5837.78 * 0.30                     
                                    * PORC / 100
                   ELSE MOVE '*BONI.M010 ENF/IN' TO A-CODIGO (I)  
                        IF FECHA-MES < '201604'                             
                         THEN COMPUTE A-HABER(I) = H-HABER * 22 / 100               
                                    * PORC / 100 + A-HABER(1) * 33 / 100
                         ELSE COMPUTE A-HABER(I) = 5837.78 * 0.30                     
                                    * PORC / 100 + A-HABER(1) * 0.33
                                    + SUMAFIJ2 * 0.33 * PORC / 100.
            MOVE ZEROS TO BASE-BM010.
       BONI-M010-X.     EXIT.

    ***** exptes 21557-142141-09  y 21557-170711-10                                                           
       VERCARGO-M119.                                 
 ***********  solo vienen los >200301 y <200702
              MOVE ZEROS TO FUN119.
              IF POS-6-7 = '19' OR '21'  
                         THEN MOVE 300 TO FUN119
                         ELSE
                  IF POS-6-7 = '24' OR '45' OR '35' 
                         THEN MOVE 200 TO FUN119.
              IF FECHA-MES > '200508'
                           THEN
                      IF (POS-6-7 = '08' OR '07') OR           
                                 (POS-5 = 'K' AND POS-6-7 = '43')
                         THEN MOVE 300 TO FUN119.
       F-VERCARGO-M119. EXIT.
       BON-M059-OB.
            IF FECHA-MES > '201103' AND  SUFI2 > ZEROS
              THEN ADD 1 TO I
                   MOVE REG-MAE TO REGISTRO (I)
                   MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                   COMPUTE A-HABER (I) = SUFI2 * PORC / 100      
                   MOVE '*BONI.M059O 2011'  TO A-CODIGO (I)
                   MOVE '02' TO A-CONC (I). 
            IF POS-10 = '2'                                  
              THEN ADD 1 TO I
                   MOVE REG-MAE TO REGISTRO (I)
                   MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                   MOVE '*BONI.M059O TUR2'  TO A-CODIGO (I)
                   MOVE '02' TO A-CONC (I)  
                   IF FECHA-MES < '200807'  THEN                          
     ************* COMPUTE A-HABER (I) = A-HABER (1) * 20 / 100 * PORC / 100 cambio, pasa 2 veces por el porc.movsetiembre15.
                   COMPUTE A-HABER (I) = A-HABER (1) * 20 / 100      
                                            ELSE
     ************* COMPUTE A-HABER (I) = A-HABER (1) * 25 / 100 * PORC / 100      
                   COMPUTE A-HABER (I) = A-HABER (1) * 25 / 100      
              ELSE IF POS-10 = '3'                                  
                   THEN ADD 1 TO I
                        MOVE REG-MAE TO REGISTRO (I)
                        MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                        MOVE '*BONI.M059O TUR3'  TO A-CODIGO (I)
                        MOVE '02' TO A-CONC (I)  
                        IF FECHA-MES < '200807'  THEN                          
     ****************   COMPUTE A-HABER (I) = A-HABER (1) * 30 / 100 * PORC / 100 cambio en mov set15.
                        COMPUTE A-HABER (I) = A-HABER (1) * 30 / 100
                                                 ELSE
     ****************** COMPUTE A-HABER (I) = A-HABER (1) * 40 / 100 * PORC / 100.     
                        COMPUTE A-HABER (I) = A-HABER (1) * 40 / 100.     
       BON-M059-OB-X.        EXIT.
      *
       BON-M049.  
           MOVE ZEROS   TO  SUMA-ORD.
   ******  IF POS-5-8 = 'S252'             
   ******     THEN IF FECHA-MES > '200609'                          
   ******            THEN GO TO BON-M049-X.

           IF FECHA-MES < '200502'                          
                   THEN MOVE 216.87    TO  SUMA-ORD 
                   ELSE
     *****      IF (POS-5-8 = 'S481' OR 'S007') no hay cod s007 dde mov nov11 y es cod no vig.
                IF POS-5-8 = 'S481'                
                     THEN  MOVE 316    TO  SUMA-ORD
                     ELSE
                     IF FECHA-MES < '200505' THEN
                           MOVE 316    TO  SUMA-ORD 
                                           ELSE
                           IF FECHA-MES < '200507' THEN
                                  MOVE 244    TO  SUMA-ORD
                                                   ELSE
                                  IF FECHA-MES < '200509' THEN
                                       MOVE 172    TO  SUMA-ORD
                                                          ELSE
                                       MOVE 100    TO  SUMA-ORD.
************ se SACA   el 28-03-06
       BON-M049-VER.
              IF SUMA-ORD = ZEROS THEN GO TO BON-M049-X.
       CALC-049.
              ADD 1 TO I.
              MOVE REG-MAE TO REGISTRO (I).
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).                       
              MOVE '*ORD/16639 ' TO A-CODIGO (I).
              MOVE '02'  TO A-CONC (I).
           IF A-POS-9-10N (I) > 00 AND A-POS-9-10N (I) < 35
                    THEN
           COMPUTE A-HABER (I) = ((SUMA-ORD * A-POS-9-10N (I)) / 35)         
                                  * PORC / 100
                    ELSE
           COMPUTE A-HABER (I) = SUMA-ORD * PORC / 100.
           MOVE SPACES         TO A-EME (I).
           MOVE POS-15-16      TO A-POS-17-18 (I).
   ******  IF POS-1-4 = 'M049' AND MARCAAMP = 1
   ******     THEN DISPLAY 'SALE ' 
   ******          DISPLAY REGISTRO (I).
       BON-M049-X.    
           EXIT.
       BON1-M049.    
           IF POS-5 = '1' OR '2' OR '3' OR '4' OR '5' OR     
                   '6' OR 'A' OR 'M' OR 'V'
                 THEN
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)                        
              MOVE '02'  TO A-CONC (I) 
              MOVE '*OR13213/D1462/07' TO A-CODIGO (I) 
              IF FECHA-MES = '201108'
               THEN COMPUTE A-HABER (I) = 400 * PORC / 100 
               ELSE IF FECHA-MES = '200808' OR '201103' OR '201206'
                 THEN COMPUTE A-HABER (I) = 500 * PORC / 100 
                 ELSE IF FECHA-MES = '201306' OR '201309'                 
                  THEN COMPUTE A-HABER (I) = 800 * PORC / 100 
                  ELSE IF FECHA-MES = '200807'
               THEN COMPUTE A-HABER(I) = A-HABER(1) * 20 / 100 
               ELSE COMPUTE A-HABER(I) = A-HABER(1) * 40 / 100.
       BON1-M049-X.    
           EXIT.

     **** desde jul08 cambia el calculo de los titulos.
       BON-OBS-M049.                       
           MOVE 0 TO TITU.
           MOVE 1 TO T.
           IF POS-9 = 'S' THEN                                 
                 IF FECHA-MES < '200808' THEN
             IF POS-6-7 = 'P7' OR 'PN'          
                OR 'PO' OR 'PP' OR 'PQ' OR 'PM' 
                          THEN
             COMPUTE TITU = SP7M049O * PORC / 100
                          ELSE
             COMPUTE TITU = SECM049O * PORC / 100 
                                         ELSE
             PERFORM BASE-TIT THRU BASE-TIT-X
             COMPUTE TITU = MINOBS49 * 0.1 * PORC / 100.
           IF POS-9 = 'T' THEN     
                 IF FECHA-MES < '200808' THEN
             IF POS-6-7 = 'P7' OR 'PN'          
                OR 'PO' OR 'PP' OR 'PQ' OR 'PM' 
                          THEN
             COMPUTE TITU = TP7M049O * PORC / 100 
                          ELSE
             COMPUTE TITU = TECM049O * PORC / 100 
                                         ELSE
             PERFORM BASE-TIT THRU BASE-TIT-X
             COMPUTE TITU = MINOBS49 * 0.15 * PORC / 100.
           IF POS-9 = 'A' THEN     
                 IF FECHA-MES < '200808' THEN
             IF POS-6-7 = 'P7' OR 'PN'          
                OR 'PO' OR 'PP' OR 'PQ' OR 'PM' 
                          THEN
             COMPUTE TITU = AP7M049O * PORC / 100
                          ELSE
             COMPUTE TITU = UNAM049O * PORC / 100 
                                         ELSE
             PERFORM BASE-TIT THRU BASE-TIT-X
             COMPUTE TITU = MINOBS49 * 0.20 * PORC / 100.
           IF POS-9 = 'B' THEN     
                 IF FECHA-MES < '200808' THEN
             IF POS-6-7 = 'P7' OR 'PN'          
                OR 'PO' OR 'PP' OR 'PQ' OR 'PM' 
                          THEN
             COMPUTE TITU = BP7M049O * PORC / 100
                          ELSE
             COMPUTE TITU = UNBM049O * PORC / 100 
                                         ELSE
             PERFORM BASE-TIT THRU BASE-TIT-X           
             COMPUTE TITU = MINOBS49 * 0.30 * PORC / 100.
           IF NOT (POS-6-7 = 'P7' OR 'PN'          
                OR 'PO' OR 'PP' OR 'PQ' OR 'PM')
              THEN
           IF POS-9 = 'E' THEN     
                 IF FECHA-MES < '200808' THEN
             PERFORM SUPTEC THRU SUPTEC-X VARYING T FROM 1 BY 1
                     UNTIL T > 20 
                                         ELSE
             COMPUTE TITU = A-HABER (1) * 0.1            
                          ELSE
           IF POS-9 = 'C' THEN     
                 IF FECHA-MES < '200808' THEN
             PERFORM SUPUNA THRU SUPUNA-X VARYING T FROM 1 BY 1
                     UNTIL T > 20 
                                         ELSE
             COMPUTE TITU = A-HABER (1) * 0.11           
                          ELSE
           IF POS-9 = 'D' THEN     
                 IF FECHA-MES < '200808' THEN
             PERFORM SUPUNB THRU SUPUNB-X VARYING T FROM 1 BY 1
                     UNTIL T > 20 
                                         ELSE
             COMPUTE TITU = A-HABER (1) * 0.12.          
       RTIT.
           IF TITU > 0 THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE TITU TO A-HABER (I)                            
                     COMPUTE BONDEDFC = BONDEDFC + A-HABER (I)
                     MOVE '*BONI.M049O TITU'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     MOVE 0 TO TITU.
           IF POS-10 = 'R' THEN 
             PERFORM GROTAT THRU GROTAT-X VARYING T FROM 1 BY 1
                     UNTIL T > 20
               IF TITU > 0 THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE TITU TO A-HABER (I)                            
                     MOVE '*BONI.M049O GROT'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     MOVE 0 TO TITU.
           IF POS-19 = 'D' OR 'E' THEN 
             PERFORM DIASG  THRU DIASG-X VARYING T FROM 1 BY 1
                     UNTIL T > 20 
               IF TITU > 0 THEN
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE TITU TO A-HABER (I)                            
                     MOVE '*BONI.M049O DGD.'  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     MOVE 0 TO TITU.
           IF POS-20 = 'F' THEN 
                     COMPUTE TITU = FCM049O  * PORC / 100
                     ADD 1 TO I
                     MOVE REG-MAE TO REGISTRO (I)
                     MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                     MOVE TITU TO A-HABER (I)                            
                     MOVE '*BONI.M049O FCA '  TO A-CODIGO (I)
                     MOVE '02' TO A-CONC (I) 
                     MOVE 0 TO TITU.
           IF FECHA-MES > '200807'                   
              THEN IF (POS-6-7 = 'R8' AND POS-8 = 'K') OR               
                      (POS-6-7 = 'P7' OR 'PQ' OR 'PO'          
                                      OR 'PM' OR 'PN' OR 'PP')         
                       THEN ADD 1 TO I
                            MOVE REG-MAE TO REGISTRO (I)
                            MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                            COMPUTE A-HABER (I) = A-HABER (1) * 0.22    
                            COMPUTE BONDEDFC = BONDEDFC + A-HABER (I)
                            MOVE '*BONI.M049O ESTR'  TO A-CODIGO (I)
                            MOVE '02' TO A-CONC (I).
       BON-OBS-M049-X.

       SUPTEC.
           IF CLASE(T) = POS-8
             THEN COMPUTE TITU = (TECM049O + ETEC (T)) * PORC / 100
                  MOVE 22 TO T.
       SUPTEC-X.    EXIT.
       SUPUNA.
           IF CLASE(T) = POS-8
             THEN COMPUTE TITU = (UNAM049O + EUNA (T)) * PORC / 100
                  MOVE 22 TO T.
       SUPUNA-X.    EXIT.
       SUPUNB.
           IF CLASE(T) = POS-8
             THEN COMPUTE TITU = (UNBM049O + EUNB (T)) * PORC / 100
                  MOVE 22 TO T.
       SUPUNB-X.    EXIT.
       GROTAT.
           IF CLASE(T) = POS-8
             THEN COMPUTE TITU = GROT (T) * PORC / 100
                  MOVE 22 TO T.
       GROTAT-X.    EXIT.

     ****** dde mov feb13 agr letra E en pos 19 para dias de guardia 3 turnos, dde junio 05, antes sf.
     ******  en expte 21557-257449-13 la repart pide la cod q ya esta creada dde mov feb13.             
       DIASG.
           IF CLASE (T) = POS-8 THEN
            IF FECHA-MES < '200507'
            THEN IF T < 8                             
                  THEN
                  COMPUTE TITU = D1M049O  * PORC / 100
                  MOVE 22 TO T 
                  ELSE
                 IF CLASE (T) = 'A' OR 'B' OR 'C' OR 'D' OR 'E'
                  THEN
                  COMPUTE TITU = DAM049O  * PORC / 100
                  MOVE 22 TO T 
                  ELSE
                 IF CLASE (T) = 'F' OR 'G' OR 'H' OR 'I' OR 'J'
                                   OR 'K' OR 'L' OR 'M'          
                  THEN COMPUTE TITU = DFM049O  * PORC / 100 
                  ELSE NEXT SENTENCE
            ELSE
                  IF POS-19 = 'D'     
                    THEN COMPUTE TITU = A-HABER(1) * 29 / 100 
                    ELSE COMPUTE TITU = A-HABER(1) * 39 / 100.
       DIASG-X.   EXIT.

       BASE-TIT.           
           IF T > 22 THEN GO TO BASE-TIT-X.
           IF FECHA-MES > PPTO-T(T)
             THEN MOVE BAS-T(T) TO MINOBS49
                  MOVE 22 TO T 
             ELSE ADD 1 TO T        
                  GO TO BASE-TIT.
       BASE-TIT-X.     EXIT.

       CALCULA-20.
     ***************** mov abr14 agr salto para el mes pq aparecia en las liq de correlaciones y pidieron q no salga.
           IF COD-RETRO = 'M'                  
                      THEN GO TO FIN-CALCULA-20.
           IF COD-RETRO = 'R' OR 'A'            
              THEN IF FECHA-MES > '199808'
                      THEN GO TO FIN-CALCULA-20.
           IF COD-RETRO = 'C'                  
                      THEN GO TO FIN-CALCULA-20.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
           IF FECHA-MES < '199204'
              THEN
                   MOVE 2000.00 TO A-HABER (I)
              ELSE
                   MOVE 20.00     TO A-HABER (I).

           MOVE '*BONI.NO REM.   '  TO A-CODIGO (I).
           MOVE '02'                TO A-CONC (I).
       FIN-CALCULA-20. EXIT.
      *
       CALCULA-AMPARO.
           IF NRO = '1083867940' AND MARCA = '1' AND CONC = '01'
             THEN IF FECHA-MES > '200407'     
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          COMPUTE SUP-AUX = 530 * PORC / 100
                          MOVE SUP-AUX TO A-HABER (I)
                          MOVE '*AMPAROM053     '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          DISPLAY 'agrega ' NRO ' amparo '
                          GO TO FIN-CALCULA-AMPARO.
           IF NRO = '8055952910' AND MARCA = '1' AND CONC = '01'
             THEN IF FECHA-MES > '200507'     
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          MOVE 373.80  TO A-HABER (I)
                          MOVE '*AMPAROM104     '  TO A-CODIGO (I)
                          MOVE '02' TO A-CONC (I) 
                          DISPLAY 'agrega ' NRO ' amparo '
                          GO TO FIN-CALCULA-AMPARO.
           IF NRO = '1050614410' AND MARCA = '1' AND CONC = '01'
             THEN IF FECHA-MES > '200510'     
                     THEN ADD 1 TO I
                          MOVE REG-MAE TO REGISTRO (I)
                          MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                          COMPUTE SUP-AUX = 530 * PORC / 100
                          MOVE SUP-AUX TO A-HABER (I)
                          MOVE '*AMPAROM053     '  TO A-CODIGO (I)
                          DISPLAY 'agrega ' NRO ' amparo '
                          MOVE '02' TO A-CONC (I).
       FIN-CALCULA-AMPARO. EXIT.

       068-ENE09.                   
         IF CO-068 (T, X) = SPACES   
           THEN MOVE 10 TO X 
                GO TO 068-ENE09-X
           ELSE
         IF POS-5-8 NOT = CO-068 (T, X) THEN
             GO TO 068-ENE09-X.                           
         MOVE NR-068 (T) TO SF68.
         IF FECHA-MES < '201102' 
           THEN MOVE NR1-068 (T) TO SF68-1
           ELSE IF T < 8         
                THEN MOVE NR1-068 (T) TO SF68-1
                ELSE MOVE ZEROS TO SF68-1.
         IF FECHA-MES > '201106'  AND FECHA-MES < '201304'
           THEN MOVE NR2-068 (T) TO SF68-2.
         MOVE 18 TO T.
       068-ENE09-X.   EXIT.                   

       SU-M102.                     
         IF POS-5-8 NOT = CO-102 (T)     
             THEN GO TO SU-M102-X
             ELSE COMPUTE S-M102 = 0.04 * NR-102 (T)               
                  IF POS-9-10 = '40' OR '48'
                 THEN COMPUTE S-M102 = S-M102 * 1.33                   
                 ELSE 
                  IF POS-9-10 = '45'        
                 THEN COMPUTE S-M102 = S-M102 * 1.5.                    
         MOVE 45 TO T.
       SU-M102-X.     EXIT.

       BUS-M017.              
         IF POS-5-6 = 'C0' OR 'N0' OR 'NA'
           THEN COMPUTE B-M017 = 750 * 0.2
           ELSE
         IF POS-5-6 = 'NS'                          
           THEN COMPUTE B-M017 = 1000 * 0.2
           ELSE
         IF POS-5-8 = 'S007' OR 'S302' OR 'S347'     
           THEN COMPUTE B-M017 = 2200 * 0.2
           ELSE
         IF POS-5-8 = 'S050' OR 'S454'               
           THEN COMPUTE B-M017 = 1652 * 0.2
           ELSE
         IF POS-5-8 = 'S051'                        
           THEN COMPUTE B-M017 = 2750 * 0.2
           ELSE
         IF POS-5-8 = 'S100'                        
           THEN COMPUTE B-M017 = 1800 * 0.2
           ELSE
         IF POS-5-8 = 'S121' OR 'S476'              
           THEN COMPUTE B-M017 = 1870 * 0.2
           ELSE
         IF POS-5-8 = 'S250'                        
           THEN COMPUTE B-M017 = 1700 * 0.2
           ELSE
         IF POS-5-8 = 'S252'                        
           THEN COMPUTE B-M017 = 12000 * 0.2
           ELSE
         IF POS-5-8 = 'S312'                        
           THEN COMPUTE B-M017 =  2000 * 0.2
           ELSE
         IF POS-5-8 = 'S456' OR 'S466'              
           THEN COMPUTE B-M017 =  3000 * 0.2
           ELSE COMPUTE B-M017 = 0.
       BUS-M017-X.   EXIT.    

    ******** para la bonif cargar los valores del cod NN02 con cada rh.
       TODOM130.                                
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I).
           MOVE '02' TO A-CONC (I).
           MOVE '*B.M130 ORD2782' TO A-CODIGO (I).
           IF FECHA-MES > '201509'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 5636.38                               
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130      = 7382.11                              
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 8112.46                              
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201507'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 4901.20                               
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130      = 6419.42                              
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 7054.32                              
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201503'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 4492.77                               
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130      = 5884.47                              
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 6466.16                              
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201407'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 4084.13                               
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130      = 5349.03                              
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 5878.06                              
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201403'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 3612.88                             
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130      = 4731.83                            
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 5199.82                            
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201309'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130 = 3141.64                               
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130  = 4114.64                              
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 4521.59                              
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201303'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130      = 1963.72                           
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130 = 3740.93                            
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130  = 4110.91                             
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201207'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130 = 1510.56                             
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130  = 3117.44                             
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130      = 3425.76                             
                                    ELSE COMPUTE B-M130 = 0
            ELSE
           IF FECHA-MES > '201205'
            THEN IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130 = 1374.36                                    
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130 = 2836.36                           
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 3116.88                         
                                    ELSE COMPUTE B-M130 = 0
            ELSE
                 IF POS-9-10 = '00' OR '35' THEN
                 COMPUTE B-M130 = 1238.16                                 
                                            ELSE
                 IF POS-9-10 = '42' THEN      
                 COMPUTE B-M130 = 2555.28                                   
                                    ELSE
                 IF POS-9-10 = '48' THEN      
                 COMPUTE B-M130 = 2808                            
                                    ELSE COMPUTE B-M130 = 0.
           IF B-M130 = 0                        
                THEN NEXT SENTENCE
                ELSE
                 COMPUTE A-HABER (I) = B-M130 * 0.2 * POS-15-16N / 100.
           MOVE ZEROS TO B-M130.
       TODOM130-X.     EXIT.                    

       TODOM027.                                
           IF POS-17-18 IS NUMERIC AND POS-17-18N > ZEROS                           
             THEN ADD 1 TO I
                  MOVE REG-MAE TO REGISTRO (I)
                  MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                  MOVE '02' TO A-CONC (I) 
                  MOVE '*B.M027 DIS.PTE' TO A-CODIGO (I)
                  COMPUTE A-HABER (I) = A-HABER(1) * POS-17-18N.
           IF POS-5 = '2' AND POS-19 = 'B'
             THEN ADD 1 TO I
                  MOVE REG-MAE TO REGISTRO (I)
                  MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                  MOVE '02' TO A-CONC (I) 
                  MOVE '*B.M027 BLOQ.TI' TO A-CODIGO (I)
                  COMPUTE A-HABER (I) = A-HABER(1) * 0.50.        
       TODOM027-X.       EXIT.                  

       TODOM069.                                
           IF POS-5-8 = 'S050' THEN GO TO TODOM069-X.  
           IF POS-17  = '1' OR '2' OR '3' OR '4' OR '5'
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    MOVE '02' TO A-CONC (I) 
                    IF POS-17 = '1' THEN
                     MOVE '*B.M069 TITULO1' TO A-CODIGO (I)
                     COMPUTE A-HABER (I) = A-HABER(1) * 0.2 
                                    ELSE 
                    IF POS-17 = '2' THEN
                     MOVE '*B.M069 TITULO2' TO A-CODIGO (I)
                     COMPUTE A-HABER (I) = A-HABER(1) * 0.15
                                    ELSE 
                    IF POS-17 = '3' THEN
                     MOVE '*B.M069 TITULO3' TO A-CODIGO (I)
                     COMPUTE A-HABER (I) = A-HABER(1) * 0.1
                                    ELSE 
                    IF POS-17 = '4' THEN
                     MOVE '*B.M069 TITULO4' TO A-CODIGO (I)
                     COMPUTE A-HABER (I) = A-HABER(1) * 0.08
                                    ELSE 
                     MOVE '*B.M069 TITULO5' TO A-CODIGO (I)
                     COMPUTE A-HABER (I) = A-HABER(1) * 0.05.
           IF POS-5-8 = 'S252' THEN GO TO TODOM069-X.
           IF POS-5 = 'S' OR '1' OR '2' OR '3'
             THEN IF POS-18-19 IS NUMERIC AND POS-18-19R > 0
                                   AND POS-18-19R < 51
                    THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    MOVE '02' TO A-CONC (I) 
                    MOVE '*B.M069 DISP.PER.' TO A-CODIGO (I)
                    COMPUTE A-HABER (I) = A-HABER(1) * POS-18-19R
                                          / 100.
       TODOM069-X.      EXIT.                   

       TODOM012.                                
           IF POS-5 = '1' OR '2' OR '3' OR '4' OR '5'
                     OR '6' OR 'A' OR 'P'
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    MOVE '*B.M012 ASIG R'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                    MOVE 0 TO BON-MDAD012
                    PERFORM PONE-BON THRU PONE-BON-X 
                    MOVE BON-MDAD012 TO A-HABER (I)
                    ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    COMPUTE A-HABER (I) = SUPL2 * PORC / 100     
                    MOVE '*B.M012 D197/80'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                    IF POS-17 = 'R'
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    COMPUTE A-HABER (I) = H-HABER * 0.4 * PORC / 100     
                    MOVE '*B.M012 T.INS.'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                    IF POS-18-19R NUMERIC AND POS-18-19R > 0 AND
                       POS-18-19R < 41
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    COMPUTE A-HABER (I) = H-HABER * POS-18-19R / 100     
                                                           * PORC / 100     
                    MOVE '*B.M012 T.PEN.'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I).
                    IF POS-5 = '1' AND POS-6 = 'D'            
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    COMPUTE A-HABER (I) = H-HABER * 0.5 * PORC / 100     
                    MOVE '*B.M012 DISP.P'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I).
                    IF (POS-5 = 'P' OR '3') AND POS-6 = 'T'            
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    COMPUTE A-HABER (I) = H-HABER * 0.3 * PORC / 100     
                    MOVE '*B.M012 TA.EX.'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I).
                    IF (POS-5 = '4' OR '5') AND (POS-6 = '1' OR '2' OR                       
                                          '3' OR '4' OR '5')
               THEN ADD 1 TO I
                    MOVE REG-MAE TO REGISTRO (I)
                    MOVE ZEROS   TO A-IOMA (I) A-ANTIG (I)
                    MOVE '*B.M012 CONDUC'  TO A-CODIGO (I)
                    MOVE '02' TO A-CONC (I) 
                    IF POS-6 = '1'
                      THEN
                   COMPUTE A-HABER (I) = H-HABER * 0.3 * PORC / 100     
                      ELSE
                    IF POS-6 = '2' OR '3'
                      THEN
                   COMPUTE A-HABER (I) = H-HABER * 0.22 * PORC / 100     
                      ELSE
                    IF POS-6 = '4' OR '5'
                      THEN
                 COMPUTE A-HABER (I) = H-HABER * 0.18 * PORC / 100.    
       TODOM012-X.     EXIT.         

       PONE-BON.                     
            IF FECHA-MES > '201607'
              THEN  COMPUTE BON-MDAD012 = 5300 * PORC / 100     
              ELSE
            IF FECHA-MES > '201603'
              THEN  COMPUTE BON-MDAD012 = 4857 * PORC / 100     
              ELSE
             IF FECHA-MES > '201507'
              THEN  COMPUTE BON-MDAD012 = 4357 * PORC / 100     
              ELSE
               IF FECHA-MES > '201503'
                THEN COMPUTE BON-MDAD012 = 3825 * PORC / 100     
                ELSE
                 IF FECHA-MES > '201307'
                      THEN
                    COMPUTE BON-MDAD012 = 1880 * PORC / 100     
                      ELSE
                    IF FECHA-MES > '201303'
                      THEN
                    COMPUTE BON-MDAD012 = 1940 * PORC / 100     
                      ELSE
                    COMPUTE BON-MDAD012 = 1424 * PORC / 100.    
       PONE-BON-X.     EXIT.         
