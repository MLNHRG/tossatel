       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. 'BPAGAMUNA'.                                             000
       AUTHOR. 'MONTSERRAT 20/12/02'.                                       000
      *REMARKS. 'GENERA EL PAGO PARA LA M079 M106 M112 M135 M090 '.         000
      *REMARKS. 'GENERA DEUDA PARA LA M013  '.                              000
      *REMARKS. 'ARREGLO PARA EL 2000. MìNICA.'.                            000
      *REMARKS. 'Mov. 12-05 M117 se agrega bonif. de $196 a '.              000
      *REMARKS. 'partir de 08-05. para codif. vieja.'.                      000
      *REMARKS. 'Mov. 01-06 M084 se agrega bonif. de $140 a '.              000
      *REMARKS. 'partir de 08-05, 09-05 de hiscar. para las cat.'.          000
      *REMARKS. '1 a la 23 y para el S479.'.                                000
      *REMARKS. 'Mov. 04-06 M078 se agrega bonif. de $80 a '.               000
      *REMARKS. 'partir de 05-05 a 11-05 de hiscar.para los cargos.'.       000
      *REMARKS. 'comunes y el '3' '9C' '9F' Y '9G''.                        000 
      *REMARKS. '20-09-07  la M028 paga boni ORD4500 tiene IOMA'.    
      *REMARKS. '20-09-07  la M056 paga boni DTO268/07         '.    
      *REMARKS. '27-09-07  la M109 paga boni ORD1730           '.    
      *REMARKS. '02-02-07  la M119 paga boni *DT2876/07 PTO. 07/07'. 
      *REMARKS. '23-01-11  la M068 paga boni *DT1853/07 PTO. 10/07'. 
      *REMARKS. '03-09-13  la M124 paga GARANTIA desde PTO. 07/13'. 
      *REMARKS. '12-03-14  la M049 paga DTO 600/2010   PTO. 03/10'. 
      *REMARKS. '17-03-14  M119 paso bon DTOR2876/07 al gmae pq se usa p/las hs fcionales.
      *REMARKS. '12-11-14  M0519 agr bon de planta, 30%, si no cobra guardias. Agr algs S de cmh dde julio14.
       ENVIRONMENT DIVISION.                                                000
       CONFIGURATION SECTION.                                               000
       INPUT-OUTPUT SECTION.                                                001
       FILE-CONTROL.                                                        001
           SELECT MAESTRO ASSIGN MAESTRO.
           SELECT MAEACT ASSIGN MAEACT.
       DATA DIVISION.                                                       001
       FILE SECTION.                                                        001
       FD  MAESTRO                                                          001
           RECORD 140                                                       002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-MAE.                                          002
       01  REG-MAE PIC X(140).                                              002
       FD  MAEACT                                                           001
           RECORD 140                                                       002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-SAL.                                          002
       01  REG-SAL PIC X(140).                                              002
       WORKING-STORAGE SECTION.                                             004
           COPY WSFECHA.CPY.
       77  T-MAELEI     PIC 9(8) VALUE ZEROS.                               004
       77  T-GRABA      PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-010    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-013    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-013N   PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-028    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-049    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-056    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-060    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-061    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-063    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-044    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-074    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-079    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-078    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-084    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-090    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-106    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-109    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-1091    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-112    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-122    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-135    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-117    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-124    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-119    PIC 9(8) VALUE ZEROS.                               004
       77  T-MOD-051    PIC 9(8) VALUE ZEROS.                               004
       77  T-M004       PIC 9(8) VALUE ZEROS.                               004
       77  T-M008       PIC 9(8) VALUE ZEROS.                               004
       77  T-M010       PIC 9(8) VALUE ZEROS.                               004
       77  T-M013       PIC 9(8) VALUE ZEROS.                               004
       77  T-M013N      PIC 9(8) VALUE ZEROS.                               004
       77  T-M028       PIC 9(8) VALUE ZEROS.                               004
       77  T-M033       PIC 9(8) VALUE ZEROS.                               004
       77  T-M049       PIC 9(8) VALUE ZEROS.                               004
       77  T-M056       PIC 9(8) VALUE ZEROS.                               004
       77  T-M044       PIC 9(8) VALUE ZEROS.                               004
       77  T-M060       PIC 9(8) VALUE ZEROS.                               004
       77  T-M061       PIC 9(8) VALUE ZEROS.                               004
       77  T-M062       PIC 9(8) VALUE ZEROS.                               004
       77  T-M063       PIC 9(8) VALUE ZEROS.                               004
       77  T-M068       PIC 9(8) VALUE ZEROS.                               004
       77  T-M074       PIC 9(8) VALUE ZEROS.                               004
       77  T-M078       PIC 9(8) VALUE ZEROS.                               004
       77  T-M079       PIC 9(8) VALUE ZEROS.                               004
       77  T-M0791       PIC 9(8) VALUE ZEROS.                               004
       77  T-M084       PIC 9(8) VALUE ZEROS.                               004
       77  T-M090       PIC 9(8) VALUE ZEROS.                               004
       77  T-M106       PIC 9(8) VALUE ZEROS.                               004
       77  T-M109       PIC 9(8) VALUE ZEROS.                               004
       77  T-M112       PIC 9(8) VALUE ZEROS.                               004
       77  T-M122       PIC 9(8) VALUE ZEROS.                               004
       77  T-M135       PIC 9(8) VALUE ZEROS.                               004
       77  T-M117       PIC 9(8) VALUE ZEROS.                               004
       77  T-M124       PIC 9(8) VALUE ZEROS.                               004
       77  T-M119       PIC 9(8) VALUE ZEROS.                               004
       77  T-M051       PIC 9(8) VALUE ZEROS.                               004
       77  PORC-N       PIC 9V999  VALUE ZEROS.                             004
       77  ANTI         PIC 99     VALUE ZEROS.                             004
       77  SUMA-ORD     PIC S9(9)V999 VALUE ZEROS.                          004
       77  IMPOR109     PIC S9(9)V999 VALUE ZEROS.                          004
       77  IMPOR1091     PIC S9(9)V999 VALUE ZEROS.                          004
       77  IMPORTE      PIC S9(9)V999 VALUE ZEROS.                          004
       77  IMPORTE1     PIC S9(9)V999 VALUE ZEROS.                          004
       77  IMPORTE2     PIC S9(9)V99  VALUE ZEROS.                          004
       77  IMPORTE033   PIC S9(9)V99  VALUE ZEROS.                          004
       77  HABER-AUX    PIC S9(9)V999 VALUE ZEROS.                          004
       77  IOMA-AUX     PIC S9(9)V99  VALUE ZEROS.                          004
       77  SUMA-M008    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M004    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M004F2    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M0049F2    PIC S9(9)V999 VALUE ZEROS.                          004
       77  FIJA2-AUX       PIC S9(9)V999 VALUE ZEROS.                          004
       77  FIJA2-AUX1       PIC S9(9)V999 VALUE ZEROS.                          004
       77  HABM004-ACT    PIC S9(9)V999 VALUE ZEROS.                          004
       77  HABM004-ANT    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M060    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M028    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUMA-M056    PIC S9(9)V999 VALUE ZEROS.                          004
       77  CARGO-M060   PIC S9(9)V999 VALUE ZEROS.                          004
       77  HABER-ANT    PIC S9(9)V999 VALUE ZEROS.                          004
       77  SUM-HABER    PIC S9(9)V999 VALUE ZEROS.                          004
       77  BONIF-AUX    PIC S9(5)V99  VALUE ZEROS.                          004
       77  BONI-063     PIC S9(5)V99  VALUE ZEROS.                          004
       77  BONI-068     PIC S9(5)V99  VALUE ZEROS.                          004
       77  MIN063       PIC S9(5)V99  VALUE ZEROS.                          004
       77  MIN068       PIC S9(5)V99  VALUE ZEROS.                          004
       77  DIFE         PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-112    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-117    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-124    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-084    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-078    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-090    PIC S9(9)V99  VALUE ZEROS.                          004
       77  VALOR-013    PIC S9(9)V99  VALUE ZEROS.                          004
       77  IMP-SUPL1    PIC S9(9)V99  VALUE ZEROS.                          004
       77  IMP-SUPL2    PIC S9(9)V99  VALUE ZEROS.                          004
       77  IMP-SUMF1    PIC S9(9)V99  VALUE ZEROS.                          004
       77  IMP-SUMF2    PIC S9(9)V99  VALUE ZEROS.                          004
       77  T-REG        PIC 999     VALUE ZEROS.                            004
       77  I            PIC 999     VALUE ZEROS.                            004
       77  J            PIC 99      VALUE ZEROS.                            004
       77  K            PIC 99      VALUE ZEROS.                            004
       77  Z            PIC 99      VALUE ZEROS.                            004
       77  T            PIC 99      VALUE ZEROS.                            004
       77  I63          PIC 99      VALUE ZEROS.                            004
       77  I68          PIC 99      VALUE ZEROS.                            004
       77  I13          PIC 99      VALUE ZEROS.                            004
       77  I10          PIC 99      VALUE ZEROS.                            004
       77  I62          PIC 99      VALUE ZEROS.                            004
       77  I44          PIC 99      VALUE ZEROS.                            004
       77  I106         PIC 99      VALUE ZEROS.                            004
       77  I117         PIC 99      VALUE ZEROS.                            004
       77  I124         PIC 99      VALUE ZEROS.                            004
       77  I049         PIC 99      VALUE ZEROS.                            004
       77  I051         PIC 99      VALUE ZEROS.                            004
       77  I84          PIC 99      VALUE ZEROS.                            004
       77  PORC         PIC 999     VALUE ZEROS.                            004
       77  PORC68       PIC 999     VALUE ZEROS.                            004
       77  PORCM004     PIC 999     VALUE ZEROS.                            004
       77  REGH         PIC 99      VALUE ZEROS.                            004
       77  MASI         PIC 9      VALUE ZEROS.                             004
       77  VER-M004     PIC X  VALUE ' '.                                   004
       77  MARCA-004    PIC 9  VALUE ZERO.                                  004
       77  MARCA-004-14    PIC 9  VALUE ZERO.                               004
       77  MARCA-008    PIC 9  VALUE ZERO.                                  004
       77  MARCA-010    PIC 9  VALUE ZERO.                                  004
       77  MARCA-013    PIC 9  VALUE ZERO.                                  004
       77  MARCA-028    PIC 9  VALUE ZERO.                                  004
       77  MARCA-033    PIC 9  VALUE ZERO.                                  004
       77  MARCA-049    PIC 9  VALUE ZERO.                                  004
       77  MARCA-056    PIC 9  VALUE ZERO.                                  004
       77  MARCA-044    PIC 9  VALUE ZERO.                                  004
       77  MARCA-060    PIC 9  VALUE ZERO.                                  004
       77  MARCA-061    PIC 9  VALUE ZERO.                                  004
       77  MARCA-062    PIC 9  VALUE ZERO.                                  004
       77  MARCA-063    PIC 9  VALUE ZERO.                                  004
       77  MARCA-068    PIC 9  VALUE ZERO.                                  004
       77  MARCA-074    PIC 9  VALUE ZERO.                                  004
       77  MARCA-078    PIC 9  VALUE ZERO.                                  004
       77  MARCA-079    PIC 9  VALUE ZERO.                                  004
       77  MARCA-0791   PIC 9  VALUE ZERO.                                  004
       77  MARCA-0792   PIC 9  VALUE ZERO.                                  004
       77  MARCA-084    PIC 9  VALUE ZERO.                                  004
       77  MARCA-090    PIC 9  VALUE ZERO.                                  004
       77  MARCA-106    PIC 9  VALUE ZERO.                                  004
       77  MARCA-109    PIC 9  VALUE ZERO.                                  004
       77  MARCA-112    PIC 9  VALUE ZERO.                                  004
       77  MARCA-122    PIC 9  VALUE ZERO.                                  004
       77  MAR-M112     PIC XX VALUE 'NO'.                                  004
       77  MARCA-135    PIC 9  VALUE ZERO.                                  004
       77  MARCA-117    PIC 9  VALUE ZERO.                                  004
       77  MARCA-124    PIC 9  VALUE ZERO.                                  004
       77  MARCA-119    PIC 9  VALUE ZERO.                                  004
       77  MARCA-051    PIC 9  VALUE ZERO.                                  004
       77  MAR-C-060    PIC 9  VALUE ZERO.                                  004
       77  FIN          PIC AA  VALUE 'NO'.                                 004
       77  SI-074       PIC AA  VALUE 'NO'.                                 004
       77  M-NRO-ANT    PIC X(10)    VALUE SPACES.                          004
       77  FECHA-AUX    PIC X(6)     VALUE SPACES.
       77  AA           PIC 9(4)     VALUE ZEROS.
       77  CERO         PIC 9(4)     VALUE ZEROS.
      *
      *      DISEºO DE MAESTRO                                              007
      *                                                                     007
       01  MAE.                                                             007
           02 MARCA     PIC X.                                              007
           02 CONC      PIC XX.                                             007
           02 SEC.                                                          007
                03 SEC1  PIC X.                                             007
                03 SEC2  PIC X.                                             008
           02 NRO        PIC X(10).                                         008
           02 EME        PIC X.                                             008
           02 APYN-Y-PAGO.                                                  008
               03 APY     PIC X(27).                                        008
               03 PAGO.                                                     008
                  04 APOD PIC X.                                            008
                  04 LOCA PIC X(3).                                         008
           02 REAJUSTE REDEFINES APYN-Y-PAGO.                               008
               03 APYN    PIC X(23).                                        008
               03 FECHA-REA.
                  04 F-REA-AA  PIC X(4).
                  04 F-REA-MM  PIC X(2).
               03    F-REA-DD  PIC X(2).
           02 HABER       PIC S9(9)V99.                                     009
           02 IOMA        PIC S9(7)V99.                                     009
           02 PARENTESCO.                                                   009
              03 PARENT      PIC X.                                         009
              03 ESPOSA      PIC X.                                         009
              03 HIJO        PIC XX.                                        009
           02 PORCE-GRU.                                                    009
              03 PORC-G    PIC 9(3)V999.
           02 MARCA-IOMA     PIC X.
           02 GRUPO          PIC 9(7).                                      009
           02 M-ANTI         PIC S9(7)V99.                                  009
           02 SI-ES-VIAL     PIC X.                                         009
           02 SI-ES-ANTIG    PIC X.                                         009
           02 CODIGO.                                                       009
              03 M-1-8.                                                     009
                 04 POS-1-4.                                                009
                  05 POS-1-2.                                               009
                   06 POS-1       PIC X.                                    009
                   06 POS-2       PIC X.                                    009
                  05 POS-3        PIC X.                                    009
                  05 POS-4        PIC X.                                    009
                 04  POS-5-8.                                               009
                   05 POS-5-6.   
                    06 POS-5      PIC X.                                    009
                    06 POS-6      PIC X.                                    009
                   05 POS-7-8.                                              009
                     06 POS-7     PIC X.                                    009
                     06 POS-8     PIC X.                                    009
                 04  POS-5-8-R REDEFINES POS-5-8.     
                    06 FILLER     PIC X.                                    009
                    06 POS-6-8    PIC X(3).                                 009
                 04  POS-5-8-RR REDEFINES POS-5-8.     
                    06 POS-5-7    PIC X(3).                                 009
                    06 FILLER     PIC X.                                    009
               03 POS-9-10  PIC X(2).                                       009
               03 POS-11-12  PIC X(2).                                      009
               03 POS-13-14  PIC X(2).                                      009
               03 POS-13-14N REDEFINES POS-13-14 PIC 9(2).     
               03 POS-15-16.                                                009
                  04 POS-15  PIC X.                                         009
                  04 POS-16  PIC X.                                         009
               03 POS-15-16-N REDEFINES POS-15-16 PIC 99.                   009
               03 POS-17-20  PIC X(4).                                      009
           02 CODIGO2.                                                      009
               03 POS-117-127    PIC X(11).                                 009
               03 POS-128        PIC X.                                     009
               03 POS-129-131    PIC X(3).                                  009
           02 F     PIC X(6).
           02 POS-138-139   PIC XX.
           02 POS-140       PIC X.
       01  CONCEPTOS.
           02 REGIS  OCCURS 80.
             03 AM-MARCA   PIC X.                                           007
             03 AM-CONC    PIC XX.                                          007
             03 AM-SEC.                                                     007
                04 AM-SEC1  PIC X.                                          007
                04 AM-SEC2  PIC X.                                          008
             03 AM-NRO      PIC X(10).                                      008
             03 AM-EME      PIC X.                                          008
             03 AM-APYN-Y-PAGO.                                             008
                04 AM-APYN         PIC X(27).                               008
                04 AM-PAGO.                                                 008
                   05 AM-APOD      PIC X.                                   008
                   05 AM-LOCA      PIC X(3).                                008
             03 AM-REAJUSTE REDEFINES AM-APYN-Y-PAGO.                       008
                04 AM-APYN-R       PIC X(23).                               008
                04 AM-FECHA-R.                                              008
                   05 AM-F-REA-AA  PIC X(4).
                   05 AM-F-REA-MM  PIC X(2).
                04    AM-F-REA-DD  PIC X(2).
             03 AM-HABER      PIC S9(9)V99.                                 009
             03 AM-IOMA       PIC S9(7)V99.                                 009
             03 AM-PARENT     PIC X.                                        009
             03 AM-ESPOSA     PIC X.                                        009
             03 AM-HIJO       PIC XX.                                       009
             03 AM-CADUC      PIC X(6).                                     009
             03 AM-MARCA-IOMA PIC X.                                        009
             03 AM-GRUPO      PIC 9(7).                                     009
             03 AM-ANTIG      PIC S9(7)V99.                                 009
             03 AM-SI-ES-VIAL PIC X.                                        009
             03 AM-SI-ES-ANTIG PIC X.                                       009
             03 AM-CODIGO.                                                  009
              04 AM-1-10.                                                   009
                 05 AM-POS-1-4.                                             009
                  06 AM-POS-1-2.                                            009
                   07 AM-POS-1     PIC X.                                   009
                   07 AM-POS-2     PIC X.                                   009
                  06 AM-POS-3      PIC X.                                   009
                  06 AM-POS-4      PIC X.                                   009
                 05  AM-POS-5-8.                                            009
                   06  AM-POS-5-6.                
                     07  AM-POS-5      PIC X.   
                     07  AM-POS-6      PIC X.  
                   06  AM-POS-7      PIC X.                                 009
                   06  AM-POS-8      PIC X.                                 009
                 05 AM-POS-9-10  PIC X(2).                                  009
               04 AM-POS-11-12  PIC X(2).                                   009
               04 AM-POS-13-14  PIC X(2).                                   009
               04 AM-POS-15-16.                                             009
                  05 AM-POS-15  PIC X.                                      009
                  05 AM-POS-16  PIC X.                                      009
               04 AM-POS-17-20  PIC X(4).                                   009
             03 AMR-CODIGO REDEFINES AM-CODIGO.               
                04 AM-1-14    PIC X(14).        
                04 AM-15-20   PIC X(6).        
             03 AM-CODIGO2.                                                 009
                04 AM-COD2-1-4  PIC X(04).                                  009
                04 AM-COD2-5-15.                                            009
                   05 AM-COD2-5-8.                                            009
                     06 AM-COD2-56.                                            009
                       07 AM-COD2-5  PIC X(1).                                  009
                       07 AM-COD2-6  PIC X(1).                                  009
                     06 AM-COD2-78 PIC X(2).                                  009
                   05 F            PIC X(7).                                  009
             03 F                PIC X(6).
             03 AM-POS-138-139   PIC XX.
             03 AM-POS-140       PIC X.
      *
       01  REG-PARAM.
           02  COD-RETRO         PIC X.
           02  FECHA-MES.
               03 FE-ANIO        PIC X(4).
               03 FE-MES         PIC XX.
       PROCEDURE DIVISION.                                                  019
           COPY PDFECHA.CPY.
           OPEN INPUT MAESTRO                                               019
                OUTPUT MAEACT.
           ACCEPT REG-PARAM.
           IF COD-RETRO = 'C' OR 'R' OR 'M'
                        THEN NEXT SENTENCE
                        ELSE DISPLAY ' PARAMETRO INDICA RETRO ERRONEA'
                             DISPLAY ' DEBE SER     -M-  O  -R-      '
                             COMPUTE AA = AA / CERO
                             GO TO CIERRE.
           PERFORM BORRA THRU FIN-BORRA VARYING I FROM 1 BY 1 UNTIL
            I > 80.
           READ MAESTRO INTO MAE AT END MOVE 'SI' TO FIN.
           PERFORM PROCESO THRU FIN-PROCESO UNTIL FIN = 'SI'.
       CIERRE.                                                              017
           DISPLAY ' *** TOTALES DE CONTROL  PROG. BPAGAMUN ***'.           017
           DISPLAY 'TOTAL DE REGISTROS MAESTRO           ' T-MAELEI.        017
           DISPLAY 'TOTAL DE REGISTROS MAEACT            ' T-GRABA.         017
           DISPLAY 'TOTAL DE JUB-PEN   M004              ' T-M004.          017
           DISPLAY 'TOTAL DE JUB-PEN   M008              ' T-M008.          017
           DISPLAY 'TOTAL DE JUB-PEN   M010              ' T-M010.          017
           DISPLAY 'TOTAL DE JUB-PEN   M013              ' T-M013.          017
           DISPLAY 'TOTAL DE JUB-PEN   M013 A PARTIR 01-06 ' T-M013N.       017
           DISPLAY 'TOTAL DE JUB-PEN   M028              ' T-M028.          017
           DISPLAY 'TOTAL DE JUB-PEN   M056              ' T-M028.          017
           DISPLAY 'TOTAL DE JUB-PEN   M044              ' T-M044.          017
           DISPLAY 'TOTAL DE JUB-PEN   M060              ' T-M060.          017
           DISPLAY 'TOTAL DE JUB-PEN   M061              ' T-M061.          017
           DISPLAY 'TOTAL DE JUB-PEN   M062              ' T-M062.          017
           DISPLAY 'TOTAL DE JUB-PEN   M063              ' T-M063.          017
           DISPLAY 'TOTAL DE JUB-PEN   M068              ' T-M068.          017
           DISPLAY 'TOTAL DE JUB-PEN   M074              ' T-M074.          017
           DISPLAY 'TOTAL DE JUB-PEN   M078              ' T-M078.          017
           DISPLAY 'TOTAL DE JUB-PEN   M079              ' T-M079.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0791 02/09       ' T-M0791.         017
           DISPLAY 'TOTAL DE JUB-PEN   M084              ' T-M084.          017
           DISPLAY 'TOTAL DE JUB-PEN   M090              ' T-M090.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0106             ' T-M106.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0109             ' T-M109.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0112             ' T-M112.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0117             ' T-M117.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0119             ' T-M119.          017
           DISPLAY 'TOTAL DE JUB-PEN   M122              ' T-M122.          017
           DISPLAY 'TOTAL DE JUB-PEN   M124              ' T-M124.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0135             ' T-M135.          017
           DISPLAY 'TOTAL DE JUB-PEN   M0135             ' T-M051.          017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M010  ' T-MOD-010.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M013  ' T-MOD-013.       017
           DISPLAY 'TOT.REG. MODIF M013 A PARTIR 01-06   ' T-MOD-013N.      017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M044  ' T-MOD-044.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M049  ' T-MOD-049.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M060  ' T-MOD-060.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M061  ' T-MOD-061.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M063  ' T-MOD-063.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M074  ' T-MOD-074.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M078  ' T-MOD-078.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M079  ' T-MOD-079.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M084  ' T-MOD-079.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M090  ' T-MOD-090.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M106  ' T-MOD-106.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M109  ' T-MOD-109.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M1091  ' T-MOD-1091.     017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M112  ' T-MOD-112.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M117  ' T-MOD-117.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M119  ' T-MOD-119.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M122  ' T-MOD-122.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M124  ' T-MOD-124.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M135  ' T-MOD-135.       017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS M051  ' T-MOD-051.       017
           CLOSE MAESTRO  MAEACT.                                           032
           STOP RUN.                                                        032
       PROCESO.
           MOVE NRO OF MAE TO M-NRO-ANT.
           MOVE 0 TO MARCA-013 MARCA-079 MARCA-084 MARCA-106 HABER-AUX
                   MASI MARCA-078 BONI-063 IMPORTE IMPORTE2 MAR-C-060
                   MARCA-135 MARCA-112 HABER-ANT VALOR-112 MARCA-090 
                   MARCA-074 MARCA-010 MARCA-044 MARCA-117 MARCA-060 
                 MARCA-028 SUMA-M028 Z IOMA-AUX MARCA-119 MARCA-0791                        
                   MARCA-056 SUMA-M056 SUMA-M008  MARCA-008 MARCA-033                       
                    MARCA-109  IMPOR109 IMPOR1091 IMPORTE033 MARCA-124
                   MARCA-063  VALOR-117 PORC SUMA-ORD  SUMA-M060 K  
                   T I63 I10 I62 I44 I106 I117 I84 I13 I68 I117
                   MARCA-004 MARCA-062 MARCA-0792 MARCA-068 MARCA-049  
                   MARCA-061 MARCA-004-14 SUMA-M004 SUMA-M004F2                                      
                   SUMA-M0049F2 MARCA-051 FIJA2-AUX1 FIJA2-AUX MARCA-122.
           MOVE ZEROS TO HABM004-ACT HABM004-ANT. 
           MOVE ZEROS TO BONI-068 MIN068.
           MOVE 0 TO CARGO-M060.                                           
           IF COD-RETRO = 'M'
                    THEN
           MOVE FECHA-MES TO FECHA-AUX
           PERFORM LEE-Y-CARGA THRU FIN-LEE-Y-CARGA UNTIL NRO OF MAE
              NOT = M-NRO-ANT OR FIN = 'SI'
                    ELSE
           MOVE FECHA-REA TO FECHA-AUX
           PERFORM LEE-Y-CARGA THRU FIN-LEE-Y-CARGA UNTIL FECHA-AUX NOT
            = FECHA-REA OR  NRO OF MAE NOT = M-NRO-ANT OR FIN = 'SI'.
  ********    OJO CUANDO SE AGREGA UNA muni NUEVA SE DEBE VER SI PUEDE COBRAR SIMULTANEOS
  *************   DE OTRA mun  (POR EL NIDO DE if)
           IF MASI = '1'
              THEN GO TO AGRABAR.
 ********     ELSE NEXT SENTENCE
           IF MARCA-004 = 1
              THEN  ADD 1 TO T-M004
                    PERFORM SUMA-004 THRU FIN-SUMA-004.
           IF MARCA-004-14 = 1
              THEN  ADD 1 TO T-M004
                    PERFORM SUMA-004-14 THRU FIN-SUMA-004-14.
  *******     ELSE
           IF MARCA-008 = 1
              THEN  ADD 1 TO T-M008
                    PERFORM SUMA-008 THRU FIN-SUMA-008
              ELSE
           IF MARCA-010 = 1
              THEN  ADD 1 TO T-M010
                    PERFORM SUMA-010 THRU FIN-SUMA-010
              ELSE
           IF MARCA-013 = 1
              THEN                               
                    ADD 1 TO T-M013
                    PERFORM SUMA-013 THRU FIN-SUMA-013
              ELSE
           IF MARCA-028 = 1
              THEN  ADD 1 TO T-M028
                    PERFORM SUMA-028 THRU FIN-SUMA-028
              ELSE
           IF MARCA-033 = 1
              THEN  ADD 1 TO T-M033
                    PERFORM SUMA-033 THRU FIN-SUMA-033
              ELSE
           IF MARCA-056 = 1
              THEN  ADD 1 TO T-M056
                    PERFORM SUMA-056 THRU FIN-SUMA-056
              ELSE
           IF MARCA-044 = 1
              THEN  ADD 1 TO T-M044
                    PERFORM SUMA-044 THRU FIN-SUMA-044
              ELSE
           IF MARCA-060 = 1
              THEN  ADD 1 TO T-M060
                    PERFORM SUMA-060 THRU FIN-SUMA-060
              ELSE
           IF MARCA-062 = 1
              THEN  ADD 1 TO T-M062
                    PERFORM SUMA-062 THRU FIN-SUMA-062
              ELSE
           IF MARCA-063 = 1
              THEN  ADD 1 TO T-M063
                    PERFORM SUMA-063 THRU FIN-SUMA-063
              ELSE
           IF MARCA-068 = 1
              THEN  ADD 1 TO T-M068
                    PERFORM SUMA-068 THRU FIN-SUMA-068
              ELSE
           IF MARCA-074 = 1
              THEN  ADD 1 TO T-M074
                   PERFORM SUMA-074 THRU FIN-SUMA-074
              ELSE
           IF MARCA-078 = 1
              THEN  ADD 1 TO T-M078
                   PERFORM SUMA-078 THRU FIN-SUMA-078
              ELSE   
           IF MARCA-079 = 1
              THEN  ADD 1 TO T-M079
                   PERFORM SUMA-079 THRU FIN-SUMA-079
              ELSE   
           IF MARCA-0791 = 1
              THEN  ADD 1 TO T-M0791
                   PERFORM P200-M079 THRU FIN-P200-M079
              ELSE   
           IF MARCA-0792 = 1
              THEN  ADD 1 TO T-M0791
                   PERFORM P150-M079 THRU FIN-P150-M079
              ELSE
           IF MARCA-084 = 1
              THEN  ADD 1 TO T-M084
                   PERFORM SUMA-084 THRU FIN-SUMA-084
              ELSE   
           IF MARCA-090 = 1
              THEN  ADD 1 TO T-M090
                   PERFORM SUMA-090 THRU FIN-SUMA-090
              ELSE
           IF MARCA-106 = 1
              THEN  ADD 1 TO T-M106
                   PERFORM SUMA-106 THRU FIN-SUMA-106
              ELSE
           IF MARCA-109 = 1
              THEN  ADD 1 TO T-M109
                   PERFORM SUMA-109 THRU FIN-SUMA-109
              ELSE
           IF MARCA-112 = 1
              THEN  ADD 1 TO T-M112
                   PERFORM SUMA-112 THRU FIN-SUMA-112
              ELSE
           IF MARCA-117 = 1
              THEN  ADD 1 TO T-M117
                   PERFORM SUMA-117 THRU FIN-SUMA-117
              ELSE
           IF MARCA-119 = 1
              THEN  ADD 1 TO T-M119
                   PERFORM SUMA-119 THRU FIN-SUMA-119
              ELSE
           IF MARCA-122 = 1
              THEN  ADD 1 TO T-M122
                   PERFORM SUMA-122 THRU FIN-SUMA-122
              ELSE   
           IF MARCA-135 = 1
              THEN  ADD 1 TO T-M135
                   PERFORM SUMA-135 THRU FIN-SUMA-135  
              ELSE   
           IF MARCA-061 = 1
              THEN  ADD 1 TO T-M061
                   PERFORM SUMA-061 THRU FIN-SUMA-061  
              ELSE   
           IF MARCA-124 = 1
              THEN  ADD 1 TO T-M124
                   PERFORM SUMA-124 THRU FIN-SUMA-124  
              ELSE   
           IF MARCA-049 = 1
              THEN  ADD 1 TO T-M049
                   PERFORM SUMA-049 THRU FIN-SUMA-049  
              ELSE   
           IF MARCA-051 = 1
              THEN  ADD 1 TO T-M051
                   PERFORM SUMA-051 THRU FIN-SUMA-051. 
       AGRABAR.     
           PERFORM GRABA THRU FIN-GRABA VARYING I FROM 1 BY 1
                UNTIL I > T-REG.
           PERFORM BORRA THRU FIN-BORRA VARYING I FROM 1 BY 1 UNTIL
            I > T-REG.
           MOVE ZEROS TO T-REG.
       FIN-PROCESO.
           EXIT.
       LEE-Y-CARGA.
           ADD 1 TO T-REG.
           IF T-REG NOT > 80
              THEN MOVE MAE TO REGIS (T-REG)
              ELSE DISPLAY 'CANTIDAD DE CONCEPTOS SUPERIOR A 80 '
                   NRO OF MAE
                   GO TO LEER.
           IF POS-1 NOT = 'M'
              THEN  GO TO LEER.
           IF POS-140 = '*'
               THEN IF (CONC = '01' AND EME = SPACE) OR SEC2 = 'P'
                    OR (CONC = '02' AND EME = SPACE)                  
                       THEN  MOVE 1   TO MASI
                             GO TO LEER.
           IF (CONC = '01' AND EME = ' ') OR
                  (CONC = '02' AND EME = ' '  AND POS-1 NOT = '*')
                       THEN NEXT SENTENCE
                       ELSE GO TO LEER.
   ******
           IF POS-1-4 = 'M074'                             
              THEN IF FECHA-AUX < '200408' THEN GO TO LEER
                              ELSE
                              IF MARCA-074 = 0
                                         THEN MOVE 1 TO MARCA-074
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER.
           IF POS-1-4 = 'M004'
              THEN IF FECHA-AUX = '200902' OR '200903' OR '200904'                 
                               OR '201005' OR '201006' OR '201007'                 
                               OR '201102' OR '201103' OR '201104'                 
                           THEN
                              PERFORM CONSULTA-M004 THRU
                                                FIN-CONSULTA-M004      
                              GO TO LEER
                           ELSE
                    IF FECHA-AUX > '201403' AND FECHA-AUX < '201609'                                         
                           THEN
                              PERFORM CONSULTA-M004-14 THRU
                                                FIN-CONSULTA-M004-14      
                              GO TO LEER
                           ELSE GO TO LEER.
           IF POS-1-4 = 'M008'
              THEN IF FECHA-AUX < '200902' OR FECHA-AUX > '201006'
                THEN  GO TO LEER
                ELSE
                      IF MARCA-008 = 0
                                         THEN MOVE 1 TO MARCA-008
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER.
           IF POS-1-4 = 'M010'
              THEN IF FECHA-AUX < '200410' THEN GO TO LEER
                              ELSE
                              IF MARCA-010 = 0
                                         THEN MOVE 1 TO MARCA-010
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER.
           IF POS-1-4 = 'M033'
              THEN IF FECHA-AUX NOT = '200810' THEN GO TO LEER
                                               ELSE                                  
                              IF MARCA-033 = 0
                                      THEN IF POS-5 = '9'             
                                         THEN GO TO LEER
                                         ELSE MOVE 1 TO MARCA-033
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                      ELSE GO TO LEER.
           IF POS-1-4 = 'M106'
              THEN IF FECHA-AUX < '199611' THEN GO TO LEER
                                  ELSE
                              IF MARCA-106 = 0
                                         THEN MOVE 1 TO MARCA-106
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER.
           IF POS-1-4 = 'M109'
              THEN IF FECHA-AUX < '200710' THEN GO TO LEER
                                           ELSE IF MARCA-109 = 0
                            THEN IF (POS-5 = '9' OR POS-5-8 = 'S050')
                                         THEN GO TO LEER
                                         ELSE MOVE 1 TO MARCA-109
                                              MOVE POS-15-16-N TO PORC
                                              MOVE POS-13-14N TO ANTI
                                              GO TO LEER
                            ELSE GO TO LEER.

           IF POS-1-4 = 'M079' AND FECHA-AUX = '200903'
                 THEN IF (POS-5 = 'S' OR 'C' OR 'M' OR 'T' OR 'F')             
                           OR (POS-5-6 = 'RR' OR 'AC')
                        THEN GO TO LEER 
                        ELSE IF MARCA-0791 = 0
                                       THEN MOVE 1 TO MARCA-0791
                                            MOVE POS-15-16-N TO PORC
                                            GO TO LEER
                                       ELSE GO TO LEER  
                 ELSE
           IF POS-1-4 = 'M079' AND FECHA-AUX = '200904'
                 THEN IF (POS-5-8 = 'S479' OR 'M001' OR 'M002')             
                        THEN IF MARCA-0791 = 0
                                    THEN MOVE 1 TO MARCA-0791
                                         MOVE POS-15-16-N TO PORC
                                         GO TO LEER
                                    ELSE GO TO LEER  
                        ELSE GO TO LEER  
                 ELSE
           IF POS-1-4 = 'M079' AND FECHA-AUX = '200905'
                 THEN IF POS-5-8 = 'S303' OR 'S304'    
                        THEN IF MARCA-0791 = 0
                                    THEN MOVE 1 TO MARCA-0791
                                         MOVE POS-15-16-N TO PORC
                                         GO TO LEER
                                    ELSE GO TO LEER. 


           IF POS-1-4 = 'M079'
              THEN IF FECHA-AUX < '199512' OR FECHA-AUX > '200403'
                              THEN NEXT SENTENCE
                              ELSE
                              IF MARCA-079 = 0
                                       THEN MOVE 1 TO MARCA-079
                                            MOVE POS-15-16-N TO PORC
                                            GO TO LEER
                                       ELSE GO TO LEER.

           IF POS-1-4 = 'M079'
              THEN IF FECHA-AUX < '201004'                                          
                        THEN GO TO LEER
                        ELSE
                    IF ((POS-5-8 = 'RRRR' OR 'S050' OR 'S252' OR 'S316'                             
                      OR 'S317' OR 'S454' OR 'S463' OR 'S004' OR 'S007'                           
                      OR 'S499' OR 'S612' OR 'S504' OR 'S122' OR 'SCOU'                           
                      OR 'ACRI' OR '311A' OR '311B' OR '311C') OR                          
                           (POS-5 = 'T' OR '9' OR 'C' OR 'F'))                          
                              THEN GO TO LEER
                              ELSE
                              IF MARCA-0792 = 0
                                       THEN MOVE 1 TO MARCA-0792
                                            MOVE POS-15-16-N TO PORC
                                            GO TO LEER
                                       ELSE GO TO LEER.


              IF POS-1-4 = 'M078' 
               THEN IF FECHA-AUX < '200505' OR FECHA-AUX > '200511'
                         THEN GO TO LEER
                         ELSE
                           IF POS-5 = 'S' OR (POS-5-6 ='9H' OR '9B' 
                              OR '9E' OR '9I' OR '9K')
                             THEN GO TO LEER
                             ELSE 
                             IF MARCA-078 = 0
                                       THEN MOVE 1 TO MARCA-078
                                            MOVE POS-15-16-N TO PORC
                                            GO TO LEER
                                       ELSE GO TO LEER.                           

            IF POS-1-4 = 'M084' AND ( POS-5 = '0' OR
                          POS-5-8 = 'S479' )     
                   THEN IF FECHA-AUX < '200509'                        
                         THEN GO TO LEER
                         ELSE
                         IF MARCA-084 = 0
                                       THEN MOVE 1 TO MARCA-084
                                            MOVE POS-15-16-N TO PORC 
                                            GO TO LEER
                                       ELSE GO TO LEER.                           

           IF POS-1-4 = 'M090'
              THEN IF FECHA-AUX < '199902' OR FECHA-AUX > '200501'
                              THEN GO TO LEER
                              ELSE
                              IF MARCA-090 = 0
                                         THEN MOVE 1 TO MARCA-090
                                              MOVE POS-15-16-N  TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER.
           IF POS-1-4 = 'M135'
              THEN IF FECHA-AUX < '199512'                     
                         THEN GO TO LEER
                         ELSE
                              IF MARCA-135 = 0
                                         THEN MOVE 1 TO MARCA-135
                                              MOVE POS-15-16-N TO PORC
                                              GO TO LEER
                                         ELSE GO TO LEER. 
      *
           IF (POS-1-4 = 'M117' AND POS-5 NOT = 'S') OR
              (POS-1-4 = 'M117' AND (POS-5-8 = 'S480' OR 'S479'))
              THEN IF FECHA-AUX < '200502'                
                       THEN GO TO LEER
                       ELSE
                        IF MARCA-117 = 0
                            THEN
                            IF POS-15-16 > '00' AND POS-15-16-N > PORC
                                    THEN MOVE 1 TO MARCA-117
                                         MOVE POS-15-16-N TO PORC
                                         GO TO LEER
                                    ELSE GO TO LEER 
                            ELSE GO TO LEER. 
      *
           IF (POS-1-4 = 'M062' AND POS-5 NOT = 'S')     
              THEN IF FECHA-AUX < '201002'                
                       THEN GO TO LEER
                       ELSE
                        IF MARCA-062 = 0
                            THEN
                            IF POS-15-16 > '00' AND POS-15-16-N > PORC
                                    THEN MOVE 1 TO MARCA-062
                                         MOVE POS-15-16-N TO PORC
                                         GO TO LEER
                                    ELSE GO TO LEER 
                            ELSE GO TO LEER. 
**************     para la M060    ******************************************
           IF POS-1-4 = 'M060'                              
              THEN IF FECHA-AUX < '200509'                
                  THEN GO TO LEER
                  ELSE
                  IF POS-5-6 = 'BF' OR 'BC'                   
                       THEN GO TO LEER
                       ELSE
                  IF POS-5-8 = 'S252' OR 'S050' OR 'S450' OR
                 'S484' OR 'S121' OR 'S003' OR 'S020' OR 'S454' OR
                 'S463' OR 'S480' OR 'S488' OR 'S464' OR 'S491' OR
                 'S122' OR 'S401' OR 'S105' OR 'S009' OR 'S025' OR
                 'S026' OR 'S305' OR 'S051' OR 'S060' OR 'S103' OR
                 'S501' OR 'S147'                                      
                       THEN GO TO LEER
                       ELSE
                       IF POS-5-8 = 'TS00' OR 'TT00' OR 'TU00'                     
                              THEN 
                      COMPUTE SUMA-M060 = SUMA-M060 +  AM-HABER (T-REG)                          
                                          + AM-ANTIG (T-REG)
                              ELSE                          
                      COMPUTE SUMA-M060 = SUMA-M060 +  AM-HABER (T-REG)                          
                                          + AM-ANTIG (T-REG)
                       IF FECHA-AUX > '200801'                           
                                THEN IF (POS-5 = 'E' OR 'B' OR 'H') OR              
                                               (POS-5-6 = 'N0' OR 'TE')
                                       THEN MOVE 1 TO MARCA-060
                                       ELSE GO TO LEER       
                                 ELSE MOVE 1 TO MARCA-060
              ELSE GO TO ALA63.
       MAS060.
             IF FECHA-AUX > '200606'
                      THEN IF POS-5 = 'N' AND               
                                  (POS-6-8 < '014' OR                   
                                   POS-6-8 = '00A' OR 
                                   POS-6-8 = '00B')   
                                 THEN MOVE 1 TO MAR-C-060
                                      MOVE POS-15-16-N TO PORC
                                      GO TO LEER     
                                 ELSE GO TO LEER     
                      ELSE GO TO LEER.

**************     para la M060    ******************************************
       ALA63.
           IF POS-1-4 = 'M063'                              
              THEN IF FECHA-AUX < '200510'                
                      THEN GO TO LEER
                      ELSE
                      IF POS-15-16 > '00' AND POS-15-16-N > PORC
                                    THEN MOVE 1 TO MARCA-063
                                         MOVE POS-15-16-N  TO PORC
                                         PERFORM CONSULTA-M063 THRU
                                             FIN-CONSULTA-M063      
                                         GO TO LEER
                                    ELSE GO TO LEER.


           IF POS-1-4 = 'M112' AND POS-5-8 NOT = 'S050'
              THEN IF FECHA-AUX < '199902'                        
                      THEN GO TO LEER
                      ELSE 
                         IF MARCA-112 = 0
                                    THEN MOVE 1 TO MARCA-112
                                         MOVE POS-15-16-N  TO PORC 
                                         GO TO LEER
                                    ELSE GO TO LEER.
      *
           IF POS-1-4 = 'M044' AND ( POS-5 = '2' OR '3' OR '4' OR '5'
                                                 OR '6' OR '7' )     
              THEN IF FECHA-AUX < '200404'                    
                         THEN GO TO LEER
                         ELSE
                         IF MARCA-044 = 0
                                    THEN MOVE 1 TO MARCA-044
                                         MOVE POS-15-16-N TO PORC 
                                         GO TO LEER
                                    ELSE GO TO LEER.
      *
  ******************************  A PARTIR DE ESTE PUNTO SOLO M013
  ************* 'GENERA EL DESCUENTO PARA LA M013 DE UN 7.5 0 10%'.         000
  ************* ' SUPERANDO LOS 501 O 900  RESPECTIVAMENTE       '.         000
      *
           IF POS-1-4 = 'M013'
              THEN                     
             IF FECHA-AUX < '199602'                    
                         THEN GO TO LEER 
                         ELSE
              IF FECHA-AUX > '199901' 
                        AND FECHA-AUX < '200512'
                               THEN GO TO LEER 
                               ELSE
                               IF MARCA-013 = 0
                                         THEN MOVE 1 TO MARCA-013
                                              MOVE POS-15-16-N TO PORC 
                                              GO TO LEER
                                         ELSE GO TO LEER.

           IF POS-1-4 = 'M028'
              THEN                    
             IF FECHA-AUX < '200709'                  
                            THEN GO TO LEER 
                            ELSE
                            IF MARCA-028 = 0
                                  THEN MOVE 1 TO MARCA-028
                                       MOVE POS-15-16-N TO PORC 
                                       MOVE T-REG TO Z                             
                                       COMPUTE IOMA-AUX =                              
                        ((AM-HABER (T-REG) + AM-ANTIG (T-REG)) * 0.048) 
                                       COMPUTE SUMA-M028 =                          
                                          SUMA-M028 +  AM-HABER (T-REG)                       
                                          + AM-ANTIG (T-REG) - IOMA-AUX 
                                       GO TO LEER
                                  ELSE GO TO LEER.

           IF POS-1-4 = 'M056'
              THEN                    
             IF FECHA-AUX < '200704'                  
                            THEN GO TO LEER 
                            ELSE
                            IF MARCA-056 = 0
                                  THEN MOVE 1 TO MARCA-056
                                       MOVE POS-15-16-N TO PORC 
                                       MOVE T-REG TO Z                             
                      COMPUTE SUMA-M056 = SUMA-M056 +  AM-HABER (T-REG)                        
                                          + AM-ANTIG (T-REG)   
                                       GO TO LEER
                                  ELSE GO TO LEER.

           IF POS-1-4 = 'M068'                                  
              THEN IF FECHA-AUX > '200709' AND FECHA-AUX < '200809'                       
                   THEN
                   IF  POS-5 = 'S' OR '9' OR 'C' 
                                THEN GO TO LEER 
                                ELSE
                                     MOVE 1 TO MARCA-068
                                     PERFORM CONSULTA-M068 THRU
                                       FIN-CONSULTA-M068      
                    ELSE GO TO LEER. 

           IF POS-1-4 = 'M119' AND (( POS-5 = '1' OR '2' OR '3' 
                   OR '4' OR 'M' OR '5' OR '6' ) OR 
                   (POS-5-8 ='S304' OR 'S303' OR 'S3S3' OR 'S3D3' 
                     OR 'S3S4'))   
              THEN IF FECHA-AUX < '200708'                       
                          THEN GO TO LEER 
                          ELSE
                          IF MARCA-119 = 0
                                    THEN MOVE 1 TO MARCA-119
                                         MOVE POS-15-16-N TO PORC                                                  
                                    ELSE GO TO LEER.
           IF POS-1-4 = 'M124'                                       
              THEN IF FECHA-AUX < '201308'                       
                          THEN GO TO LEER 
                          ELSE
                          IF MARCA-124 = 0
                                THEN
                          IF POS-15-16 > '00' AND POS-15-16-N > PORC
                                    THEN MOVE 1 TO MARCA-124
                                         MOVE POS-15-16-N TO PORC 
                                    ELSE GO TO LEER 
                                ELSE GO TO LEER.

           IF POS-1-4 = 'M049'                                      
              THEN IF FECHA-AUX < '201004'                       
                          THEN GO TO LEER 
                          ELSE
                  IF (POS-5-8 ='S252' OR 'S450' OR 'S600' OR 'S484' 
                            OR 'S100' OR 'S171' OR 'S101' OR 'S102' 
                            OR 'S601' OR 'S168' OR 'S099' OR 'S050' 
                            OR 'S191' OR 'S192' OR 'S176' OR 'S147' 
                            OR 'S204')   
                          THEN GO TO LEER 
                          ELSE
                 IF (POS-5 = 'O' OR 'G' OR 'M' OR 'B' OR '7') 
                          THEN GO TO LEER 
                          ELSE
                 ADD 1 TO MARCA-049
                 IF POS-11-12 > ZEROS
                          THEN ADD 1 TO MARCA-049
                          ELSE
                            MOVE T-REG TO I049
                            MOVE POS-15-16-N TO PORC.                                                  

           IF POS-1-4 = 'M051'                                      
              THEN IF FECHA-AUX < '201204'                       
                          THEN GO TO LEER 
                          ELSE
                 IF POS-5 = '9' OR         
                    (POS-5-8 = 'S108' OR 'S114' OR 'S141' OR 
                    'S478' OR 'S115' OR 'S060' OR 'S309')
                          THEN ADD 1 TO MARCA-051
                               MOVE T-REG TO I051
                               MOVE POS-15-16-N TO PORC                                                   
                          ELSE
                            IF POS-5-7 = 'BGM' OR 'BAG'
                              THEN ADD 2 TO MARCA-051.
********   agrego 10-6-16
           IF POS-1-4 = 'M122'                                      
              THEN IF FECHA-AUX < '201602'                       
                          THEN GO TO LEER 
                          ELSE
                 IF (POS-5 = '2' OR '3' OR '4' OR '5' OR '6')         
                     OR (POS-5-8 = 'S303' OR 'S304' OR 'S479'     
                           OR 'S451')
                     OR (POS-5-6 = '93')                                   
                     OR (POS-5-6 = '96' AND POS-7-8 <> '20')                                   
                          THEN
****************************  me quedo con el porc mas grande desde 2016/09  
                          IF POS-15-16 > '00' AND POS-15-16-N > PORC
                          THEN MOVE 1 TO MARCA-122
                               MOVE POS-15-16-N TO PORC.                                                  

     **    IF POS-1-4 = 'M061' AND (POS-5 = 'B' AND POS-6 NOT = 'O') lo saco en mov jun13 q se aplican aum de pptos ene y mar13. 
     **            THEN         
      *            IF FECHA-AUX = '201302' OR '201303' OR '201304'                       
      *                THEN
      *                IF MARCA-061 = 0
      *                   THEN
      *                   IF POS-15-16 > '00' AND POS-15-16-N > PORC
      *                         THEN MOVE 1 TO MARCA-061
      *                              MOVE POS-15-16-N TO PORC                                                  
      *                         ELSE GO TO LEER 
      *                 ELSE GO TO LEER. 
       LEER.
           PERFORM LEER-MAESTRO THRU FIN-LEER-MAESTRO.
       FIN-LEE-Y-CARGA.
           EXIT.
       LEER-MAESTRO.
           ADD 1 TO T-MAELEI.
           READ MAESTRO INTO MAE AT END MOVE 'SI' TO FIN
                                    MOVE '9999999999' TO NRO OF MAE.
       FIN-LEER-MAESTRO.
           EXIT.
       BORRA.
           MOVE SPACES TO REGIS (I).
       FIN-BORRA.
           EXIT.
       CONSULTA-M004.
           IF POS-5-8 = 'S479' OR 'S480' OR 'S309' OR 'S310'  
                    OR  'S3S4' OR 'S304' OR 'S3S3' OR 'S303' 
                    OR  'S161'                                     
                    THEN NEXT SENTENCE
                    ELSE
           IF POS-5 = '7' OR 'M' OR '5' OR '6' OR '9' OR '4' OR '8'  
                          OR '3' OR '2' OR 'N' OR '1'  
                    THEN NEXT SENTENCE
                    ELSE GO TO FIN-CONSULTA-M004.
           MOVE 1   TO MARCA-004.
           IF CONC = '01' AND EME = ' '
                THEN MOVE POS-15-16-N TO PORCM004 
                     IF COD-RETRO = 'C' THEN
                             IF PORC = ZEROS                             
                                THEN MOVE 100 TO PORCM004.
     ****   IF PORC = ZEROS
     ****           THEN MOVE POS-15-16-N TO PORC. 
       FIN-CONSULTA-M004. EXIT.
       SUMA-004.      
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           IF FECHA-AUX = '201102' OR '201103' OR '201104'
           THEN COMPUTE AM-HABER (T-REG) = 300 * PORCM004 / 100 
                MOVE '*DTO.154-11' TO AM-CODIGO (T-REG) 
           ELSE COMPUTE AM-HABER (T-REG) = 200 * PORCM004 / 100 
                MOVE '*DTO.1029-09/1510-10' TO AM-CODIGO (T-REG).
       FIN-SUMA-004.     
           EXIT.

       CONSULTA-M004-14.
           IF POS-5-8 = 'S479' OR 'S480' OR 'S309' OR 'S310'  
                    OR  'S3S4' OR 'S304' OR 'S3S3' OR 'S303' 
                    OR  'S161'                                     
                    OR 'S200' OR 'S153' OR 'S154'
                    THEN NEXT SENTENCE
                    ELSE
           IF POS-5 = '7' OR 'M' OR '5' OR '6' OR '4' OR '8'  
                          OR '9' OR '3' OR '2' OR 'N' OR '1'  
                    THEN NEXT SENTENCE
                    ELSE GO TO FIN-CONSULTA-M004-14.
           MOVE 1   TO MARCA-004-14.
       FIN-CONSULTA-M004-14. EXIT.
       SUMA-004-14.      
           PERFORM ACUMULA-M004 THRU FIN-ACUMULA-M004                
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF FIJA2-AUX > ZEROS
                    THEN                                   
                 COMPUTE FIJA2-AUX1 = SUMA-M0049F2 - FIJA2-AUX 
              COMPUTE HABM004-ANT = ((SUMA-M004 + FIJA2-AUX) / 1.37                                   
                               + FIJA2-AUX1 ) * 1.31 
                    ELSE
           COMPUTE HABM004-ANT = (SUMA-M004 / 1.37  + SUMA-M004F2)                   
                                                         * 1.31.
  ******** DISPLAY 'BENEF ' AM-NRO (1).   
  ******** DISPLAY 'SUMA-M004         ' SUMA-M004.   
 **********DISPLAY 'SUMAFIJ2t ' SUMA-M004F2.   
  *********DISPLAY 'FIJA2-AUXi(40) ' FIJA2-AUX.                       
  *********DISPLAY 'FIJA2-AUX1 parte fija    ' FIJA2-AUX1.                       
  *********DISPLAY 'haber ant C/ 1.31 ' HABM004-ANT. 
  *********DISPLAY 'haber ACT         ' HABM004-ACT.
           IF HABM004-ACT > HABM004-ANT 
                       THEN GO TO FIN-SUMA-004-14.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE '*DTO.723/14-3' TO AM-CODIGO (T-REG).
           COMPUTE AM-HABER (T-REG) = HABM004-ANT - HABM004-ACT.
********   DISPLAY 'DIF HABER ' AM-HABER (T-REG).
       FIN-SUMA-004-14.     
           EXIT.
       ACUMULA-M004.
           MOVE ' '   TO VER-M004.                     
            PERFORM CONSULTA-M00414AUX THRU FIN-CONSULTA-M00414AUX.
            IF VER-M004 = '*'
                        THEN NEXT SENTENCE
                        ELSE GO TO FIN-ACUMULA-M004.
           IF (AM-CONC (I) = '01' OR '02')  AND  AM-EME (I) = ' '                         
                              AND  AM-POS-1-4 (I) = 'M004'                         
                THEN COMPUTE SUMA-M004 = SUMA-M004 + AM-HABER (I)    
                                                   + AM-ANTIG (I) 
                 COMPUTE HABM004-ACT = HABM004-ACT + AM-HABER (I)    
                                                   + AM-ANTIG (I) 
           IF AM-COD2-56 (I) = '9B' OR '9E' OR '9H' OR '9I' OR '9K'                         
                     THEN
               COMPUTE FIJA2-AUX = FIJA2-AUX + (AM-HABER (I) * 0.40).   
 ************ acumula los supl2                                                        
           IF AM-CONC (I) = '02' AND  AM-COD2-1-4 (I) = 'M004'                         
              AND AM-1-14 (I) = '*BON.MUN.SUPL2' 
                THEN COMPUTE SUMA-M004 = SUMA-M004 + AM-HABER (I)    
                 COMPUTE HABM004-ACT = HABM004-ACT + AM-HABER (I).   

           IF AM-CONC (I) = '02' AND  AM-COD2-1-4 (I) = 'M004'                         
              AND AM-1-14 (I) = '*BON.MUN.SUMF2' 
                    THEN
                COMPUTE HABM004-ACT = HABM004-ACT + AM-HABER (I)    
           IF AM-COD2-56 (I) = '9B' OR '9E' OR '9H' OR '9I' OR '9K'                         
                THEN COMPUTE SUMA-M0049F2 = SUMA-M0049F2 + AM-HABER (I)    
                ELSE COMPUTE SUMA-M004F2 = SUMA-M004F2 + AM-HABER (I).   
       FIN-ACUMULA-M004. EXIT.
 *******
       CONSULTA-M00414AUX.
           IF AM-COD2-1-4 (I) = 'M004'                         
                         THEN NEXT SENTENCE
                         ELSE GO TO FIN-CONSULTA-M00414AUX.
           IF AM-COD2-5-8 (I) = 'S479' OR 'S480' OR 'S309' OR 'S310'  
                    OR  'S3S4' OR 'S304' OR 'S3S3' OR 'S303' 
                    OR  'S161'                                     
                    OR 'S200' OR 'S153' OR 'S154'
                    THEN NEXT SENTENCE
                    ELSE
           IF AM-COD2-5 (I) = '7' OR 'M' OR '5' OR '6' OR '4'                        
                      OR '8' OR '9' OR '3' OR '2' OR 'N' OR '1'  
                    THEN NEXT SENTENCE
                    ELSE GO TO FIN-CONSULTA-M00414AUX.
           MOVE '*'   TO VER-M004.                     
       FIN-CONSULTA-M00414AUX. EXIT.

       CONSULTA-M063.
            IF FECHA-AUX > '201607'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'
                          OR  'SS00' OR 'SU51'
                  THEN MOVE 21978  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'
                            OR 'S484'
                  THEN MOVE 19573  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3'                          
                  THEN MOVE 16854  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'
                  THEN MOVE 11141  TO BONI-063
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
            IF FECHA-AUX > '201601'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'
                          OR  'SS00' OR 'SU51'
                  THEN MOVE 19980  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'
                            OR 'S484'
                  THEN MOVE 17794  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3'                          
                  THEN MOVE 15322  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'
                  THEN MOVE 10128  TO BONI-063
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201507'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 16649  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                            OR 'S484'
                  THEN MOVE 14828  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3' OR 'S118'                               
                  THEN MOVE 12768  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 8440   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201501'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 15135  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                            OR 'S484'
                  THEN MOVE 13480  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3' OR 'S118'                               
                  THEN MOVE 11607  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 7673   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201401'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 12612  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                            OR 'S484'
                  THEN MOVE 11232  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3' OR 'S118'                               
                  THEN MOVE 9672   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 6394   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201301'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 10510  TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                            OR 'S484'
                  THEN MOVE 9360   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3' OR 'S118'                               
                  THEN MOVE 8060   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 5328   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201201'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 8760   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                  THEN MOVE 7800   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 4440   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '201107'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 7300   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                  THEN MOVE 6500   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S100'                                
                  THEN MOVE 3700   TO BONI-063 
                  ELSE GO TO FIN-CONSULTA-M063
              ELSE
           IF FECHA-AUX > '200601'
              THEN
               IF POS-5-8 = 'S450' OR 'S051' OR 'SU00' OR 'ST00'  
                          OR  'SS00' OR 'SU51' 
                  THEN MOVE 2800   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S485' OR 'S486' OR 'S105' OR 'S106'  
                  THEN MOVE 2300   TO BONI-063
                  ELSE
               IF POS-5-8 = 'S103' OR 'S1S3'                      
                  THEN MOVE 2000   TO BONI-063.
       FIN-CONSULTA-M063.
           EXIT.
       CONSULTA-M068.
               IF CONC = '01' AND EME = ' '
                        THEN NEXT SENTENCE
                        ELSE
                        IF CONC = '02' AND BONI-068 = ZEROS
                             THEN NEXT SENTENCE
                             ELSE GO TO FIN-CONSULTA-M068.
               IF POS-5-8 = '5043' OR '5643' OR '6043' OR '700D'  
                        OR  '2003' OR '300D' OR '400D' OR '5605' 
                        OR  '6040' OR '6643'                            
                  THEN MOVE 70     TO BONI-068
                  ELSE
               IF POS-5-8 = '2004' OR '3004' OR '400C' OR '500C'  
                                   OR '6200' OR '700C'                  
                  THEN MOVE 48     TO BONI-068
                  ELSE
               IF POS-5-8 = '2005' OR '300C' OR '4005' OR '5602'                      
                                   OR '6521' OR '7005' OR '6005'                      
                  THEN MOVE 32     TO BONI-068 
                  ELSE GO TO FIN-CONSULTA-M068.
               MOVE POS-15-16-N TO PORC68.                                  
               MOVE T-REG TO I68. 
       FIN-CONSULTA-M068.
           EXIT.
       CONSULTA-M112.
           IF (AM-POS-5-8 (I) = 'S103' OR 'S479' OR 'S009' OR 'S476'  
                OR  'S488' OR 'S454' ) OR AM-POS-5 (I) IS NUMERIC 
                  THEN MOVE 'SI'  TO MAR-M112.
       F-CONSULTA-M112.
       GRABA.
           IF AM-POS-1(I) = '*' AND AM-HABER(I) = ZEROS     
             THEN GO TO FIN-GRABA 
             ELSE                   
           WRITE REG-SAL FROM REGIS (I) 
           ADD 1 TO T-GRABA.
       FIN-GRABA.
           EXIT.
 ****
 ********************************
       SUMA-028.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-M028 THRU FIN-ACUMULA-M028                
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.

           IF SUMA-M028 NOT > ZEROS THEN GO TO FIN-SUMA-028.

           IF AM-POS-9-10 (Z) = '40' OR '44' OR '48'
               THEN IF FECHA-AUX < '200806'
                    THEN COMPUTE IMPORTE = 1000 * PORC / 100
                    ELSE COMPUTE IMPORTE = 1300 * PORC / 100
               ELSE
           IF AM-POS-9-10 (Z) = '00' OR '30' OR '32' OR '35'
               THEN IF FECHA-AUX < '200806'
                    THEN COMPUTE IMPORTE = 815 * PORC / 100
                    ELSE COMPUTE IMPORTE = 1060 * PORC / 100
               ELSE DISPLAY ' ver REG. HOR ' REGIS (Z). 

           IF SUMA-M028 > IMPORTE
                    THEN GO TO FIN-SUMA-028 
                    ELSE COMPUTE IMPORTE = IMPORTE - SUMA-M028.

           ADD 1 TO T-MOD-028.
           ADD 1 TO T-REG.
           MOVE REGIS (Z) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           IF FECHA-AUX < '200806'
            THEN MOVE '*M028ORD4500       ' TO AM-CODIGO (T-REG) 
            ELSE MOVE '*M028ORD4664       ' TO AM-CODIGO (T-REG).
       FIN-SUMA-028.
           EXIT.
       ACUMULA-M028.
 ************ acumula los supl y SUMF que se separaron en Dic/12
           IF AM-CONC (I) = '02' AND  AM-COD2-1-4 (I) = 'M028'                         
              AND AM-1-10 (I) = '*BON.MUN.S' 
                THEN COMPUTE SUMA-M028 = SUMA-M028 + AM-HABER (I) -  
                                     (AM-HABER (I) * 0.048).
       FIN-ACUMULA-M028. EXIT.
       SUMA-033.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           COMPUTE IMPORTE033 = 150 * PORC / 100.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE033 TO AM-HABER (T-REG).                               
           MOVE '*M033 BON150       ' TO AM-CODIGO (T-REG).
       FIN-SUMA-033.
           EXIT.
       SUMA-056.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-056 THRU FIN-ACUMULA-HABER-056       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.

           IF SUMA-M056 NOT > ZEROS THEN GO TO FIN-SUMA-056.

           COMPUTE IMPORTE = 750 * PORC / 100.

           IF SUMA-M056 > IMPORTE
                THEN GO TO FIN-SUMA-056 
                ELSE COMPUTE IMPORTE = IMPORTE - SUMA-M056.

           ADD 1 TO T-MOD-056.
           ADD 1 TO T-REG.
           MOVE REGIS (Z) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*M056DTO268/07     ' TO AM-CODIGO (T-REG).
       FIN-SUMA-056.
           EXIT.
       ACUMULA-HABER-056.
 ************ acumula los supl y SUMF que se separaron en Dic/12
           IF AM-CONC (I) = '02' AND  AM-COD2-1-4 (I) = 'M056'                         
              AND AM-1-10 (I) = '*BON.MUN.S' 
                THEN COMPUTE SUMA-M056 = SUMA-M056 + AM-HABER (I).  
       FIN-ACUMULA-HABER-056. EXIT.
       SUMA-060.
           MOVE ZEROS TO SUM-HABER.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-060 THRU FIN-ACUMULA-HABER-060       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF SUMA-M060 NOT > ZEROS THEN GO TO FIN-SUMA-060.
           COMPUTE IMPORTE = SUMA-M060 * 0.13.                 
           ADD 1 TO T-MOD-060.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*BONI M060-08/05   ' TO AM-CODIGO (T-REG).
           IF FECHA-AUX < '200607' THEN GO TO FIN-SUMA-060.
      *     
   *******                      se saca 27/10/2006
   ******* COMPUTE SUMA-M060 = SUMA-M060 + IMPORTE.
      *   
           COMPUTE IMPORTE = SUMA-M060 * 0.10.                 
           ADD 1 TO T-MOD-060.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*ORD. 10184/06   ' TO AM-CODIGO (T-REG).
           IF MAR-C-060 = 0 THEN GO TO FIN-SUMA-060.
           COMPUTE IMPORTE2 = 100.00 * PORC / 100.              
           IF IMPORTE < IMPORTE2              
                      THEN MOVE IMPORTE2 TO IMPORTE 
                      ELSE GO TO FIN-SUMA-060.
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*GAR.ORD. 10184/06   ' TO AM-CODIGO (T-REG).
       FIN-SUMA-060.
           EXIT.

       ACUMULA-HABER-060.
 ************ acumula los supl y SUMF que se separaron en Dic/12
           IF AM-CONC (I) = '02' AND  AM-COD2-1-4 (I) = 'M060'                         
              AND AM-1-10 (I) = '*BON.MUN.S' 
                THEN COMPUTE SUMA-M060 = SUMA-M060 + AM-HABER (I).  
       FIN-ACUMULA-HABER-060. EXIT.
       SUMA-068.
           IF COD-RETRO = 'C' THEN
              IF PORC68 = ZEROS  THEN MOVE 100 TO PORC68.
           MOVE ZEROS TO IMPORTE IOMA-AUX.
           IF BONI-068 = ZEROS OR PORC68 = ZEROS
              THEN GO TO FIN-SUMA-068 
              ELSE
                  COMPUTE IMPORTE = BONI-068 * PORC68 / 100. 
           ADD 1 TO T-REG.
           MOVE REGIS (I68) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE SPACE TO AM-EME (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '**DTO1853/07      ' TO AM-CODIGO (T-REG). 
           MOVE ZEROS TO BONI-068.            
       FIN-SUMA-068.
       SUMA-063.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUM-HABER HABER-AUX.
           MOVE ZEROS TO IMPORTE IOMA-AUX.
           PERFORM ACUMULA-HABER-063 THRU FIN-ACUMULA-HABER-063       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF HABER-AUX NOT > ZEROS THEN GO TO FIN-SUMA-063.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           COMPUTE IMPORTE2 = (HABER-AUX - IOMA-AUX) * 100 / PORC.
           IF BONI-063 > ZEROS
              THEN
                 IF IMPORTE2 < BONI-063
                   THEN COMPUTE SUM-HABER = BONI-063 - IMPORTE2  
                        COMPUTE IMPORTE = SUM-HABER * PORC / 100 
                   ELSE GO TO FIN-SUMA-063 
              ELSE
           IF IMPORTE < MIN063                                     
              THEN  COMPUTE SUM-HABER = MIN063 - IMPORTE   
                    COMPUTE IMPORTE = SUM-HABER * PORC / 100 
              ELSE GO TO FIN-SUMA-063.
           ADD 1 TO T-MOD-063.
           ADD 1 TO T-REG.
           MOVE REGIS (I63) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE SPACE TO AM-EME (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           IF BONI-063 > ZEROS
              THEN MOVE '*G.M063 ORD2028/06' TO AM-CODIGO (T-REG) 
              ELSE MOVE '*GAR.M063 D1986/05' TO AM-CODIGO (T-REG).
           MOVE ZEROS TO BONI-063.            
           MOVE ZEROS TO MIN063.              
       FIN-SUMA-063.
           EXIT.

       ACUMULA-HABER-063.
   ******      para ver minimos
             IF FECHA-AUX > '201607'                          
                     THEN MOVE 5357 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201601'                          
                     THEN MOVE 4870 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201507'                          
                     THEN MOVE 4059 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201501'                          
                     THEN MOVE 3690 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201401'                          
                     THEN MOVE 3075 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201301'                          
                     THEN MOVE 2563 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201201'                          
                     THEN MOVE 2136 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201107'                          
                     THEN MOVE 1780 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '201007'                          
                     THEN MOVE 1404 TO MIN063  
                     ELSE                         
             IF FECHA-AUX > '200801'                          
                     THEN MOVE 1000 TO MIN063  
                     ELSE MOVE 600 TO MIN063.

   ********  la munic no tiene reg. horario
   *********           THEN IF AM-POS-9-10 (I) = '35' 
   **********                   THEN MOVE 1000 TO MIN063 
   *********                    ELSE
   ***********              IF AM-POS-9-10 (I) = '40' 
   ***********                  THEN MOVE 1100 TO MIN063 
   ***********                  ELSE
   **********               IF AM-POS-9-10 (I) = '45' 
   *********                    THEN MOVE 1200 TO MIN063.
   ******      para ver minimos

           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02')
              THEN                                                
               IF (AM-POS-1-4 (I) = 'M063' OR AM-COD2-1-4 (I) = 'M063')                  
                   AND AM-CODIGO (I) NOT = '*BONI.NO REM.'
                   AND AM-CODIGO (I) NOT = '*BON.MUN.SUMF2'
************** agrego febrero/16
                   AND (AM-CODIGO (I) NOT = '*BON.MUN.SUMF1'                                    
                                        AND FECHA-AUX > '201601')
                THEN COMPUTE HABER-AUX = HABER-AUX + (AM-HABER (I) +
                                                      AM-ANTIG (I)) 
                     MOVE I TO I63
                     COMPUTE IOMA-AUX = IOMA-AUX +                   
                               ((AM-HABER (I) + AM-ANTIG (I)) * 0.048).
       FIN-ACUMULA-HABER-063.
           EXIT.
       SUMA-008.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUMA-M008.
           PERFORM ACUMULA-HABER-008 THRU FIN-ACUMULA-HABER-008       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF SUMA-M008 NOT > ZEROS THEN GO TO FIN-SUMA-008.
           COMPUTE IMPORTE = SUMA-M008 * 0.20.         
           ADD 1 TO T-REG.
           MOVE REGIS (T) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*DTO.2767-08        ' TO AM-CODIGO (T-REG).
       FIN-SUMA-008.
           EXIT.
       ACUMULA-HABER-008.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN                                                
               IF AM-POS-1-4 (I) = 'M008'                     
                 THEN IF AM-POS-5 (I) = 'R' OR                              
                 (AM-POS-5-6 (I) = '9B' OR '9E' OR '9H' OR '9I' OR '9K') 
                     THEN NEXT SENTENCE
                     ELSE MOVE I TO T                                          
                          COMPUTE SUMA-M008 = SUMA-M008 + AM-HABER (I) +
                                                          AM-ANTIG (I). 
       FIN-ACUMULA-HABER-008.
           EXIT.
       SUMA-010.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUM-HABER.
           PERFORM ACUMULA-HABER-010 THRU FIN-ACUMULA-HABER-010       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF HABER-AUX NOT > ZEROS THEN GO TO FIN-SUMA-010.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF IMPORTE < 700.00 THEN                                
                 IF FECHA-AUX < '200501'
                   THEN  COMPUTE IMPORTE = 185.00 * PORC / 100
                   ELSE IF FECHA-AUX < '200507'
                      THEN  COMPUTE IMPORTE = 155.00 * PORC / 100
                      ELSE  COMPUTE IMPORTE = 125.00 * PORC / 100
                               ELSE                          
                 IF FECHA-AUX < '200501'
                   THEN  COMPUTE IMPORTE = 100.00 * PORC / 100 
                   ELSE IF FECHA-AUX < '200507'
                      THEN COMPUTE IMPORTE =  70.00 * PORC / 100 
                      ELSE COMPUTE IMPORTE =  40.00 * PORC / 100.
           ADD 1 TO T-MOD-010.
           ADD 1 TO T-REG.
           MOVE REGIS (I10) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*BON.M010 DTO 575/04' TO AM-CODIGO (T-REG).
       FIN-SUMA-010.
           EXIT.
       ACUMULA-HABER-010.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN                                                
               IF AM-POS-1-4 (I) = 'M010'                     
                 THEN IF AM-POS-5 (I) = 'N' OR
               (AM-POS-5-8 (I) = 'S051' OR 'S302' OR 'S312' OR 'S501'  
                                        OR 'S463')
                 IF FECHA-AUX < '200501'
                   THEN MOVE I TO I10                                     
                        COMPUTE HABER-AUX = HABER-AUX + (AM-HABER (I) +
                AM-ANTIG (I)) - ((AM-HABER (I) + AM-ANTIG (I)) * 0.045) 
                   ELSE MOVE I TO I10                                     
                        COMPUTE HABER-AUX = HABER-AUX + (AM-HABER (I) +
                AM-ANTIG (I)) - ((AM-HABER (I) + AM-ANTIG (I)) * 0.048).
       FIN-ACUMULA-HABER-010.
           EXIT.
       SUMA-062.
           IF COD-RETRO = 'C' THEN
                     IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUM-HABER.
           PERFORM ACUMULA-HABER-062 THRU FIN-ACUMULA-HABER-062       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF FECHA-AUX > '201004' THEN
           IF IMPORTE > 2700.00 THEN GO TO FIN-SUMA-062 
                                ELSE
           IF IMPORTE < 1620.00 THEN                                
                         COMPUTE IMPORTE = 100.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 1890.01 THEN                                
                         COMPUTE IMPORTE =  80.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 2160.01 THEN                                
                         COMPUTE IMPORTE =  60.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 2440.01 THEN                                
                         COMPUTE IMPORTE =  40.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 2700.01 THEN                                
                         COMPUTE IMPORTE =  20.00 * PORC / 100  
                                ELSE GO TO FIN-SUMA-062
                                   ELSE
           IF FECHA-AUX > '201001' THEN
           IF IMPORTE > 2000.00 THEN                    
                         COMPUTE IMPORTE = 100.00 * PORC / 100  
                                ELSE
           IF IMPORTE < 1550.00 THEN                                
                         COMPUTE IMPORTE = 160.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 1750.01 THEN                                
                         COMPUTE IMPORTE = 140.00 * PORC / 100 
                                ELSE
           IF IMPORTE < 2000.01 THEN                                
                         COMPUTE IMPORTE = 120.00 * PORC / 100 
                                ELSE GO TO FIN-SUMA-062 
                                  ELSE GO TO FIN-SUMA-062.
           ADD 1 TO T-M062.
           ADD 1 TO T-REG.
           MOVE REGIS (I62) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*BONI.M062 ' TO AM-CODIGO (T-REG).
       FIN-SUMA-062.
           EXIT.
       ACUMULA-HABER-062.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN                                                
               IF AM-POS-1-4 (I) = 'M062' AND AM-POS-5 (I) NOT = 'S'                     
                   THEN MOVE I TO I62                                    
                        COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                                        AM-ANTIG (I).  
       FIN-ACUMULA-HABER-062.
           EXIT.
       SUMA-044.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUM-HABER.
           PERFORM ACUMULA-HABER-044 THRU FIN-ACUMULA-HABER-044       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF HABER-AUX NOT > ZEROS THEN GO TO FIN-SUMA-044.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF IMPORTE NOT > 1000.00 THEN                                
                         COMPUTE IMPORTE = 100.00 * PORC / 100 
                                ELSE GO TO FIN-SUMA-044.
           ADD 1 TO T-MOD-044.
           ADD 1 TO T-REG.
           MOVE REGIS (I44) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*BON.M044 DTO 136/04' TO AM-CODIGO (T-REG).
       FIN-SUMA-044.
           EXIT.
       ACUMULA-HABER-044.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN                                                
               IF AM-POS-1-4 (I) = 'M044'                     
                 IF FECHA-AUX < '200501'
                   THEN MOVE I TO I44                                        
                   COMPUTE HABER-AUX = HABER-AUX + (AM-HABER (I) +
                AM-ANTIG (I)) - ((AM-HABER (I) + AM-ANTIG (I)) * 0.045) 
                   ELSE MOVE I TO I44                                        
                        COMPUTE HABER-AUX = HABER-AUX + (AM-HABER (I) +
                AM-ANTIG (I)) - ((AM-HABER (I) + AM-ANTIG (I)) * 0.048). 
       FIN-ACUMULA-HABER-044.
           EXIT.
       SUMA-013.
           IF COD-RETRO = 'C'          
              THEN IF PORC = ZEROS THEN MOVE 100 TO PORC.
           MOVE ZEROS TO SUM-HABER.
           PERFORM ACUMULA-HABER-013 THRU FIN-ACUMULA-HABER-013       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE  = HABER-AUX * 100 / PORC.
           COMPUTE IMPORTE1 = HABER-AUX * 100 / PORC.

           IF FECHA-AUX < '199902'
              THEN
                PERFORM CALCULA-BONIF-013 THRU FIN-CALCULA-BONIF-013.
            
           IF FECHA-AUX < '200512' THEN GO TO FIN-SUMA-013. 
           MOVE ZEROS TO VALOR-013.

           IF (AM-POS-5 (1) = '9' OR
               AM-POS-5-8 (1) = 'S050' OR
               AM-POS-5-8 (1) = 'S463')
              THEN  GO TO FIN-SUMA-013. 

           IF FECHA-AUX > '200511' AND FECHA-AUX < '200702' 
              THEN             
               IF IMPORTE > 1000.00
                 THEN MOVE 30.00 TO VALOR-013
                 ELSE 
                   IF IMPORTE > 900.00
                    THEN MOVE 40.00 TO VALOR-013
                    ELSE 
                     IF IMPORTE > 800.00
                      THEN MOVE 50.00 TO VALOR-013
                      ELSE 
                       IF IMPORTE > 700.00
                        THEN MOVE 60.00 TO VALOR-013
                        ELSE 
                         IF IMPORTE > 600.00
                          THEN MOVE 70.00 TO VALOR-013
                          ELSE 
                           IF IMPORTE > 500.00
                            THEN MOVE 80.00 TO VALOR-013
                            ELSE 
                              MOVE 90.00 TO VALOR-013.
           IF FECHA-AUX > '200701' 
              THEN             
               IF IMPORTE > 1000.00
                 THEN MOVE 110.00 TO VALOR-013
                 ELSE 
                   IF IMPORTE > 900.00
                    THEN MOVE 120.00 TO VALOR-013
                    ELSE 
                     IF IMPORTE > 800.00
                      THEN MOVE 130.00 TO VALOR-013
                      ELSE 
                       IF IMPORTE > 700.00
                        THEN MOVE 140.00 TO VALOR-013
                        ELSE 
                         IF IMPORTE > 600.00
                          THEN MOVE 150.00 TO VALOR-013
                          ELSE 
                           IF IMPORTE > 500.00
                            THEN MOVE 160.00 TO VALOR-013
                            ELSE 
                              MOVE 170.00 TO VALOR-013.
           IF VALOR-013 NOT > ZEROS  THEN GO TO FIN-SUMA-013. 
           ADD 1 TO T-MOD-013N.
           ADD 1 TO T-REG.
           MOVE REGIS (I13) TO REGIS (T-REG).
           MOVE ZEROS     TO AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'      TO AM-CONC (T-REG).
           MOVE SPACES    TO AM-EME (T-REG).
           MOVE '*DTO1101/05  ' TO AM-CODIGO (T-REG).
           COMPUTE AM-HABER (T-REG) = VALOR-013 * PORC / 100.
       FIN-SUMA-013.
           EXIT.
       CALCULA-BONIF-013.
           IF IMPORTE < 501.00 THEN GO TO FIN-CALCULA-BONIF-013.

           IF IMPORTE < 900.00 THEN MOVE 0.075 TO PORC-N
                               ELSE MOVE 0.100 TO PORC-N.

           PERFORM MODIFICA-HABER-013 THRU FIN-MODIFICA-HABER-013
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.

           COMPUTE IMPORTE = SUM-HABER * 100 / PORC.

           IF IMPORTE < 501.00
                       THEN COMPUTE AM-HABER (1) = AM-HABER (1) +
                            ((501.00 - IMPORTE) * PORC / 100).
       FIN-CALCULA-BONIF-013.
           EXIT.
       ACUMULA-HABER-013.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M013'                     
                     THEN MOVE I TO I13
                         COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-013.
           EXIT.
       MODIFICA-HABER-013.
            IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
            THEN IF AM-POS-1-4 (I) = 'M013'
                                AND AM-FECHA-R (I) < '199702'
                    THEN COMPUTE IMPORTE = AM-HABER (I) * PORC-N
                         COMPUTE AM-HABER (I) = AM-HABER (I) - IMPORTE
                         COMPUTE IMPORTE = AM-ANTIG (I) * PORC-N
                         COMPUTE AM-ANTIG (I) = AM-ANTIG (I) - IMPORTE
                         COMPUTE SUM-HABER = SUM-HABER + AM-HABER (I)
                                         + AM-ANTIG (I)
                         ADD 1 TO T-MOD-013
                    ELSE IF AM-POS-1-4 (I) = 'M013'
                            AND AM-FECHA-R (I) > '199701' AND
               (AM-POS-5-8 (I) = 'S252' OR 'S450' OR 'S501' OR 'S051' OR
                              'S103' OR 'S100' OR 'S101' OR 'S302' OR
                              '1103' OR '1123')
                          THEN
                         COMPUTE IMPORTE = AM-HABER (I) * PORC-N
                         COMPUTE AM-HABER (I) = AM-HABER (I) - IMPORTE
                         COMPUTE IMPORTE = AM-ANTIG (I) * PORC-N
                         COMPUTE AM-ANTIG (I) = AM-ANTIG (I) - IMPORTE
                         COMPUTE SUM-HABER = SUM-HABER + AM-HABER (I)
                                         + AM-ANTIG (I)
                         ADD 1 TO T-MOD-013.
       FIN-MODIFICA-HABER-013.
           EXIT.
       SUMA-074.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO K IMPORTE.
           MOVE 'NO' TO SI-074.       
           PERFORM VERIFICA-074 THRU FIN-VERIFICA-074       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           IF SI-074 = 'SI' THEN GO TO FIN-SUMA-074.
           IF K > ZEROS THEN
                   COMPUTE IMPORTE = AM-IOMA (K) * PORC / 100.
           IF IMPORTE NOT > ZEROS  
               THEN GO TO FIN-SUMA-074.
           ADD 1 TO T-MOD-074.
           ADD 1 TO T-REG.
           MOVE REGIS (K) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           MOVE IMPORTE TO AM-HABER (T-REG).                               
           MOVE '*BONI.M074      '  TO AM-CODIGO (T-REG).
       FIN-SUMA-074.
           EXIT.
       VERIFICA-074.
           IF AM-CONC (I) = '02' AND                            
                  AM-1-10 (I) = '*BONI.M074'                     
                    THEN  MOVE 'SI' TO SI-074.                             
           IF AM-CONC (I) = '02' AND AM-POS-1-4 (I) = 'M074'    
                    THEN MOVE I TO K.                                         
       FIN-VERIFICA-074.
           EXIT.
       ACUMULA-HABER-079.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M079'             
                      THEN IF FECHA-AUX < '200204'
                    THEN COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I) 
                    ELSE IF AM-POS-5-8 (I) NOT = 'RRRR'                   
                           THEN
                         COMPUTE HABER-AUX = HABER-AUX + AM-IOMA (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-079.
           EXIT.
       ACUMULA-HABER-106.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M106'
                     THEN MOVE I TO I106
                         COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-106.
           EXIT.
       ACUMULA-HABER-112.
 ************   se agrega el 31-05-06
           PERFORM CONSULTA-M112 THRU F-CONSULTA-M112.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M112'                      
                    THEN COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-112.
           EXIT.
       ACUMULA-HABER-084.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M084'                      
                    THEN MOVE I TO I84
                         COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-084.
           EXIT.    
       ACUMULA-HABER-117.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M117'                      
                    THEN MOVE I TO I117
                         COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-117.
           EXIT.
       ACUMULA-HABER-135.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M135'                      
                    THEN COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-135.
           EXIT.
       ACUMULA-HABER-090.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M090'                      
                    THEN COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I).
       FIN-ACUMULA-HABER-090.
           EXIT.
       ACUMULA-HABER-124.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ') OR
              (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M124'
                THEN                                                              
                MOVE I TO I124
                COMPUTE IOMA-AUX = (AM-HABER (I) + AM-ANTIG (I)) * 0.048
                COMPUTE HABER-AUX = HABER-AUX + AM-HABER (I) +
                                             AM-ANTIG (I) - IOMA-AUX.
       FIN-ACUMULA-HABER-124.
           EXIT.

       SUMA-079.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-079 THRU FIN-ACUMULA-HABER-079       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF IMPORTE > 699.99
               THEN GO TO FIN-SUMA-079.
           ADD 1 TO T-MOD-079.
           PERFORM MODIFICA-HABER-079 THRU FIN-MODIFICA-HABER-079
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
       FIN-SUMA-079.
           EXIT.
       MODIFICA-HABER-079.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M079'     
                   THEN IF (AM-POS-5 (I) = 'C' OR AM-POS-5 (I) = 'A'
                         OR AM-POS-5-8 (I) = 'RRRR')
                           THEN NEXT SENTENCE
                           ELSE  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (100.00 * PORC / 100)
                                 MOVE T-REG TO I
                                 ADD 1 TO T-MOD-079
                   ELSE NEXT SENTENCE
             ELSE
           IF (AM-CONC (I) = '02' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M079'     
                     THEN IF (AM-POS-5 (I)  = 'C' OR AM-POS-5 (I) = 'A'
                        OR AM-POS-5-8 (I)  = 'RRRR')
                           THEN NEXT SENTENCE
                           ELSE COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (100.00 * PORC / 100)
                   DISPLAY AM-NRO (I) ' ' AM-APYN (I) ' ' AM-HABER (I)
                           AM-ANTIG (I)
                         ADD 1 TO T-MOD-079
                         MOVE T-REG TO I.
       FIN-MODIFICA-HABER-079.
           EXIT.
       P200-M079.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = 200 * PORC / 100.
           MOVE '*DTO.001/09 ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-P200-M079.
           EXIT.
       P150-M079.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           IF FECHA-AUX > '201006'
             THEN COMPUTE AM-HABER (T-REG) = 200 * PORC / 100 
             ELSE COMPUTE AM-HABER (T-REG) = 150 * PORC / 100.
           MOVE '*DTO.423/10 ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-P150-M079.
           EXIT.
       SUMA-090.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-090 THRU FIN-ACUMULA-HABER-090       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF IMPORTE > 500.00
               THEN MOVE 70.00 TO VALOR-090
               ELSE MOVE 85.00 TO VALOR-090.
           PERFORM MODIFICA-HABER-090 THRU FIN-MODIFICA-HABER-090
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
       FIN-SUMA-090.
           EXIT.
       MODIFICA-HABER-090.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M090' AND
                   ( AM-POS-5-8 (I) = 'S051' OR AM-POS-5-8 (I) = 'S501'
                        OR AM-POS-5 (I) NOT = 'S')
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (VALOR-090 * PORC / 100)
                         MOVE T-REG TO I
                         ADD 1 TO T-MOD-090
                   ELSE NEXT SENTENCE
             ELSE
           IF (AM-CONC (I) = '02' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M090' AND
                   ( AM-POS-5-8 (I) = 'S051' OR AM-POS-5-8 (I) = 'S501'
                        OR AM-POS-5 (I) NOT = 'S')
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (VALOR-090 * PORC / 100)
                         ADD 1 TO T-MOD-090
                         MOVE T-REG TO I.
       FIN-MODIFICA-HABER-090.
       SUMA-135.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-135 THRU FIN-ACUMULA-HABER-135     
            VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF IMPORTE > 699.99
               THEN GO TO FIN-SUMA-135.
           PERFORM MODIFICA-HABER-135 THRU FIN-MODIFICA-HABER-135
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
       FIN-SUMA-135.
           EXIT.
       MODIFICA-HABER-135.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M135' AND AM-POS-5 (I) NOT = 'C'
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (100.00 * PORC / 100)
                         MOVE T-REG TO I
                         ADD 1 TO T-MOD-135
                   ELSE NEXT SENTENCE
             ELSE
           IF (AM-CONC (I) = '02' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M135' AND AM-POS-5 (I) NOT = 'C'
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (100.00 * PORC / 100)
                   DISPLAY AM-NRO (I) ' ' AM-APYN (I) ' ' AM-HABER (I)
                           AM-ANTIG (I)
                        ADD 1 TO T-MOD-135
                         MOVE T-REG TO I.
       FIN-MODIFICA-HABER-135.
           EXIT.
       SUMA-106.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           PERFORM ACUMULA-HABER-106 THRU FIN-ACUMULA-HABER-106       
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           COMPUTE IMPORTE = IMPORTE * 0.10.
           IF IMPORTE < 60.00
               THEN MOVE 60.00 TO IMPORTE.
           ADD 1 TO T-MOD-106.
           ADD 1 TO T-REG.
           MOVE REGIS (I106) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = IMPORTE * PORC / 100.
           MOVE '*REFRIGERIO ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-106.
           EXIT.
       SUMA-109.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           IF FECHA-AUX < '200804' THEN
                    COMPUTE IMPOR109 = 600.00 * PORC / 100 
                                   ELSE
                IF FECHA-AUX < '200806' THEN
                           COMPUTE IMPOR109 = 750.00 * PORC / 100 
                                        ELSE
                            PERFORM VER-ANTI109 THRU F-VER-ANTI109.
           IF IMPOR109 > ZEROS
                     THEN
              ADD 1 TO T-MOD-109
              ADD 1 TO T-REG 
              MOVE REGIS (1) TO REGIS (T-REG) 
              MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG)
              MOVE '02'  TO AM-CONC (T-REG) 
              MOVE IMPOR109 TO AM-HABER (T-REG)
              MOVE '*M109ORD1730' TO AM-CODIGO (T-REG) 
              MOVE SPACES         TO AM-EME (T-REG) 
                     ELSE
                  IF IMPOR1091 > ZEROS
                            THEN
                      ADD 1 TO T-MOD-1091 
                      ADD 1 TO T-REG 
                      MOVE REGIS (1) TO REGIS (T-REG) 
                      MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG) 
                      MOVE IMPOR1091 TO AM-HABER (T-REG)
                      MOVE '02'  TO AM-CONC (T-REG) 
                      MOVE '*M109DTO152/08' TO AM-CODIGO (T-REG) 
                      MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-109.
           EXIT.   
       VER-ANTI109.
               IF FECHA-AUX > '201511' THEN
                           COMPUTE IMPOR1091 = 2500 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201212' THEN
                           COMPUTE IMPOR1091 = 1500 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201208' THEN
                           COMPUTE IMPOR1091 = 1000 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201110' THEN
                           COMPUTE IMPOR1091 = 1500 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201108' THEN
                           COMPUTE IMPOR1091 = 1400 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201105' THEN
                           COMPUTE IMPOR1091 = 1300 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201102' THEN
                           COMPUTE IMPOR1091 = 1200 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201008' THEN
                           COMPUTE IMPOR1091 = 1100 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF FECHA-AUX > '201003' THEN
                           COMPUTE IMPOR1091 = 1000 * PORC / 100 
                           GO TO F-VER-ANTI109
                                       ELSE                        
               IF ANTI < 06 THEN
                 COMPUTE IMPOR1091 = 750.00 * PORC / 100
                                ELSE
                 IF ANTI < 11 THEN
                     COMPUTE IMPOR1091 = 800.00 * PORC / 100
                                   ELSE
                         IF ANTI < 21 THEN
                     COMPUTE IMPOR1091 = 850.00 * PORC / 100
                                           ELSE
                     COMPUTE IMPOR1091 = 900.00 * PORC / 100.

       F-VER-ANTI109.  EXIT.

       SUMA-084.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE 140.00 TO VALOR-084.
           ADD 1 TO T-MOD-084
           ADD 1 TO T-REG. 
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = VALOR-084 * PORC / 100.
           MOVE '*DTO883/05  ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-084.
           EXIT.   
       SUMA-078.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE 80 TO VALOR-078.
           ADD 1 TO T-MOD-078
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = VALOR-078 * PORC / 100.
           MOVE '*BON04A10/05' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-078.
           EXIT.         
       SUMA-112.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO VALOR-112.
           MOVE 'NO' TO MAR-M112.
           PERFORM ACUMULA-HABER-112 THRU FIN-ACUMULA-HABER-112             
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
           IF FECHA-AUX  < '200405'     
                      THEN
               IF IMPORTE > 600.00                       
                   THEN MOVE 100.00 TO VALOR-112
                   ELSE MOVE 150.00 TO VALOR-112 
                      ELSE
             IF FECHA-AUX < '200502'      
                         THEN
                   IF IMPORTE > 660.00
                       THEN MOVE 110.00 TO VALOR-112
                       ELSE MOVE 165.00 TO VALOR-112 
                         ELSE
                    IF FECHA-AUX < '200508'      
                                THEN
                          IF MAR-M112 = 'SI'
                                   THEN
                              IF IMPORTE > 726.00                         
                                 THEN MOVE 121.00 TO VALOR-112
                                 ELSE MOVE 181.50 TO VALOR-112 
                                   ELSE NEXT SENTENCE
                                ELSE
                          IF MAR-M112 = 'SI'
                                   THEN
                              IF IMPORTE > 798.60 
                                 THEN MOVE 133.10 TO VALOR-112
                                 ELSE MOVE 199.65 TO VALOR-112.
           PERFORM MODIFICA-HABER-112 THRU FIN-MODIFICA-HABER-112
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           ADD 1 TO T-MOD-112.
       FIN-SUMA-112.
           EXIT.
       MODIFICA-HABER-112.
           IF (AM-CONC (I) = '01' AND AM-EME (I) = ' ')
              THEN IF AM-POS-1-4 (I) = 'M112'
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (VALOR-112 * PORC / 100)
                         MOVE T-REG TO I
                   ELSE NEXT SENTENCE
             ELSE
           IF (AM-CONC (I) = '02' AND AM-POS-1 (I) NOT = '*')
              THEN IF AM-POS-1-4 (I) = 'M112'
                   THEN  COMPUTE AM-HABER (I) = AM-HABER (I) +
                                   (VALOR-112 * PORC / 100) 
   ********      DISPLAY ' M112 '                                      
   ********            AM-NRO (I) ' ' AM-APYN (I) ' ' AM-HABER (I)
   ********          ' '   AM-ANTIG (I)
                         MOVE T-REG TO I.
       FIN-MODIFICA-HABER-112.
           EXIT.
       SUMA-117.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           MOVE ZEROS TO VALOR-117.                
           PERFORM ACUMULA-HABER-117 THRU FIN-ACUMULA-HABER-117             
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = HABER-AUX * 100 / PORC.
   *****                                            
   *****   IF FECHA-AUX < '200508' THEN               
   *****       IF IMPORTE > 1000.00
   *****           THEN MOVE 30.00 TO VALOR-117
   *****           ELSE MOVE 60.00 TO VALOR-117 
   *****                           ELSE
   *****       IF IMPORTE > 1000.00
   *****           THEN MOVE 60.00 TO VALOR-117
   ******          ELSE MOVE 120.00 TO VALOR-117.
   *****                                            
   *****      cambio el 28-02-06                                          
   *****                                            
           IF FECHA-AUX < '200508' THEN               
               IF IMPORTE > 1000.00
                   THEN MOVE 55.00 TO VALOR-117
                   ELSE MOVE 110.00 TO VALOR-117 
                                   ELSE
           IF FECHA-AUX < '201004' THEN               
               IF IMPORTE > 1000.00
                   THEN MOVE 60.00 TO VALOR-117
                   ELSE MOVE 120.00 TO VALOR-117 
                                   ELSE
               IF IMPORTE > 1000.00
                   THEN MOVE 60.00 TO VALOR-117.
           ADD 1 TO T-MOD-117.
           ADD 1 TO T-REG.
           MOVE REGIS (I117) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = VALOR-117 * PORC / 100.
           MOVE '*DTO419/04  ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
      *    IF FECHA-AUX > '200508'  AND    
      *      ((AM-POS-5 (1) = 'G' OR '1' OR 'M' OR 'C') OR
      *       (AM-POS-5 (1) = '9' AND AM-POS-8 (1) IS NUMERIC )
      *       OR (AM-POS-5-8 (1) = 'S480' OR 'S479'))lo reemplazo por aplic.del cambio de esc, abr09.
      **** desde que autorizan aplicar el cambio                                                      
           IF FECHA-AUX > '200711'  AND                          
             ((AM-POS-5 (1) = 'M' OR 'C') OR
               (AM-POS-5-8 (1) = 'S480' OR 'S479' OR 'G000'))        
      *        (AM-POS-5-8 (1) = 'S480' OR 'S479' OR '9004' OR '9409' OR 'G000')) lo cambio en jun09
                THEN
              MOVE 196.00 TO VALOR-117 
              ADD 1 TO T-MOD-117 
              ADD 1 TO T-REG 
              MOVE REGIS (I117) TO REGIS (T-REG) 
              MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG) 
              MOVE '02'  TO AM-CONC (T-REG) 
              COMPUTE AM-HABER (T-REG) = VALOR-117 * PORC / 100 
              MOVE '*DTO380-05  ' TO AM-CODIGO (T-REG) 
              MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-117.
           EXIT.
       SUMA-119.
      *    IF COD-RETRO = 'C' THEN    ****** lo saco dde mar14 y lo paso al gmae pq se usa para las hs fcionales.
      *    IF PORC = ZEROS  THEN MOVE 100 TO PORC.
      *    ADD 1 TO T-MOD-119.
      *    ADD 1 TO T-REG.
      *    MOVE REGIS (1) TO REGIS (T-REG).
      *    MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
      *    MOVE '02'  TO AM-CONC (T-REG).
      *    COMPUTE AM-HABER (T-REG) = 250.00 * PORC / 100.                               
      *    MOVE '*DTO2876/07     ' TO AM-CODIGO (T-REG).
      *    MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-119.
           EXIT.
       SUMA-061.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           ADD 1 TO T-MOD-061.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = 1444.00 * PORC / 100.                               
           MOVE '*DTO101/13      ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-061.
           EXIT.
       SUMA-122.
           IF COD-RETRO = 'C' THEN
           IF PORC = ZEROS  THEN MOVE 100 TO PORC.
           ADD 1 TO T-MOD-122.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG) AM-IOMA (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = 400.00 * PORC / 100.                               
           IF AM-COD2-56 (T-REG) = '93' OR '96' 
                      THEN
                    MOVE '*DTO305/16-M122 ' TO AM-CODIGO (T-REG) 
                      ELSE
                    MOVE '*DTO258/16-M122 ' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-122.
           EXIT.
       SUMA-124.
           IF COD-RETRO = 'C' THEN
                     IF PORC = ZEROS                                 
                            THEN MOVE 100 TO PORC.
           MOVE ZEROS TO VALOR-124.
           PERFORM ACUMULA-HABER-124 THRU FIN-ACUMULA-HABER-124
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.

   *****      GARANTIA VILLA GESELL
           IF FECHA-AUX > '201404'       
               THEN
               COMPUTE IMPORTE = 4000.00 * PORC / 100    
               ELSE
           IF FECHA-AUX > '201309'       
               THEN
               COMPUTE IMPORTE = 2904.00 * PORC / 100    
               ELSE
           IF FECHA-AUX > '201307'       
               THEN
               COMPUTE IMPORTE = 2640.00 * PORC / 100    
               ELSE GO TO FIN-SUMA-124.               

           IF HABER-AUX > IMPORTE
                   THEN GO TO FIN-SUMA-124.               
 ********
           ADD 1 TO T-MOD-124.
           ADD 1 TO T-REG.
           MOVE REGIS (I124) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = IMPORTE - HABER-AUX.     
           MOVE '*GARANT.M124' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
           DISPLAY REGIS (T-REG).
       FIN-SUMA-124.
           EXIT.

       SUMA-049.
           IF COD-RETRO = 'C' THEN
                     IF PORC = ZEROS                                 
                            THEN MOVE 100 TO PORC.
           IF FECHA-AUX = '201004'       
               THEN COMPUTE IMPORTE = 578.51 * PORC / 100    
               ELSE
           IF FECHA-AUX = '201005'       
               THEN COMPUTE IMPORTE = 607.44 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201009'       
               THEN COMPUTE IMPORTE = 637.81 * PORC / 100    
               ELSE
           IF FECHA-AUX = '201009'       
               THEN COMPUTE IMPORTE = 660.77 * PORC / 100    
               ELSE
           IF FECHA-AUX = '201010'       
               THEN COMPUTE IMPORTE = 684.56 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201104'       
               THEN COMPUTE IMPORTE = 707.15 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201109'       
               THEN COMPUTE IMPORTE = 777.86 * PORC / 100    
               ELSE
           IF FECHA-AUX = '201109'       
               THEN COMPUTE IMPORTE = 830.76 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201203'       
               THEN COMPUTE IMPORTE = 882.23 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201207'       
               THEN COMPUTE IMPORTE = 970.45 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201307'       
               THEN COMPUTE IMPORTE = 1058.68 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201311'       
               THEN COMPUTE IMPORTE = 1143.37 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201401'       
               THEN COMPUTE IMPORTE = 1228.07 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201403'       
               THEN COMPUTE IMPORTE = 1270.41 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201405'       
               THEN COMPUTE IMPORTE = 1312.76 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201409'       
               THEN COMPUTE IMPORTE = 1489.98 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201412'       
     ************* dde 6-11-14 se aplica equiv a algs agrupam, por eso distingo valor dde nov14.  ES el 25% del bas 500144.
               THEN COMPUTE IMPORTE = 1667.20 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201502'       
               THEN COMPUTE IMPORTE = 1721.43 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201505'       
               THEN COMPUTE IMPORTE = 1928.01 * PORC / 100    
               ELSE
           IF FECHA-AUX < '201507'       
               THEN COMPUTE IMPORTE = 2031.29 * PORC / 100    
               ELSE COMPUTE IMPORTE = 2117.36 * PORC / 100.   

 ********
           ADD 1 TO T-MOD-049.
           ADD 1 TO T-REG.
           MOVE REGIS (I049) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = IMPORTE.                 
           MOVE '*DTO-600/2010' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
     ***   DISPLAY REGIS (T-REG).
       FIN-SUMA-049.
           EXIT.

       SUMA-051.
           ADD 1 TO T-MOD-051.
           ADD 1 TO T-REG.
           MOVE REGIS (I051) TO REGIS (T-REG).
           MOVE ZEROS TO  AM-ANTIG (T-REG).
           MOVE '02'  TO AM-CONC (T-REG).
           COMPUTE AM-HABER (T-REG) = AM-HABER (I051) * 0.3.          
           MOVE '*DTO-452/12-7' TO AM-CODIGO (T-REG).
           MOVE SPACES         TO AM-EME (T-REG).
       FIN-SUMA-051.          EXIT.
