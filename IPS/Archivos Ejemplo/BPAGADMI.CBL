       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. 'BPAGADMI'.                                              000
       AUTHOR. 'MONTSERRAT 20/12/02'.                                       000
      *REMARKS. 'GENERA BONIFICACION PARA ADMINISTRACION        '.          000
      *REMARKS. 'ZONA FRIA de MAGISTERIO                        '.          000
      *REMARKS. 'EXCOMBATIENTES DE MALV DEL SENADO             '.           000
      *REMARKS. 'ARBAJ 30-04-10                                 '.          000
       ENVIRONMENT DIVISION.                                                000
       CONFIGURATION SECTION.                                               000
       INPUT-OUTPUT SECTION.                                                001
       FILE-CONTROL.                                                        001
           SELECT MAESTRO ASSIGN MAESTRO.
           SELECT MAEACT ASSIGN MAEACT.
           SELECT PESQUI ASSIGN PESQUI.
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
       FD  PESQUI                                                           001
           RECORD 71                                                        002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-PES.                                          002
       01  REG-PES.                                                         002
           02 NRO-BEN-P   PIC X(10).
           02 APEL-NOM-P  PIC X(25).
           02 HABER-P     PIC S9(7)V99.
           02 FILLER      PIC X(27).
       WORKING-STORAGE SECTION.                                             004
           COPY WSFECHA.CPY.
       77  ELIMURPE     PIC 9(6)    VALUE ZEROS.                            004
       77  T-MAELEI     PIC 9(6)    VALUE ZEROS.                            004
       77  T-GRABA      PIC 9(6)    VALUE ZEROS.                            004
       77  T-MOD-ADMI   PIC 9(6)    VALUE ZEROS.                            004
       77  T-MOD-J      PIC 9(6)    VALUE ZEROS.                            004
       77  T-ADMI       PIC 9(6)    VALUE ZEROS.                            004
       77  T-ARBAJ      PIC 9(6)    VALUE ZEROS.                            004
       77  T-ARBA3R0    PIC 9(6)    VALUE ZEROS.                            004
       77  T-ARBA3RX    PIC 9(6)    VALUE ZEROS.                            004
       77  T-LEY        PIC 9(6)    VALUE ZEROS.                            004
       77  T-ZONA       PIC 9(6)    VALUE ZEROS.                            004
       77  T-EXCOM      PIC 9(6)    VALUE ZEROS.                            004
       77  T-SMAL       PIC 9(6)    VALUE ZEROS.                            004
       77  T-SERVI      PIC 9(6)    VALUE ZEROS.                            004
       77  T-J          PIC 9(6)    VALUE ZEROS.                            004
       77  T-GRUPO-J    PIC 9(6)    VALUE ZEROS.                            004
       77  T-PESQ       PIC 9(6)    VALUE ZEROS.                            004
       77  T-ENC-P      PIC 9(6)    VALUE ZEROS.                            004
       77  T-BON-P      PIC 9(6)    VALUE ZEROS.                            004
       77  PORC-N       PIC 9V999   VALUE ZEROS.                            004
       77  IMPORTE      PIC S9(8)V999 VALUE ZEROS.                          004
       77  HABER-AUX    PIC S9(8)V999 VALUE ZEROS.                          004
       77  A-HABER-AUX  PIC S9(8)V999 VALUE ZEROS.                          004
       77  HABER-TOT    PIC S9(8)V999 VALUE ZEROS.                          004
       77  IOMA-AUX     PIC S9(8)V999 VALUE ZEROS.                          004
       77  SUM-HABER    PIC S9(8)V999 VALUE ZEROS.                          004
       77  ZONAFRIA     PIC S9(8)V999 VALUE ZEROS.                          004
       77  BONIF-AUX    PIC S9(8)V99  VALUE ZEROS.                          004
       77  BONIFIC      PIC S9(8)V99  VALUE ZEROS.                          004
       77  BONARBAJ     PIC S9(8)V99  VALUE ZEROS.                          004
       77  BONARBAT     PIC S9(8)V99  VALUE ZEROS.                          004
       77  ADIARBAJ     PIC S9(8)V99  VALUE ZEROS.                          004
       77  HABER3R0     PIC S9(8)V99  VALUE ZEROS.                          004
       77  HABER3RX     PIC S9(8)V99  VALUE ZEROS.                          004
       77  DIFE         PIC S9(8)V99  VALUE ZEROS.                          004
       77  DIFE3R0      PIC S9(8)V99  VALUE ZEROS.                          004
       77  DIFE3RX      PIC S9(8)V99  VALUE ZEROS.                          004
       77  TITULO       PIC S9(8)V99  VALUE ZEROS.                          004
       77  NUM-SEC      PIC S9(5)V99  VALUE ZEROS.                          009
       77  T-REG        PIC 99      VALUE ZEROS.                            004
       77  I            PIC 99      VALUE ZEROS.                            004
       77  J            PIC 99      VALUE ZEROS.                            004
       77  X            PIC 99      VALUE ZEROS.                            004
       77  K            PIC 99      VALUE ZEROS.                            004
       77  S            PIC 99      VALUE ZEROS.                            004
       77  P            PIC 99      VALUE ZEROS.                            004
       77  R            PIC 99      VALUE ZEROS.                            004
       77  K-ADMI       PIC 99      VALUE ZEROS.                            004
       77  K-J          PIC 99      VALUE ZEROS.                            004
       77  PORC         PIC 999     VALUE ZEROS.                            004
       77  P1           PIC 999V99  VALUE ZEROS.                            004
       77  MASI         PIC 9       VALUE ZEROS.                            004
       77  MASIA         PIC 9       VALUE ZEROS.                            004
       77  MARCA-3R0    PIC 9       VALUE ZERO.                             004
       77  MARCA-3RX    PIC 9       VALUE ZERO.                             004
       77  MARCA-ARBAJ  PIC 9       VALUE ZERO.                             004
       77  MARCA-ADMI   PIC 9       VALUE ZERO.                             004
       77  MARCA-EXCOMBA PIC 9       VALUE ZERO.                            004
       77  MARCA-J      PIC 9       VALUE ZERO.                             004
       77  MARCA-SER    PIC 9       VALUE ZERO.                             004
       77  MARCA-LEY    PIC 9       VALUE ZERO.                             004
       77  MARCA-SMAL   PIC 9       VALUE ZERO.                             004
       77  MARCA-SER01  PIC 9       VALUE ZERO.                             004
       77  MARCA-SER-SIMUL  PIC 9       VALUE ZERO.                         004
       77  FIN          PIC AA      VALUE 'NO'.                             004
       77  FIN-P        PIC AA      VALUE 'NO'.                             004
       77  NRO-ANT     PIC X(10)   VALUE SPACES.                            004
       77  FECHA-AUX    PIC X(6)    VALUE SPACES.
       77  AA           PIC 9(4)    VALUE ZEROS.
       77  CERO         PIC 9(4)    VALUE ZEROS.
       77  PARENTESCO   PIC X       VALUE SPACES.                           004
       77  IMPORTE1     PIC S9(8)V9999  VALUE ZEROS.                        004
       77  IMPORTE2     PIC S9(8)V9999  VALUE ZEROS.                        004
       77  HABER01      PIC S9(8)V9999  VALUE ZEROS.                        004
      *
      *      DISEºO DE MAESTRO                                              007
      *                                                                     007
       01  MAE.                                                             007
           02 MARCA           PIC X.                                        007
           02 CONC            PIC XX.                                       007
           02 SEC.                                                          007
              03 SEC1         PIC X.                                        007
              03 SEC2         PIC X.                                        007
           02 NRO             PIC X(10).                                    008
           02 EME             PIC X.                                        008
           02 APYN-Y-PAGO.                                                  008
              03 APYN            PIC X(27).                                 008
              03 PAGO            PIC X(4).                                  008
           02 R-APYN-Y-PAGO   REDEFINES APYN-Y-PAGO.                        008
              03 FILLER          PIC X(23).                                 008
              03 FECHA-RETRO1.                                              008
                 04 FECHA-RETRO  PIC X(6).                                  008
                 04 FECHA-RET-DD PIC X(2).                                  008
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
                 04 POS-1-5.                                                009
                    05 POS-1-4.                                             009
                       06 POS-1-2.                                          009
                          07 POS-1     PIC X.                               009
                          07 POS-2     PIC X.                               009
                       06 POS-3-4.                                          009
                          07 POS-3     PIC X.                               009
                          07 POS-4     PIC X.                               009
                    05 POS-5    PIC X.                                      009
                 04 POS-6       PIC X.                                      009
                 04 POS-7-8     PIC XX.                                     009
                 04 POS-9-10    PIC XX.                                     009
                 04 POS-9-10N REDEFINES POS-9-10    PIC 99.     
                 04 POS-11-12   PIC XX.                                     009
                 04 POS-11-12N REDEFINES POS-11-12    PIC 99.     
                 04 POS-13-14.                                              009
                    05 POS-13  PIC X.                                       009
                    05 POS-14  PIC X.                                       009
                 04 POS-13-14N REDEFINES POS-13-14    PIC 99.     
                 04 POS-15-16.                                              009
                    05 POS-15  PIC X.                                       009
                    05 POS-16  PIC X.                                       009
                 04 FILLER     PIC X(2).
                 04 POS-19     PIC X(1).
                 04 POS-20     PIC X(1).
           02 CODIGO2.                                                      009
                  03 POS1A5-DEHIS   PIC X(5).
                  03 POS6A15-DEHIS  PIC X(10).
           02 NRO-SEC          PIC 9(6).                                    011
           02 POS-138-139      PIC XX.                                      011
           02 POS-140          PIC X.                                       011
       01  CONCEPTOS.
           02 REGIS  OCCURS 80.
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
              03 A-ESPOSA.                                                  009
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
              04 A-POS-1-12.                                                009
                 05 A-POS-1-9.                                              009
                    06 A-POS-1-6.                                           009
                     07 A-POS-1-2.                                           009
                       08 A-POS-1     PIC X.                                009
                       08 A-POS-2     PIC X.                                009
                     07 A-POS-3-4.                                           009
                       08 A-POS-3     PIC X.                                009
                       08 A-POS-4     PIC X.                                009
                     07 A-POS-5-6.                                           009
                       08 A-POS-5    PIC X.                                 009
                       08 A-POS-6    PIC X.                                 009
                    06 A-POS-7-8-9   PIC XXX.                               009
                 05 A-POS-1-9-R  REDEFINES A-POS-1-9.                       009
                    06 A-POS-1-7     PIC X(7).                              009
                    06 FILLER        PIC X(2).                              009
                 05 A-POS-1-9-R1  REDEFINES A-POS-1-9.                       009
                    06 A-POS-1-8.
                      07 A-POS-1-4     PIC X(4).        
                      07 FILLER        PIC X(4).    
                    06 FILLER        PIC X.    
                 05 A-POS-10     PIC X.                                     009
                 05 A-POS-11-12  PIC XX.                                    009
                 05 A-POS-13-14.                                            009
                    06 A-POS-13  PIC X.                                     009
                    06 A-POS-14  PIC X.                                     009
                 05 A-POS-15-16.                                            009
                    06 A-POS-15  PIC X.                                     009
                    06 A-POS-16  PIC X.                                     009
              04 FILLER          PIC X(2).
              04 A-POS-19        PIC X(1).
              04 F               PIC X(1).
           03 A-CODIGO2.                                                    009
              04 A-POS-1-A-5-DE-HISCAR.                                     009
                 05 A-POS-1-2-DE-HISCAR.                                      009
                    06 A-POS-1-DE-HISCAR  PIC X.                               009
                    06 A-POS-2-DE-HISCAR  PIC X.                               009
                 05 A-POS-3-DE-HISCAR  PIC X.                            009
                 05 FILLER             PIC X(2).                            009
              04 FILLER             PIC X(10).                              009
           03 A-NRO-SEC             PIC 9(6).                               011
           03 A-POS-138-139         PIC XX.                                 009
           03 A-POS-140             PIC X.                                  009
      *
       01  REG-PARAM.
           02  COD-RETRO         PIC X.
           02  FECHA-MES.
               03 FE-ANIO        PIC X(4).
               03 FE-MES         PIC XX.
           02  TIP-LIQUI         PIC X.
       PROCEDURE DIVISION.                                                  019
           COPY PDFECHA.CPY.
           OPEN INPUT MAESTRO  PESQUI                                       019
                OUTPUT MAEACT.
           ACCEPT REG-PARAM.
           IF COD-RETRO = 'R' OR 'M' OR 'C'
                        THEN NEXT SENTENCE
                        ELSE DISPLAY ' PARAMETRO INDICA RETRO ERRONEA'
                             DISPLAY ' DEBE SER     -M-  O  -R-  C  '
                             COMPUTE AA = AA / CERO
                             GO TO CIERRE.
           PERFORM BORRA THRU FIN-BORRA VARYING I FROM 1 BY 1 UNTIL
            I > 80.
           READ MAESTRO INTO MAE AT END MOVE 'SI' TO FIN.
           READ PESQUI AT END MOVE ALL '9' TO NRO-BEN-P
                              MOVE 'SI' TO FIN-P.
           PERFORM PROCESO THRU FIN-PROCESO UNTIL FIN = 'SI'.
       CIERRE.                                                              017
           DISPLAY ' *** TOTALES DE CONTROL  PROG. BPAGADMI ***'.           017
           DISPLAY 'TOTAL DE REGISTROS MAESTRO           ' T-MAELEI.        017
           DISPLAY 'TOTAL DE REGISTROS MAEACT            ' T-GRABA.         017
           DISPLAY 'TOTAL DE REGISTROS PESQUI            ' T-PESQ.          017
           DISPLAY 'TOTAL DE REGISTROS ENCONTRADOS       ' T-ENC-P.         017
           DISPLAY 'TOTAL DE REGISTROS CON BONIF         ' T-BON-P.         017
           DISPLAY 'TOTAL DE REGISTROS CON ZONAFRIA      ' T-ZONA.          017
           DISPLAY 'TOTAL DE REGISTROS CON BONARBAJ      ' T-ARBAJ.         017
           DISPLAY 'TOTAL DE REGISTROS CON BONARBA3R0    ' T-ARBA3R0.       017
           DISPLAY 'TOTAL DE REGISTROS CON BONARBA3RX    ' T-ARBA3RX.       017
           DISPLAY 'TOTAL DE REGISTROS EXCOMBA DEL SENAD ' T-EXCOM.         017
           DISPLAY 'TOTAL DE JUB-PEN   ADMI              ' T-ADMI.          017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS ADMI  ' T-MOD-ADMI.      017
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS LEYES ' T-LEY.               
           DISPLAY 'TOTAL DE REGISTROS MODIFICADOS -J-   ' T-MOD-J.         017
           DISPLAY 'TOTAL DE REGISTROS ELIMINADOS URPE   ' ELIMURPE.        017
           CLOSE MAESTRO  MAEACT PESQUI.                                    032
           STOP RUN.                                                        032
       PROCESO.
           MOVE NRO OF MAE TO NRO-ANT.
           MOVE 0   TO MARCA-ADMI MASI MARCA-J MARCA-LEY.
           MOVE 0   TO MARCA-SER MARCA-SER01 MARCA-SER-SIMUL.   
           MOVE 0   TO ZONAFRIA  MARCA-EXCOMBA MARCA-ARBAJ.                              
           MOVE 0   TO BONARBAJ BONARBAT P1 ADIARBAJ TITULO.                            
           MOVE 0   TO HABER01 MASIA.                                                                  
           MOVE ZEROS   TO HABER3R0 DIFE3R0 MARCA-3R0.                                                               
           MOVE ZEROS   TO HABER3RX DIFE3RX MARCA-3RX.                                                               
           IF COD-RETRO = 'M'
                    THEN
           MOVE FECHA-MES TO FECHA-AUX
           PERFORM LEE-Y-CARGA THRU FIN-LEE-Y-CARGA UNTIL NRO OF MAE
              NOT = NRO-ANT OR FIN = 'SI'
                    ELSE
           MOVE FECHA-RETRO TO FECHA-AUX
           PERFORM LEE-Y-CARGA THRU FIN-LEE-Y-CARGA UNTIL FECHA-AUX NOT
            = FECHA-RETRO OR  NRO OF MAE NOT = NRO-ANT.

   ******  IF NRO-ANT = '5014030640' then display NRO-ANT ' ' MASI ' '
   ******                           MARCA-ADMI.
   ******                           se saca 15/02/2006 
   ******  IF MASI = 1 AND MARCA-ADMI = 0
   ******         THEN NEXT SENTENCE
   ******         ELSE
   ******                           se saca 02/11/2007 
           IF ZONAFRIA > ZEROS
              THEN  ADD 1 TO T-ZONA
                    PERFORM ZONA-FRIA THRU FIN-ZONA-FRIA.
        
   ******                           se saca 03/07/2007 
           IF MARCA-SMAL = 1
              THEN  ADD 1 TO T-SMAL
                    PERFORM MODI-SMAL THRU FIN-MODI-SMAL
              ELSE
   ******                           se saca 15/02/2006 
           IF MARCA-J = 1
              THEN  ADD 1 TO T-J
                    PERFORM SUMA-JOTA THRU FIN-SUMA-JOTA
              ELSE
    ****** IF MARCA-ADMI = 1
    ******    THEN  ADD 1 TO T-ADMI
    *******         PERFORM SUMA-ADMI THRU FIN-SUMA-ADMI 
    ******    ELSE
           IF MARCA-SER = 1
              THEN  ADD 1 TO T-SERVI
                    PERFORM VER-SERVI THRU FIN-VER-SERVI 
              ELSE
           IF MARCA-EXCOMBA = 1
              THEN  ADD 1 TO T-EXCOM
                    PERFORM EX-COMBA THRU FIN-EX-COMBA  
                        VARYING I FROM 1 BY 1 UNTIL I > T-REG 
              ELSE
           IF MARCA-ARBAJ   = 1
              THEN  ADD 1 TO T-ARBAJ 
                    PERFORM ARBAJ    THRU FIN-ARBAJ     
              ELSE
           IF MARCA-3R0 = 1
              THEN  ADD 1 TO T-ARBA3R0
                    PERFORM ARBA-3R0  THRU FIN-ARBA-3R0     
              ELSE
           IF MARCA-3RX = 1
              THEN  ADD 1 TO T-ARBA3RX
                    PERFORM ECON-3RX  THRU FIN-ECON-3RX     
              ELSE
           IF MARCA-3R0 > 1
              THEN  DISPLAY '***  OJO 3R00XB0A   es DOBLE ****'
                    DISPLAY '***  OJO 3R00XB0A   es DOBLE ****'
                    DISPLAY REGIS (R)                                    
                    DISPLAY '***  OJO 3R00XB0A   es DOBLE ****'
                     COMPUTE AA = AA / CERO
                     GO TO LEER 
              ELSE
           IF MARCA-3RX > 1
              THEN  DISPLAY '***  OJO 3RX0XB0A   es DOBLE ****'
                    DISPLAY '***  OJO 3RX0XB0A   es DOBLE ****'
                    DISPLAY REGIS (R)                                    
                    DISPLAY '***  OJO 3RX0XB0A   es DOBLE ****'
                     COMPUTE AA = AA / CERO
                     GO TO LEER.


           PERFORM GRABA THRU FIN-GRABA VARYING I FROM 1 BY 1
                UNTIL I > T-REG.
           PERFORM BORRA THRU FIN-BORRA VARYING I FROM 1 BY 1 UNTIL
            I > T-REG.
           MOVE ZEROS TO T-REG.
       FIN-PROCESO.
           EXIT.
       LEE-Y-CARGA.
           ADD 1 TO T-REG.

     ****  IF   NRO OF MAE = '1025151090'
     ****       THEN DISPLAY T-REG ' ' MAE.

           IF T-REG NOT > 80
              THEN MOVE MAE TO REGIS (T-REG)
              ELSE DISPLAY 'CANTIDAD DE CONCEPTOS SUPERIOR A 80 '
                     NRO OF MAE
                     COMPUTE AA = AA / CERO
                     GO TO LEER.

   *******    para generar zona fria de magisterio
           IF (A-POS-1-2-DE-HISCAR (T-REG) = '01' OR '91' 
                                     OR '21' OR '90')
                     AND A-POS-19 (T-REG) = 'Z'
                   THEN
                   IF A-CODIGO (T-REG) = '*COMP.AL.MINIMO      ' OR
                                         '*DIF MIN.            '
                          THEN NEXT SENTENCE
                          ELSE
                   COMPUTE ZONAFRIA = ZONAFRIA + A-HABER (T-REG)
                                               + A-ANTIG (T-REG).                                                

   *******    para generar zona fria de magisterio

           IF FECHA-AUX > '200703'
              THEN                                                         
              IF (A-CONC (T-REG) = '02' AND A-EME (T-REG) = ' ')      
                AND A-POS-1-8 (T-REG) = 'SMALV001' 
                             THEN  MOVE 1 TO MARCA-SMAL             
                                   MOVE T-REG TO K
                                   GO TO LEER. 
   ******
           IF FECHA-AUX > '200508'
              THEN                                                         
              IF (A-CONC (T-REG) = '01' AND A-EME (T-REG) = ' ')      
                AND A-POS-1-7 (T-REG) = '002ELEY' 
                             THEN  MOVE 1 TO MARCA-LEY              
                             PERFORM MODI-LEY THRU FIN-MODI-LEY
                                   GO TO LEER. 
   *****  
   *****   IF SEC2 = 'P'  OR POS-140 = '*' OR MASI = '1'
           IF POS-140 = '*'
           IF (CONC = '01' AND EME = ' ')
                           AND POS-140 = '*'
               THEN MOVE 1   TO MASIA
                    GO TO LEER.

           IF SEC2 = 'P' 
               THEN  MOVE 1   TO MASI
                    GO TO LEER.
           IF FECHA-AUX < '199802'
               THEN
                    GO TO LEER.
   *****  
           IF FECHA-AUX > '200501'
              THEN IF (CONC = '01' AND EME = ' ') AND 
               POS-1-2 = '02' AND  (POS-6 = '1' OR '2')
                             THEN  MOVE 1 TO MARCA-SER01            
                             ELSE
                   IF (CONC = '02' AND EME = ' ')
                       THEN IF POS-1-2 = '02' AND (POS-3-4 = '0A'
                       OR '0B' OR 'S0' OR 'E0' OR 'M0' OR 'M1'    
                       OR 'M2' OR 'M3' OR 'M4' OR 'TA' OR 'AE')    
                              THEN IF MARCA-SER = '0'  
                                   THEN MOVE 1 TO MARCA-SER 
                                        MOVE T-REG TO K            
                                        GO TO LEER 
                                   ELSE MOVE 1 TO MARCA-SER-SIMUL 
                                        MOVE T-REG TO S            
                                        GO TO LEER.
           IF FECHA-AUX > '200001' AND FECHA-AUX < '200704'
              THEN IF (CONC = '01' AND EME = ' ')
                       THEN IF POS-1 = 'J'
                               THEN PERFORM VERIFICA-SI-J THRU
                                        FIN-VERIFICA-SI-J
                                    GO TO LEER.
           IF (CONC = '01' AND EME = ' ')
               THEN IF POS-1 = '3' OR 'V' OR 'I' OR 'K' OR '8' OR
                               'H' OR 'B' OR 'X' OR 'E' OR 'A'
                         THEN MOVE 1 TO MARCA-ADMI
                              PERFORM CONSULTA THRU FIN-CONSULTA.
    ************  27/10/09        
           IF (POS1A5-DEHIS = 'S060D' OR 'S06N3')           
                                     AND SI-ES-VIAL = 'M'
                         THEN MOVE 1 TO MARCA-EXCOMBA
                              MOVE T-REG TO X   
                              GO TO LEER.
    ************  30/04/10 ARBAJ        
           IF POS-1-A-16 = '*ADIC.ARBAJ     '           
                              THEN
                   COMPUTE ADIARBAJ = ADIARBAJ + A-HABER (T-REG)
                              MOVE 1 TO MARCA-ARBAJ      
                              MOVE T-REG TO P.   
           IF A-CODIGO (T-REG) = '*GASTOS REPR    ' OR
                                         '*DISPONIB.PERM  '
              THEN
                IF POS1A5-DEHIS = 'J2502' OR 'J1702' OR 'J2402'                                          
                              OR 'J2702' 
                   THEN
             COMPUTE BONARBAJ = BONARBAJ + A-HABER (T-REG) 
                   ELSE
                IF POS1A5-DEHIS = 'J1101'                                        
                   THEN
  ************** HABER01 = DIPONIBILIDAD
             COMPUTE HABER01 = HABER01 +                                                 
                                   (A-HABER (T-REG) * 0.74) 
             COMPUTE BONARBAJ = BONARBAJ + A-HABER (T-REG) 
                   ELSE
                IF POS1A5-DEHIS = 'J0201'                             
                   THEN
  ************** HABER01 = DIPONIBILIDAD
             COMPUTE HABER01 = HABER01 +                                                 
                                 (A-HABER (T-REG) * 0.52)
             COMPUTE BONARBAJ = BONARBAJ + A-HABER (T-REG) .

**************** adicional de jerarq super de ARBA
             IF A-POS-1-DE-HISCAR (T-REG ) = '3'     
                 THEN
                 IF A-POS-1-2-DE-HISCAR (T-REG ) = '3R' AND
                             A-POS-3-DE-HISCAR (T-REG ) = '0'               
                      THEN ADD 1 TO MARCA-3R0        
                           MOVE T-REG TO R  
                      ELSE
             COMPUTE HABER3R0 = HABER3R0 + A-HABER (T-REG).
 
**************** adicional de jerarq super de ECONIMIA
             IF A-POS-1-DE-HISCAR (T-REG ) = '3'     
                 THEN
                 IF A-POS-1-2-DE-HISCAR (T-REG ) = '3R' AND
                             A-POS-3-DE-HISCAR (T-REG ) = 'X'               
                      THEN ADD 1 TO MARCA-3RX        
                           MOVE T-REG TO R  
                      ELSE
             COMPUTE HABER3RX = HABER3RX + A-HABER (T-REG).
 
 
  *******  IF (POS1A5-DEHIS = 'J1101' OR 'J0201') AND                             
  *******                  A-CODIGO (T-REG) = '*TITULO JERARQ.  '
 ************      THEN
 ***********       COMPUTE TITULO = TITULO + A-HABER (T-REG).            
       LEER.
           PERFORM LEER-MAESTRO THRU FIN-LEER-MAESTRO.
       FIN-LEE-Y-CARGA.
           EXIT.
 ********
       ARBA-3R0.  
             COMPUTE DIFE3R0 = A-HABER (R) - HABER3R0.
             IF DIFE3R0 > ZEROS
                        THEN MOVE DIFE3R0 TO A-HABER (R) 
                        ELSE MOVE ZEROS   TO A-HABER (R).
             MOVE ZEROS TO  A-IOMA (R) A-ANTIG (R).
       FIN-ARBA-3R0.    EXIT.
 ********
       ECON-3RX.  
             COMPUTE DIFE3RX = A-HABER (R) - HABER3RX.
             IF DIFE3RX > ZEROS
                        THEN MOVE DIFE3RX TO A-HABER (R) 
                        ELSE MOVE ZEROS   TO A-HABER (R).
             MOVE ZEROS TO  A-IOMA (R) A-ANTIG (R).
       FIN-ECON-3RX.    EXIT.
 ********
       ZONA-FRIA.
             ADD 1 TO T-REG.
             MOVE REGIS (1) TO REGIS (T-REG).
             MOVE ZEROS TO  A-IOMA (T-REG) A-ANTIG (T-REG).
             COMPUTE A-HABER (T-REG) = ZONAFRIA * 0.40.
             MOVE '02'      TO A-CONC (T-REG).
             MOVE '*BON.ZONAFRIA   '  TO A-CODIGO (T-REG).
       FIN-ZONA-FRIA.   EXIT.
       ARBAJ.     
           IF FECHA-AUX < '201401'
             THEN
                IF A-POS-1-A-5-DE-HISCAR (1) =                                                                        
                           'J2502' OR 'J1702' OR 'J2402'                                          
                   THEN
                COMPUTE BONARBAJ = BONARBAJ * 1.2 
                   ELSE
                IF POS1A5-DEHIS = 'J2702'                                                      
                   THEN
               COMPUTE BONARBAJ = BONARBAJ * 1.3
                   ELSE
                IF POS1A5-DEHIS = 'J1101' OR 'J0201'                                        
                   THEN
               COMPUTE HABER01 = HABER01 * 1.7                                             
               COMPUTE BONARBAJ = BONARBAJ * 1.7 
                   ELSE NEXT SENTENCE
             ELSE
 *************  IF POS1A5-DEHIS = 'J2502' OR 'J1702' OR 'J2702'                                          
                IF A-POS-1-A-5-DE-HISCAR (1) =                                                                        
                        'J2502' OR 'J1702' OR 'J2702'                                          
                              OR 'J2402'
                   THEN
                COMPUTE BONARBAJ = BONARBAJ * 0.35
                   ELSE
                IF POS1A5-DEHIS = 'J1101'                                                      
                   THEN
               COMPUTE HABER01 = HABER01 * 0.17                                            
               COMPUTE BONARBAJ = BONARBAJ * 0.17
                   ELSE
                IF POS1A5-DEHIS = 'J0201'                                        
                   THEN
               COMPUTE HABER01 = HABER01 * 0.13                                            
               COMPUTE BONARBAJ = BONARBAJ * 0.13 
                   ELSE GO TO FIN-ARBAJ.
 ***********
           COMPUTE A-HABER (P) = ADIARBAJ + BONARBAJ +
                                   HABER01.

       FIN-ARBAJ.       EXIT.
       VER-SERVI.    
              IF MARCA-SER01 = '1'
   *********         THEN PERFORM VERDESCU THRU VERDESCU-X 
                     THEN MOVE ZEROS TO A-HABER (K) 
                          MOVE ZEROS TO A-ANTIG (K) 
                          IF MARCA-SER-SIMUL = '1'
                              THEN MOVE ZEROS TO A-HABER (S) 
                                   MOVE ZEROS TO A-ANTIG (S). 
       FIN-VER-SERVI. EXIT.
       VERDESCU.
             IF A-SEC1 (K) = '5'
               THEN IF ( COD-RETRO = 'M' AND
                          FECHA-MES = '200506' ) OR
                       ( COD-RETRO = 'R' AND
                          FECHA-MES = '200502' ) OR
                       ( COD-RETRO = 'A' AND
                          FECHA-MES = '200506' )
                      THEN
                        ADD 1 TO T-REG
                        MOVE REGIS (K) TO REGIS (T-REG)
                        MOVE ZEROS TO  A-IOMA (T-REG) A-ANTIG (T-REG)
                        COMPUTE BONIFIC = A-HABER (T-REG) * (-1)
                        MOVE BONIFIC TO A-HABER (T-REG)
                        MOVE '7T'      TO A-CONC (T-REG)
                        MOVE '*DTO.LEY 9538/80'  TO A-CODIGO (T-REG)
                        PERFORM AGREGO-01N THRU F-AGREGO-01N.
       VERDESCU-X.   EXIT.
       AGREGO-01N.
             ADD 1 TO T-REG.
             MOVE REGIS (K) TO REGIS (T-REG).
             MOVE ZEROS     TO  A-IOMA (T-REG) A-ANTIG (T-REG).
             IF  COD-RETRO = 'M'
                           THEN
                 COMPUTE A-HABER (T-REG) = BONIFIC * 0.048
                           ELSE
             COMPUTE A-HABER (T-REG) = BONIFIC * 0.048 + BONIFIC * 0.18.
             MOVE '01'      TO A-CONC (T-REG).
             MOVE 'N'      TO A-EME  (T-REG).
             MOVE '*DEV.IOMA       '  TO A-CODIGO (T-REG).
             MOVE ZEROS TO BONIFIC.
       F-AGREGO-01N.     EXIT.

       VERIFICA-SI-J.
           IF POS-15 = 'F' OR 'C' OR 'T' OR 'H'
              THEN GO TO FIN-VERIFICA-SI-J.
           IF FECHA-AUX > '200406' AND NRO-ANT = '1039054810'
              THEN GO TO FIN-VERIFICA-SI-J.
           IF POS-1-5 = 'J0101' AND FECHA-AUX > '200509'
                THEN GO TO FIN-VERIFICA-SI-J.   
           IF POS-1-5 = 'J0301' OR 'J0701' OR 'J0801' OR 'J0901' OR
                        'J1001' OR 'J1601' OR 'J1701' OR 'J1801' OR
                        'J5301' OR 'J5401' OR 'J5501' OR 'J0102' OR
                        'J0402' OR 'J0602' OR 'J0802' OR 'J1202' OR
                        'J1402' OR 'J1502' OR 'J1802' OR 'J1902' OR
                        'J2002' OR 'J7201' OR 'J01S1' OR 'J04S1' 
              THEN NEXT SENTENCE
              ELSE 
                   MOVE 1 TO MARCA-J
                   PERFORM CONSULTA THRU FIN-CONSULTA.
     ***   IF NRO-ANT = '5014030640' then display NRO-ANT '  PORC  '  
     ***                 PORC ' '    MARCA-J.
     ***   IF   NRO OF MAE = '0000015030'
     ***        THEN DISPLAY ' MARCA-J ' MARCA-J '  PORC ' PORC.
       FIN-VERIFICA-SI-J. EXIT.
       MODI-LEY.                     
           IF A-NRO (T-REG) = '7073191700'                                   
                         THEN MOVE 5107.20 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY.
    ********    desde el 13-07-06.
           GO TO FIN-MODI-LEY.

           IF A-NRO (T-REG) = '1042083500'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '2031128690'                                   
                         THEN MOVE 5107.20 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE                          
           IF A-NRO (T-REG) = '2060715990'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7006693860'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7033291640'                                   
                         THEN MOVE 5107.20 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7041508050'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7051461280'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7054592170' OR '7108237560'                                                 
                         THEN MOVE 5107.20 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7105511740'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '7112866810'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '8111965690'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG)
                              ADD 1 TO T-LEY
                         ELSE            
           IF A-NRO (T-REG) = '8117044230'                                   
                         THEN MOVE 1702.40 TO A-HABER (T-REG) 
                              ADD 1 TO T-LEY.
       FIN-MODI-LEY.  EXIT.                    
       MODI-SMAL.                     
            IF A-HABER (K) > A-HABER-AUX
                        THEN
            COMPUTE A-HABER (K) = A-HABER (K) - A-HABER-AUX 
                        ELSE
                     MOVE ZEROS TO A-HABER (K).
       FIN-MODI-SMAL. EXIT.                    
       EX-COMBA.                      
            IF I = X
                  THEN GO TO FIN-EX-COMBA.
            IF A-CODIGO (I) = A-CODIGO (X)
                        THEN
                COMPUTE A-HABER (X) = A-HABER (X) - A-HABER (I)      
                      MOVE 81 TO I 
                        ELSE GO TO FIN-EX-COMBA.
            IF A-HABER (X) > ZEROS 
               THEN IF FECHA-AUX > '201606'  
                     THEN MOVE '*BONI.DTO605/16' TO A-CODIGO (X) 
                     ELSE MOVE '*BONI.DTO960/06' TO A-CODIGO (X) 
               ELSE MOVE ZEROS TO A-HABER (X).
       FIN-EX-COMBA.  EXIT.                    
       CONSULTA.
           IF POS-140 = '*' THEN GO TO FIN-CONSULTA.
           IF POS-1 = 'V'
              THEN MOVE POS-13-14N  TO PORC
              ELSE
                   IF POS-1 = '8'
                      THEN MOVE POS-9-10N  TO PORC
                      ELSE MOVE POS-11-12N  TO PORC.
           IF COD-RETRO = 'C' THEN
                 IF PORC = ZEROS THEN MOVE 100 TO PORC.
       FIN-CONSULTA.
           EXIT.
       LEER-MAESTRO.
           ADD 1 TO T-MAELEI.
           READ MAESTRO INTO MAE AT END MOVE 'SI' TO FIN
                                    MOVE '9999999999' TO NRO OF MAE.
       FIN-LEER-MAESTRO.
           EXIT.
       LEER-PESQUI.
           ADD 1 TO T-PESQ.
           READ PESQUI AT END MOVE 'SI' TO FIN-P
                              MOVE ALL '9' TO NRO-BEN-P.
       FIN-LEER-PESQUI.
           EXIT.
       BORRA.
           MOVE SPACES TO REGIS (I).
       FIN-BORRA.
           EXIT.
       GRABA.
           IF A-CONC (I) = '02' AND A-SI-ES-VIAL (I) = 'A'
                                AND MASIA = 1
                        THEN DISPLAY REGIS (I)
                             ADD 1 TO ELIMURPE
                             GO TO FIN-GRABA.

           IF A-HABER (I) = ZEROS AND A-ANTIG (I) = ZEROS      
                                  AND A-SI-ES-VIAL (I) = 'M'                                               |
                           AND (A-POS-1-A-5-DE-HISCAR (I) = 'S060D'
                                 OR 'S06N3')
                            THEN GO TO FIN-GRABA.
           IF A-HABER (I) = ZEROS AND A-POS-1-6 (I) = 'SMALV1'
                        THEN GO TO FIN-GRABA.
           IF A-POS-1-12 (I) = '*ADIC.ARBAJ    '              
               THEN IF  I = P
                THEN MOVE '*BONI.ARBAJ     ' TO A-POS-1-12 (I)
                ELSE GO TO FIN-GRABA. 
           WRITE REG-SAL FROM REGIS (I).
           ADD 1 TO T-GRABA.
       FIN-GRABA.
           EXIT.
 *******
       SUMA-ADMI.
           MOVE ZEROS TO HABER-AUX IOMA-AUX HABER-TOT.
           PERFORM ACUMULA-HABER-ADMI THRU FIN-ACUMULA-HABER-ADMI
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = 320.00 * PORC / 100.

           COMPUTE HABER-AUX = HABER-AUX - IOMA-AUX.
           IF  HABER-AUX NOT < IMPORTE
               THEN GO TO VER-EN-PESQUI.
           COMPUTE DIFE = IMPORTE - HABER-AUX.
   ******  IF NRO-ANT = '5014030640' then display NRO-ANT   
   ******               ' importe ' IMPORTE.
       SIGO.
           IF DIFE NOT > ZEROS
********* lo saco porque ya no hay madie mas 07-07-05 
 ********      THEN GO TO VER-EN-PESQUI.
               THEN GO TO FIN-SUMA-ADMI.              
           ADD 1 TO T-MOD-ADMI.
           COMPUTE BONIF-AUX = DIFE / 0.955.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS     TO A-ANTIG (T-REG).
           MOVE ZEROS     TO A-IOMA (T-REG).
           MOVE '02'      TO A-CONC (T-REG).
           MOVE '*DIF.DTO.491/98' TO A-CODIGO (T-REG).
           MOVE BONIF-AUX    TO  A-HABER (T-REG).
           COMPUTE HABER-TOT = HABER-TOT + BONIF-AUX.
       VER-EN-PESQUI.
********* lo saco porque ya no hay madie mas 07-07-05 
********   PERFORM LEER-PESQUI THRU FIN-LEER-PESQUI  UNTIL A-NRO (1)
   ******     = NRO-BEN-P OR A-NRO (1) < NRO-BEN-P.
   ******  IF A-NRO (1) = NRO-BEN-P
   ******
********* THEN ADD 1 TO T-ENC-P
   ******          IF HABER-TOT < HABER-P
   ******            THEN ADD 1 TO T-BON-P
   ******                 COMPUTE DIFE = HABER-P - HABER-TOT
   ******                 ADD 1 TO T-REG
   ******                 MOVE REGIS (1) TO REGIS (T-REG)
   ******                 MOVE ZEROS TO A-ANTIG (T-REG) A-IOMA (T-REG)
   ******                 MOVE '02'  TO A-CONC (T-REG)
   ******                 MOVE '*DIF.DTO.4161/96' TO A-CODIGO (T-REG)
   ******                 MOVE DIFE               TO A-HABER (T-REG).
       FIN-SUMA-ADMI.
           EXIT.
 ********************************
       ACUMULA-HABER-ADMI.
           IF A-EME (I) NOT = SPACE THEN GO TO FIN-ACUMULA-HABER-ADMI.
           IF A-CONC (I) = '01' OR '02'
              THEN IF (A-POS-1 (I) = 'A' OR 'B' OR 'E' OR 'H' OR 'I'
                                  OR 'K' OR 'V' OR 'X' OR '3' OR '8')
                   OR (A-POS-1-DE-HISCAR (I) = 'A' OR 'B' OR 'E' OR 'H'
                        OR 'I'  OR 'K' OR 'V' OR 'X' OR '3' OR '8')
                   OR (A-POS-1-9 (I) = '*DIF.MIN ' OR '*GARAN.MI')
                   OR A-POS-1-12 (I) = '*DIF.BON ANT'           
                   OR A-POS-1-4 (I) = '*D20'           
                   THEN  COMPUTE HABER-AUX = HABER-AUX + A-HABER (I) +
                                             A-ANTIG (I)
                         COMPUTE HABER-TOT = HABER-TOT + A-HABER (I) +
                                             A-ANTIG (I)
                         IF A-POS-1-7 (I) = '*GARAN.' OR
                            A-POS-1-7 (I) = '*BON.JE' OR
                            A-POS-1-9 (I) = '*BON.DTO.' OR
                            A-POS-1-9 (I) = '*BONI.DTO' OR
                            A-POS-1-12 (I) = '*BON.NO REM.' OR
                            A-POS-1-12 (I) = '*BONI.NO REM'
                            THEN NEXT SENTENCE
                            ELSE
                         IF FECHA-AUX < '200502'
                             THEN
                         COMPUTE IOMA-AUX = IOMA-AUX + ((A-HABER (I) +
                                             A-ANTIG (I)) * 0.045)
                             ELSE
                         COMPUTE IOMA-AUX = IOMA-AUX + ((A-HABER (I) +
                                             A-ANTIG (I)) * 0.048)
                   ELSE  COMPUTE HABER-TOT = HABER-TOT + A-HABER (I) +
                                             A-ANTIG (I).
       FIN-ACUMULA-HABER-ADMI.
           EXIT.
       SUMA-JOTA.
           MOVE ZEROS TO HABER-AUX IOMA-AUX HABER-TOT DIFE.
           PERFORM ACUMULA-HABER-JOTA THRU FIN-ACUMULA-HABER-JOTA
              VARYING I FROM 1 BY 1 UNTIL I > T-REG.
           COMPUTE IMPORTE = 9700.00 * PORC / 100.
    *******IF  A-NRO (1)  = '1025151090'
   *********    THEN DISPLAY ' HABER-AUX ' HABER-AUX 
   *********         DISPLAY ' IMPORTE   ' IMPORTE.   
     ****      IF TIP-LIQUI = 'L'
     ****         THEN NEXT SENTENCE
     ***         ELSE COMPUTE HABER-AUX = HABER-AUX - IOMA-AUX.
           IF A-SEC1 (1) = 'H' AND A-MARCA-IOMA (1) = '0'     
              THEN NEXT SENTENCE
              ELSE COMPUTE HABER-AUX = HABER-AUX - IOMA-AUX.
     ***   IF  A-NRO (1)  = '0000015030'
     ***        THEN DISPLAY ' HABER-AUX ' HABER-AUX ' IMPORTE '
     ***             IMPORTE.
           IF  HABER-AUX NOT > IMPORTE
               THEN GO TO FIN-SUMA-JOTA.
           COMPUTE DIFE = HABER-AUX - IMPORTE.
    ****** IF  A-NRO (1)  = '1025151090'
   *******      THEN DISPLAY ' DIFE  ' DIFE.
       SIGO-J.
           IF DIFE NOT > ZEROS 
               THEN GO TO FIN-SUMA-JOTA.

           IF  A-NRO (1)  = '1049376370'
               THEN GO TO FIN-SUMA-JOTA.

           ADD 1 TO T-MOD-J.
           ADD 1 TO T-REG.
           MOVE REGIS (1) TO REGIS (T-REG).
           MOVE ZEROS     TO A-ANTIG (T-REG).
           MOVE ZEROS     TO A-IOMA (T-REG).
           MOVE ZEROS     TO A-NRO-SEC (T-REG).
           MOVE '1R'      TO A-CONC (T-REG).
           MOVE '*TOPE DTO.5/2000' TO A-CODIGO (T-REG).
           MOVE ALL ZEROS          TO A-CODIGO2 (T-REG).
           MOVE DIFE         TO  A-HABER (T-REG).
           MOVE ZEROS        TO  A-NRO-SEC (T-REG).
       FIN-SUMA-JOTA.
           EXIT.
 ********************************
       ACUMULA-HABER-JOTA.
           IF A-EME (I) NOT = SPACE THEN GO TO FIN-ACUMULA-HABER-JOTA.
           IF A-CONC (I) = '01' OR '02'
            THEN IF A-POS-1-DE-HISCAR (I) = 'J' OR
            (A-POS-1-A-5-DE-HISCAR (I) = 'X2502' OR 'X2602' OR 'X2702')
                   OR (A-POS-1-9 (I) = '*DIF.MIN ' OR '*GARAN.MI')
                   OR A-POS-1-12 (I) = '*DIF.BON ANT'
                   THEN  COMPUTE HABER-AUX = HABER-AUX + A-HABER (I) +
                                             A-ANTIG (I)
                         COMPUTE HABER-TOT = HABER-TOT + A-HABER (I) +
                                             A-ANTIG (I)
                         IF A-POS-1-7 (I) = '*GARAN.' OR
                            A-POS-1-7 (I) = '*BON.JE' OR
                            A-POS-1-9 (I) = '*BON.DTO.' OR
                            A-POS-1-9 (I) = '*BONI.DTO' OR
                            A-POS-1-12 (I) = '*BON.NO REM.' OR
                            A-POS-1-12 (I) = '*BONI.NO REM'
                            THEN NEXT SENTENCE
                            ELSE
                         IF FECHA-AUX < '200502'
                             THEN
                         COMPUTE IOMA-AUX = IOMA-AUX + ((A-HABER (I) +
                                             A-ANTIG (I)) * 0.045)
                             ELSE
                         COMPUTE IOMA-AUX = IOMA-AUX + ((A-HABER (I) +
                                             A-ANTIG (I)) * 0.048)
                   ELSE  COMPUTE HABER-TOT = HABER-TOT + A-HABER (I) +
                                             A-ANTIG (I).
       FIN-ACUMULA-HABER-JOTA.
           EXIT.
