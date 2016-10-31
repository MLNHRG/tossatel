       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. 'GMAEBASE1'.                                             000
       AUTHOR. 'MONTSERRAT  26/08/96'                                       000
      *REMARKS.'PROGRAMA LLAMADOR QUE CALCULA LOS HABERES'.                 000
      *REMARKS.'SE AGREGA PARA MONICA UNA PARAMETRO A PARA'.                000
      *REMARKS.'DIFERENCIAR SERVICIO PENITENCIARIO 29-03-2005'.             000
       ENVIRONMENT DIVISION.                                                000
       CONFIGURATION SECTION.                                               000
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.                                                001
       FILE-CONTROL.                                                        001
           SELECT MAESTRO ASSIGN MAESTRO.
           SELECT HISCAR ASSIGN HISCAR.
           SELECT MAEACT ASSIGN MAEACT.
           SELECT VIALIDAD ASSIGN VIALID.
           SELECT ARCHLIS ASSIGN ARCHLIS.
       DATA DIVISION.                                                       001
       FILE SECTION.                                                        001
       FD  MAESTRO                                                          001
           RECORD 140                                                       002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-MAE.                                          002
       01  REG-MAE PIC X(140).                                              002
       FD  HISCAR                                                           001
           RECORD 302                                                       001
           LABEL RECORD STANDARD                                            001
           DATA RECORD REG-HISCAR.
       01  REG-HISCAR PIC X(302).                                           001
       FD  MAEACT                                                           001
           RECORD 140                                                       002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-ACT.                                          002
       01  REG-ACT    PIC X(140).                                           002
       FD  VIALIDAD                                                         001
           RECORD 140                                                       002
           LABEL RECORD IS STANDARD                                         002
           DATA RECORD IS REG-VIAL.                                         002
       01  REG-VIAL   PIC X(140).                                           002
       FD  ARCHLIS                                                          001
           RECORD 118                                                       002
           LABEL RECORD STANDARD                                            002
           DATA RECORD IS REG-LIS.                                          002
       01  REG-LIS    PIC X(118).                                           002
       WORKING-STORAGE SECTION.                                             001
           COPY WSFECHA.CPY.
       77  LEIDO-M           PIC 9(8) VALUE ZEROS.
       77  LEIDO-H           PIC 9(8) VALUE ZEROS.
       77  T-GRABA           PIC 9(8) VALUE ZEROS.
       77  ELI-ADI           PIC 9(8) VALUE ZEROS.
       77  TOT01             PIC 9(8) VALUE ZEROS.
       77  TOT02             PIC 9(8) VALUE ZEROS.
       77  TOT01G             PIC 9(8) VALUE ZEROS.
       77  TOT02G             PIC 9(8) VALUE ZEROS.
       77  T-BONIFIC         PIC 9(8) VALUE ZEROS.
       77  T-BON-2R          PIC 9(8) VALUE ZEROS.
       77  T-VIALIDAD        PIC 9(8) VALUE ZEROS.
       77  I                 PIC 99         VALUE ZEROS.
       77  K                 PIC 99         VALUE ZEROS.
       77  DIFERENCIA        PIC S9(9)V99   VALUE ZEROS.
       77  FIN-MAE           PIC XX  VALUE 'NO'.
       77  FIN-HIS           PIC XX       VALUE 'NO'.
       77  CUENTA-HOJA       PIC 9(6)     VALUE ZEROS.
       77  NUM-SEC           PIC 9(6)     VALUE ZEROS.                      009
       77  MASC1             PIC ZZZ.ZZ9,99.
       77  MASC2             PIC ZZZ.ZZ9,99.
       01  PARAM.
           02 INDICA-RETRO  PIC X.
           02 FECHA-LIQ.
              03  FECHA-LIQ-AA  PIC X(4).
              03  FECHA-LIQ-MM  PIC XX.
           02 PORC-MASIVO       PIC XX.
           02 TIPO-LIQ          PIC X(2).
           02 TITU-PARAM        PIC X(68).
           02 SACA-PLANI        PIC X.
      *
      *      DISEºO DE MAESTRO DESEMPAQUETADO                               007
      *                                                                     007
       01  REG-LIQ.
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
                 04 POS-1-4.                                                009
                   05 POS-1-2.                                              009
                      06 POS-1     PIC X.                                   009
                      06 POS-2     PIC X.                                   009
                   05 POS-3-4.                                              009
                      06 POS-3     PIC X.                                   009
                      06 POS-4     PIC X.                                   009
                 04 POS-5-6.                                                009
                   05 POS-5    PIC X.                                       009
                   05 POS-6    PIC X.                                       009
                 04 POS-7-8    PIC XX.                                      009
                 04 POS-9-10   PIC XX.                                      009
                 04 POS-11-12  PIC XX.                                      009
                 04 POS-13-14.                                              009
                    05 POS-13  PIC X.                                       009
                    05 POS-14  PIC X.                                       009
                 04 POS-15-16.                                              009
                    05 POS-15  PIC X.                                       009
                    05 POS-16  PIC X.                                       009
                 04 FILLER     PIC X(4).
           02 CODIGO2          PIC X(15).                                   009
           02 NRO-SEC          PIC 9(6).                                    011
           02 POS-138-139      PIC XX.                                      011
           02 POS-140          PIC X.                                       011
       01  REG-HISC.                                                        003
           02 H-FECHA                    PIC X(6).                          003
           02 H-CODIGO.                                                     003
              03 H-SECCI.                                                   003
                 04 SECC-1               PIC X.                             004
                 04 SECC-2               PIC X.                             004
              03 AGRUP                   PIC X.                             004
              03 CARGO                   PIC 999.                           004
              03 CARG1 REDEFINES CARGO.                                     004
                 04 COD-CARG1            PIC X.                             004
                 04 COD-CARG2            PIC XX.                            004
              03 FILLER                  PIC X(4).                          004
              03 BONIF                   PIC 99.                            004
              03 BONIF1 REDEFINES BONIF.                                    004
                 04 LET1                 PIC X.                             004
                 04 LET2                 PIC X.                             004
              03 FILLER                  PIC XXX.                           004
           02 H-HABER                    PIC 9(9)V99   COMP-3.              004
      **                                              * POS 20 - 25
           02 SUPL1                      PIC 9(9)V99   COMP-3.              005
      **                                              * POS 26 - 31
           02 SUPL2                      PIC 9(9)V99   COMP-3.              005
      **                                              * POS 32 - 37
           02 TABLA.                                                        005
              03 H-ANTIG   OCCURS 41 TIMES PIC 9(9)V99 COMP-3.              005
      **                                              * POS 38 - 283
           02 SUMAFIJ1                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 284- 289
           02 SUMAFIJ2                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 290- 295
           02 SUMAFIJ3                   PIC S9(7)V99 COMP-3.               005
      **                                              * POS 296- 300
       01  ESTRU.
           02 REGIS  OCCURS 20.
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
                 05 A-POS-5-6.                                              009
                    06 A-POS-5    PIC X.                                    009
                    06 A-POS-6    PIC X.                                    009
                 05 A-POS-7-8    PIC XX.                                    009
                 05 A-POS-9-10   PIC XX.                                    009
                 05 A-POS-11-12  PIC XX.                                    009
                 05 A-POS-13-14.                                            009
                    06 A-POS-13  PIC X.                                     009
                    06 A-POS-14  PIC X.                                     009
                 05 A-POS-15-16.                                            009
                    06 A-POS-15  PIC X.                                     009
                    06 A-POS-16  PIC X.                                     009
             04    FILLER       PIC X(3).
           03 A-MARDGI           PIC X.
           03 A-CODIGO2          PIC X(15).                                 009
           03 A-NRO-SEC          PIC 9(6).                                  011
           03 A-POS-138-139      PIC XX.                                    009
           03 A-POS-140          PIC X.                                     009
       01  MARCA-MAS.
           02 MARCA-MASIVO    PIC 9.
       01  IMPO-MUNI-X.
           02 IMPO-HAB-MUN    PIC S9(7)V99.
           02 IMPO-ANT-MUN    PIC S9(5)V99.
      *
       01  REG-IMP-AUX.                                                     028
           02 X-CONC    PIC X(2).                                           028
           02 X-SEC     PIC X(2).                                           028
           02 X-BENEF   PIC X(10).                                          029
           02 X-APYN.                                                       029
              03 X-APYN1   PIC X(20).                                       029
              03 X-REL     PIC X.                                           029
              03 X-FECH    PIC X(6).                                        030
           02 X-IMPOR1     PIC S9(9)V99.                                    030
           02 X-IMPOR2     PIC S9(9)V99.                                    030
           02 X-IMPOR3     PIC S9(9)V99.                                    030
           02 X-IMPOR4     PIC S9(9)V99.                                    030
           02 X-DIFE       PIC S9(8)V99.                                    030
           02 X-CODIGO     PIC X(20).                                       029
           02 X-OBSER      PIC XX.                                          029
           02 X-SI-LETRA   PIC X.                                           029
      *
       01  CLAVE-MAE-AUX.
           02 CLAVE1        PIC X(12).
           02 CLAVE2        PIC X(6).
       01  CLAVE-HIS-AUX.
           02 H-CLAVE1      PIC X(12).
           02 H-CLAVE2      PIC X(6).
       PROCEDURE DIVISION.                                                  043
           COPY PDFECHA.CPY.
           OPEN INPUT MAESTRO HISCAR
                OUTPUT VIALIDAD MAEACT ARCHLIS.
     **
           ACCEPT PARAM.
           IF INDICA-RETRO = 'M' OR 'R' OR 'A' OR 'C'
              THEN NEXT SENTENCE                   
              ELSE DISPLAY ' *** ERROR *** INDICAR M (MES) O'
                                   ' R (RETROACTIVO '
                           GO TO CERRAR.
     **
           PERFORM LEE-HISCAR THRU FIN-LEE-HISCAR.
           DISPLAY ' ***************************************'.
           DISPLAY ' *** TRABAJANDO CON FECHA DE HISCAR ****' H-FECHA.
           DISPLAY ' ***************************************'.
     **
           PERFORM LEE-MAESTRO THRU FIN-LEE-MAESTRO.
     **
           PERFORM PROCESO THRU FIN-PROCESO UNTIL FIN-MAE = 'SI'.
       CERRAR.
           DISPLAY ' ******  TOTALES DE CONTROL PROGRAMA GMAEBASE ****'.
           DISPLAY ' TOTAL REG. LEIDOS LIQUIDACION : ' LEIDO-M.
           DISPLAY ' TOTAL REG. GRABADOS           : ' T-GRABA.
           DISPLAY ' TOTAL REG.LEI 01 BLANCO          : ' TOT01.   
           DISPLAY ' TOTAL REG.LEI 02 BLANCO          : ' TOT02.   
           DISPLAY ' TOTAL REG.GRA 01 BLANCO          : ' TOT01G.   
           DISPLAY ' TOTAL REG.GRA 02 BLANCO          : ' TOT02G.   
           DISPLAY ' TOTAL REG. VIALIDAD           : ' T-VIALIDAD.
           DISPLAY ' TOTAL REG. BONIFICACIONES     : ' T-BONIFIC.
           DISPLAY ' TOTAL REG. 2R                 : ' T-BON-2R.
           DISPLAY ' TOTAL REG. ADICIO. ELIMINDS   : ' ELI-ADI.
           CLOSE MAESTRO  MAEACT ARCHLIS VIALIDAD HISCAR.
           STOP RUN.
       PROCESO.
           MOVE SPACES TO ESTRU X-OBSER MARCA-MAS.
           MOVE ZEROS TO IMPO-HAB-MUN IMPO-ANT-MUN.
   *****     para marzo 20001      
   ******
   ******
   ******        IF NRO ='7053005780' AND SEC='3Z'
   ******                       THEN MOVE '32' TO SEC.
   ******     por unica vez
   ******
   ******
           IF CONC = '02' AND EME = ' ' AND POS-1-4 = '*DIF'
              THEN MOVE REG-LIQ TO REGIS (1)
                   PERFORM GRABA-CONC-02 THRU FIN-GRABA-CONC-02
                   GO TO LEER.

  *******  DISPLAY '1. CLAVE-MAE: ' CLAVE-MAE-AUX ' CLAVE-HIS: '
  *******   CLAVE-HIS-AUX.

           PERFORM LEE-HISCAR THRU FIN-LEE-HISCAR UNTIL
             CLAVE-MAE-AUX = CLAVE-HIS-AUX  OR
             CLAVE-MAE-AUX < CLAVE-HIS-AUX  OR FIN-HIS = 'SI'.
      *
 *********  DISPLAY '2. CLAVE-MAE: ' CLAVE-MAE-AUX ' CLAVE-HIS: '
 *********  CLAVE-HIS-AUX.
      *
           IF CLAVE-MAE-AUX = CLAVE-HIS-AUX
              THEN PERFORM BUSCA-CODIGO THRU FIN-BUSCA-CODIGO
    ******    ELSE DISPLAY ' NO ENCONTRO CARG icla-mae ' CLAVE-MAE-AUX
    ******                                    'cla-his ' CLAVE-HIS-AUX
              ELSE MOVE 1 TO MARCA-MAS.
      *
    ****   IF NRO = '1043487630' THEN DISPLAY ' SALE      ' REG-LIQ 
    ****   DISPLAY ' SALE      ' REG-LIQ 
    ****       ' ' MARCA-MAS.
           IF MARCA-MAS = '1'
              THEN MOVE '*'  TO  POS-140
                   CALL 'GMAENOCODI' USING PARAM REG-LIQ REG-HISC ESTRU
                      MARCA-MAS
                   MOVE 'MM' TO X-OBSER
              ELSE IF MARCA-MAS = '5'
                      THEN MOVE '*'  TO  POS-140
                           MOVE 'ER' TO X-OBSER.
      *
           PERFORM GRABA THRU FIN-GRABA VARYING I FROM 1 BY 1
            UNTIL I > 20 OR REGIS (I) = SPACES.
       LEER.
           PERFORM LEE-MAESTRO THRU FIN-LEE-MAESTRO.
       FIN-PROCESO. EXIT.
       LEE-MAESTRO.
           READ MAESTRO INTO REG-LIQ AT END MOVE 'SI' TO FIN-MAE
                              GO TO FIN-LEE-MAESTRO.
           ADD 1 TO LEIDO-M.
           IF CONC = '01' AND EME = ' '
                         THEN ADD 1 TO TOT01
                         ELSE
           IF CONC = '02' AND EME = ' '
                         THEN ADD 1 TO TOT02.
           IF INDICA-RETRO = 'R' OR 'A' OR 'C'
              THEN MOVE NRO-SEC       TO NUM-SEC
                   MOVE FECHA-RETRO TO FECHA-LIQ 
              ELSE MOVE H-FECHA     TO FECHA-LIQ.
           MOVE CODIGO2       TO CLAVE1.
           MOVE FECHA-LIQ     TO CLAVE2.
    ****   IF NRO = '1043487630' THEN DISPLAY ' Leyo    ' REG-LIQ.
       FIN-LEE-MAESTRO. EXIT.
       LEE-HISCAR.
           ADD 1 TO LEIDO-H.
           READ HISCAR INTO REG-HISC AT END MOVE 'SI' TO FIN-HIS
                                            MOVE ALL 'Z' TO H-FECHA
                                            MOVE ALL 'Z' TO H-CODIGO.
           MOVE H-CODIGO      TO H-CLAVE1.
           MOVE H-FECHA       TO H-CLAVE2.
       FIN-LEE-HISCAR. EXIT.
       BUSCA-CODIGO.
      *    IF NRO = '7029332580' THEN DISPLAY ' en codigo ' REG-LIQ 
      *                       DISPLAY CODIGO.
 *************** agrego el 27-4-09
  ******** IF POS-1= '3'  AND (POS-6 = 'G' OR ' Z' OR 'X')
  *******                 AND FECHA-LIQ > '200903' 
  *******     THEN
  *******  CALL 'GMAEGRAFA' USING PARAM REG-LIQ REG-HISC ESTRU            
 ********                            MARCA-MAS
 ********     ELSE
           IF POS-1-2 = 'FB' AND FECHA-LIQ > '201508' 
              THEN
           CALL 'GMAEFERRO' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1-2 = 'FD' AND FECHA-LIQ > '201003' 
              THEN
           CALL 'GMAEDEFP' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1-2 = 'F0' AND FECHA-LIQ > '201303' 
              THEN
           CALL 'GMAEABSA' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1 = 'Y' AND FECHA-LIQ > '201501' 
              THEN
           CALL 'GMAEHIPO' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1-2 = 'PM' AND FECHA-LIQ > '201103' 
              THEN
           CALL 'GMAEPTAM' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
 *************** agrego el 8-9-10
     ***** IF POS-1-2 = '0D' AND FECHA-LIQ > '201008' 
           IF POS-1-2 = '0D' AND FECHA-LIQ > '201003' 
              THEN
           CALL 'GMAEDEBB' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
 *************** agrego el 22-4-09
           IF POS-1-2 = 'NA' AND FECHA-LIQ > '199704' 
              THEN
           CALL 'GMAEAUTO' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1-2 = '92' OR '93' OR '94' OR '96' OR '97' OR '9L'
              THEN
           CALL 'GMAELEYESP' USING PARAM REG-LIQ REG-HISC ESTRU           
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '21' AND (POS-13 = 'M' OR POS-15 = 'M')
              THEN
           CALL 'GMAEMAGIMUNI' USING PARAM REG-LIQ REG-HISC ESTRU           
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '01' OR '21' OR '90' OR '91'
              THEN
           CALL 'GMAEMAGIPROV' USING PARAM REG-LIQ REG-HISC ESTRU           
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '05' OR '06'
              THEN
           CALL 'GMAEDOCE' USING PARAM REG-LIQ REG-HISC ESTRU            
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '02'            
              THEN
           CALL 'GMAESERVPEN' USING PARAM REG-LIQ REG-HISC ESTRU             
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '12'
              THEN
           CALL 'GMAEPOLICI' USING PARAM REG-LIQ REG-HISC ESTRU             
                                   MARCA-MAS
              ELSE
           IF POS-1-2 = '10' OR           
             (POS-1-2 = '11' AND FECHA-LIQ > '200804') 
              THEN
           CALL 'GMAEPODJUD' USING PARAM REG-LIQ REG-HISC ESTRU            
                                   MARCA-MAS
              ELSE
           IF POS-1 = 'V'
              THEN
      **********************                          SE ANEXO 71-72
                   IF SI-ES-VIAL = 'V'
                      THEN
           CALL 'GMAEVIAL' USING PARAM REG-LIQ REG-HISC ESTRU            
                                   MARCA-MAS
                      ELSE
                   IF SI-ES-VIAL = '3'
                      THEN
           CALL 'GMAEADMI' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
                      ELSE
           CALL 'GMAEVIAL' USING PARAM REG-LIQ REG-HISC ESTRU           
                                     MARCA-MAS
      **********************                          SE ANEXO 71-72
              ELSE
           IF POS-1 = 'M' OR 'W'
              THEN
           CALL 'GMAEMUNI' USING PARAM REG-LIQ REG-HISC ESTRU             
                                     MARCA-MAS IMPO-MUNI-X
              ELSE
           IF POS-1 = 'B' OR 'E' OR 'H' OR 'I' OR 'K'
              THEN
           CALL 'GMAECMED' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
 *************** agrego el 4-9-08
  ******** IF POS-1 = 'C' AND FECHA-LIQ > '200803' 
           IF POS-1 = 'C' AND FECHA-LIQ > '200601' 
              THEN
           CALL 'GMAECASI' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
 *************** agrego el 4-8-08
    ****** IF POS-1 = 'A' AND FECHA-LIQ > '200803' 
    ****** IF POS-1 = 'A' AND FECHA-LIQ > '200601' mov set13 cargo sdos dde ene04.
           IF POS-1 = 'A' AND FECHA-LIQ > '200401' 
              THEN
           CALL 'GMAEASTI' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1 = '3' OR 'J' OR 'X' OR 'A' 
              THEN
           CALL 'GMAEADMI' USING PARAM REG-LIQ REG-HISC ESTRU            
                                     MARCA-MAS
              ELSE
           IF POS-1 = '7' OR POS-1-2 = '95'
              THEN
           CALL 'GMAEMLVD' USING PARAM REG-LIQ REG-HISC ESTRU           
                                   MARCA-MAS
              ELSE
           IF POS-1 = '8'
              THEN
           CALL 'GMAELEY' USING PARAM REG-LIQ REG-HISC ESTRU              
                                   MARCA-MAS
              ELSE
           IF POS-1 = 'O'
              THEN
           CALL 'GMAEOSANI' USING PARAM REG-LIQ REG-HISC ESTRU            
                                   MARCA-MAS
              ELSE
           IF (POS-1 = 'S' OR 'D') OR
                      (POS-1-2 = 'LE' OR '07' OR '08')
              THEN
           CALL 'GMAELEGIS' USING PARAM REG-LIQ REG-HISC ESTRU           
                                   MARCA-MAS
              ELSE
            DISPLAY '******  CODIGO DE CARGO NO CONSIDERADO  *******'
                  NRO ' ' APYN ' ' CODIGO ' ' CODIGO2
           CALL 'GMAENOCODI' USING PARAM REG-LIQ REG-HISC ESTRU             
                                           MARCA-MAS.
       FIN-BUSCA-CODIGO. EXIT.
       IMPRIME.
           MOVE A-CONC (I)    TO X-CONC.
           MOVE A-SEC (I)     TO X-SEC.
           MOVE A-NRO (I)     TO X-BENEF.
           MOVE A-APYN (I)    TO X-APYN.
           IF INDICA-RETRO = 'R' OR 'C' OR 'A'
              THEN  MOVE ' ' TO X-REL
                    MOVE FECHA-LIQ TO X-FECH.
           MOVE HABER        TO X-IMPOR1.
           MOVE ANTIG        TO X-IMPOR2.
           MOVE CODIGO       TO X-CODIGO.
           MOVE SI-LETRA-ANTIG  TO X-SI-LETRA.
     *********************************   CONSIDERO EL CASO DE MUNIC.
           IF IMPO-HAB-MUN > 0 OR IMPO-ANT-MUN > 0
              THEN MOVE IMPO-HAB-MUN TO X-IMPOR3
                   MOVE IMPO-ANT-MUN TO X-IMPOR4
                   COMPUTE DIFERENCIA = (IMPO-HAB-MUN + IMPO-ANT-MUN) -
                                        (HABER + ANTIG)
              ELSE MOVE A-HABER (I) TO X-IMPOR3
                   MOVE A-ANTIG (I) TO X-IMPOR4
                   COMPUTE DIFERENCIA = (A-HABER (I) + A-ANTIG (I)) -
                                (HABER + ANTIG).
           MOVE DIFERENCIA    TO X-DIFE.
           WRITE REG-LIS FROM REG-IMP-AUX.
       FIN-IMPRIME. EXIT.
       GRABA.
*********para  registros agregdos para calculo de vialidad
            IF A-SI-ES-VIAL (I) = 'I'
  ********      THEN DISPLAY REGIS (I)                       
                THEN IF A-POS-1-A-16 (I) = '*RES.1661/11    '
                       THEN NEXT SENTENCE                                   
  ***************      THEN DISPLAY 'lo graba ' A-POS-1-A-16 (I)
                       ELSE COMPUTE ELI-ADI = ELI-ADI + 1
                           GO TO FIN-GRABA.

           IF INDICA-RETRO = 'R'
              THEN MOVE NUM-SEC TO A-NRO-SEC (I).
           IF A-CONC (I)  = '2R'
              THEN ADD  1 TO   T-BON-2R
              ELSE
           IF A-POS-1 (I) NOT = '*'
              THEN PERFORM IMPRIME THRU FIN-IMPRIME
              ELSE ADD  1 TO   T-BONIFIC.
           IF A-SI-ES-VIAL (I) = '3' OR 'V'
     ***      THEN IF INDICA-RETRO = 'M'
                      THEN WRITE REG-VIAL FROM REGIS (I)
                           ADD 1 TO T-VIALIDAD
                           GO TO FIN-GRABA.
     ***
           IF MARCA-MAS = '1' OR '5'      
              THEN  MOVE '*' TO A-POS-140 (I).
     ***
     ****  DISPLAY REGIS (I).
     ***
           WRITE REG-ACT FROM REGIS (I).
           IF A-CONC (I) = '01' AND A-EME (I) = ' '
                        THEN ADD 1 TO TOT01G
                        ELSE
           IF A-CONC (I) = '02' AND A-EME (I) = ' '
                        THEN ADD 1 TO TOT02G.
           ADD 1 TO T-GRABA.
       FIN-GRABA. EXIT.
       GRABA-CONC-02.
           IF INDICA-RETRO = 'R'
              THEN MOVE NUM-SEC TO A-NRO-SEC (1).
           IF A-SI-ES-VIAL (1) = '3' OR 'V'
              THEN WRITE REG-VIAL FROM REGIS (1)
                   ADD 1 TO T-VIALIDAD
              ELSE WRITE REG-ACT FROM REGIS (1)
                   ADD 1 TO T-GRABA.
           IF A-CONC (1) = '01' AND A-EME (1) = ' '
                        THEN ADD 1 TO TOT01G
                        ELSE
           IF A-CONC (I) = '01' AND A-EME (1) = ' '
                        THEN ADD 1 TO TOT02G.
       FIN-GRABA-CONC-02. EXIT.
       CALCULA-MASIVO.
           MOVE 'MM' TO X-OBSER.
           IF PORC-MASIVO NUMERIC
              THEN MOVE PORC-MASIVO TO K
                   COMPUTE A-HABER (1) = A-HABER (1) * K / 100
                   COMPUTE A-ANTIG (1) = A-ANTIG (1) * K / 100.
       FIN-CALCULA-MASIVO. EXIT.
