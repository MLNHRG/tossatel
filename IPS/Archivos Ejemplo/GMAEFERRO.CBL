       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. GMAEFERRO.                                               000
       AUTHOR. 'MONICA 29-9-16'.                                            000
      *REMARKS.'CALCULA HABER Y BONIFICACIONES DE FERROBAIRES'.             000
      *REMARKS.' DESDE PPTO AGOSTO 2015                      '.             000
       ENVIRONMENT DIVISION.                                                000
       DATA DIVISION.                                                       000
       WORKING-STORAGE SECTION.                                             001
           COPY WSFECHA.CPY.
       77  I                 PIC 99 VALUE ZEROS.
       77  J                 PIC 99 VALUE ZEROS.
       77  K                 PIC 99 VALUE ZEROS.
       77  PORC              PIC 999 VALUE ZEROS.
       77  ANTIG1            PIC 99 VALUE ZEROS.
       77  ANTIG2            PIC 99 VALUE ZEROS.
       77  ANTIG1-N          PIC S99 VALUE ZEROS.
       77  IND-F             PIC 99 VALUE ZEROS.
       77  LEYENDA           PIC X(20).
       77  BONIFIC           PIC S9(7)V99 VALUE ZEROS.
       77  ANTIGBON          PIC S9(7)V99 VALUE ZEROS.
       77  TITU              PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR1            PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR2            PIC S9(7)V99 VALUE ZEROS.
       77  CON-ANTIG         PIC AA VALUE 'NO'.
       77  BONCONHIS         PIC S9(7)V99 VALUE ZEROS.
       77  HAB-AUX           PIC S9(7)V99 VALUE ZEROS.
       77  ENCONTRE          PIC AA VALUE 'NO'.
       77  BON-REA           PIC S9(7)V99  VALUE ZEROS.
       77  ANTIG-AUX         PIC S9(10)V99 VALUE ZEROS.
       77  ANTIG-NEW         PIC S9(10)V99 VALUE ZEROS.
      ***
       77  ESC405            PIC S9(7)V99  VALUE ZEROS.
       77  ESC111            PIC S9(7)V99  VALUE ZEROS.
       77  ESC118            PIC S9(7)V99  VALUE ZEROS.
       77  ESC201            PIC S9(7)V99  VALUE ZEROS.
       77  ESC125            PIC S9(7)V99  VALUE ZEROS.
       77  ESC223            PIC S9(7)V99  VALUE ZEROS.
      *
       01  HORAPROC.
           02 H-PROC       PIC XX.
           02 M-PROC       PIC XX.
           02 S-PROC       PIC XX.
           02 S-PROC1      PIC XXX.

      ***TABLA DE LETRAS DE ANTIGUEDAD DESDE PPTO. 9601
  ********     saco tabla de antig. porque cobran todos el 3% por todos los anios.
      ***

       01  CODIGO-AUXILIAR-X.                                               009
              03 M-1-16-X.                                                  009
                 04 POS-1-2-X.                                              009
                   05 POS-1-X  PIC X.                                       009
                   05 POS-2-X  PIC X.                                       009
                 04 POS-3-4-X.                                              009
                   05 POS-3-X  PIC X.                                       009
                   05 POS-4-X  PIC X.                                       009
                 04 POS-5-6-X.                                              009
                   05 POS-5-X  PIC X.                                       009
                   05 POS-6-X  PIC X.                                       009
                 04 POS-7-8-X  PIC XX.                                      009
                 04 POS-9-10-X  PIC XX.                                     009
                 04 POS-11-12-X  PIC XX.                                    009
                 04 POS-13-14-X.                                            009
                    05 POS-13-X  PIC X.                                     009
                    05 POS-14-X  PIC X.                                     009
                 04 POS-15-16-X.                                            009
                    05 POS-15-X  PIC X.                                     009
                    05 POS-16-X  PIC X.                                     009
              03 M-17-20-X.                                                  009
                 04 POS-17-20-N  PIC 99V99.                                 009
       01   CODIGO-FIS-X REDEFINES CODIGO-AUXILIAR-X.
              03 POS-1-3-X    PIC X(3).
              03 POS-4-5-X    PIC XX.
              03 FILLER       PIC X(15).
       01   CODIGO-FISCA-X REDEFINES CODIGO-AUXILIAR-X.
              03 POS-1-6-X.
                 04 POS-1-AUX  PIC X.
                 04 POS-2-6-X  PIC X(5).
              03 FILLER        PIC X(14).
        01 CODIGO-AUX.                                                      009
           02 POS-1-6-A.                                                    009
              03 POS-1-2-A.                                                 009
                 04 POS-1-A  PIC X.                                         009
                 04 POS-2-A  PIC X.                                         009
              03 POS-3-4-A.                                                 009
                 04 POS-3-A  PIC X.                                         009
                 04 POS-4-A  PIC X.                                         009
              03 POS-5-6-A.                                                 009
                 04 POS-5-A  PIC X.                                         009
                 04 POS-6-A  PIC X.                                         009
              03 POS-7-8-A PIC XX.                                          009
              03 POS-9-10-A PIC XX.                                         009
              03 POS-11-12-A PIC XX.                                        009
              03 POS-13-14-A.                                               009
                 04 POS-13-A  PIC X.                                        009
                 04 POS-14-A  PIC X.                                        009
              03 POS-N-13-14A REDEFINES POS-13-14-A PIC 99.
              03 POS-15-16-A.                                               009
                 04 POS-15-A  PIC X.                                        009
                 04 POS-16-A  PIC X.                                        009
              03 POS-17-20-A  PIC X(4).                                     009

       LINKAGE SECTION.
       01  REG-PARAM.
           02  COD-RETRO         PIC X.
           02  FECHA-MES         PIC X(6).
           02  PORC-MAS          PIC XX.
           02  TITUS             PIC X(70).
           02  SACA-PL           PIC X.
       01  REG-MAE.
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
                 04 POS-1-A-6.                                              009
                  05 POS-1-5.                                               009
                   06 POS-1-2.                                              009
                      07 POS-1     PIC X.                                   009
                      07 POS-2     PIC X.                                   009
                    06 POS-3-4.                                             009
                      07 POS-3     PIC X.                                   009
                      07 POS-4     PIC X.                                   009
                  05 R-POS-1-5 REDEFINES POS-1-5.                           009
                    06 FILLER      PIC X.
                    06 POS-2       PIC X.
                    06 POS-3-4-5   PIC XXX.
                  05 R1-POS-1-5 REDEFINES POS-1-5.                           009
                    06 POS-1-3     PIC XXX.
                    06 FILLER      PIC XX.
                  05 POS-6           PIC X.                                 009
                 04 POS-1-A-6RR REDEFINES POS-1-A-6.                        009
                  05 POS-1-3RR     PIC XXX.                                 009
                  05 POS-4-6       PIC 999.                                 009
                 04 POS-7          PIC X.                                   009
                 04 POS-8-9-10.                                             009
                    05 POS-8-9.                                             009
                       06 POS-8     PIC X.                                  009
                       06 POS-9     PIC X.                                  009
                    05 POS-10       PIC X.                                  009
                 04 R-POS-8-9-10 REDEFINES POS-8-9-10.                      009
                   05 FILLER       PIC X.                                   009
                   05 POS-9-10     PIC XX.                                  009
                 04 POS-11-12.                                              009
                   05 POS-11       PIC X.                                   009
                   05 POS-12       PIC X.                                   009
                 04 POS-11-12R REDEFINES POS-11-12 PIC 99.                  009
                 04 POS-13-14.                                              009
                    05 POS-13      PIC X.                                   009
                    05 POS-14      PIC X.                                   009
                 04 POS-N-13-14 REDEFINES POS-13-14 PIC 99.                 009
                 04 POS-15-16-17.                                           009
                    05 POS-15      PIC X.                                   009
                    05 POS-16      PIC X.                                   009
                    05 POS-17      PIC X.                                   009
                 04 POS-15-16-17N1 REDEFINES POS-15-16-17.                  009
                    05 POS-15-16   PIC XX.                                  009
                    05 POS-17N     PIC  9.                                  009
                 04 POS-15-16-17N2 REDEFINES POS-15-16-17.                  009
                    05 POS-15-16N  PIC 99.                                  009
                    05 POS-17N2    PIC  9.                                  009
                 04 POS-15-16-17N3 REDEFINES POS-15-16-17.                  009
                    05 POS-15N3    PIC  9.                                  009
                    05 POS-16-17   PIC XX.                                  009
                    05 POS-16-17N  REDEFINES POS-16-17 PIC 99.              009
               03 POS-18-20.                            
                  04 POS-18           PIC X.
                  04 POS-19-20.                 
                     05 POS-19           PIC X.
                     05 POS-20           PIC X.

           02 CODIGO2.                                                      009
              03 POS-1-DE-HISCAR   PIC X.                                   009
              03 FILLER            PIC X(14).                               009
           02 NRO-SEC              PIC 9(6).                                011
           02 POS-138-139          PIC XX.                                  011
           02 POS-140              PIC X.                                   011
                                                                            009
       01  REG-AUX.                                                         003
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
           02 F                          PIC XXX.                           004
           02 H-HABER                    PIC S9(9)V99   COMP-3.              004
      **                                              * POS 20 - 25
           02 SUPL1                      PIC S9(9)V99   COMP-3.              005
      **                                              * POS 26 - 31
           02 SUPL2                      PIC S9(9)V99   COMP-3.              005
      **                                              * POS 32 - 37
           02 TABLA.                                                        005
              03 H-ANTIG   OCCURS 41 TIMES PIC S9(9)V99 COMP-3.             005
      **                                              * POS 38 - 283
           02 SUMAFIJ1                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 284- 289
           02 SUMAFIJ2                   PIC S9(9)V99 COMP-3.               005
      **                                              * POS 290- 295
           02 SUMAFIJ3                   PIC S9(7)V99 COMP-3.               005
      **                                              * POS 296- 300
       01  VEC-LIQ.
           02 REGISTRO  OCCURS 20.
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
                 05 A-POS-7-8.                                              009
                    06 A-POS-7   PIC X.                                     009
                    06 A-POS-8   PIC X.                                     009
                 05 A-POS-9-10.                                             009
                    06 A-POS-9   PIC X.                                     009
                    06 A-POS-10  PIC X.                                     009
                 05 A-POS-11-12  PIC XX.                                    009
                 05 A-POS-13-14.                                            009
                    06 A-POS-13  PIC X.                                     009
                    06 A-POS-14  PIC X.                                     009
                 05 A-POS-15-16.                                            009
                    06 A-POS-15  PIC X.                                     009
                    06 A-POS-16  PIC X.                                     009
              04    FILLER       PIC X(4).
  *******  03 A-MARDGI           PIC X.
           03 A-CODIGO2          PIC X(15).                                 009
           03 A-NRO-SEC          PIC 9(6).                                  011
           03 A-POS-138-139      PIC XX.                                    009
           03 A-POS-140          PIC X.                                     009
      *
       01  MASI.
           02 MARCA-MASI   PIC 9.

       PROCEDURE DIVISION  USING REG-PARAM REG-MAE REG-AUX VEC-LIQ MASI.    043
           COPY PDFECHA.CPY.
           MOVE SPACES TO VEC-LIQ MASI.
      * MASIVO POR GRUPO PENSIONARIO CON P ************
           IF SEC2 = 'P'
              THEN  MOVE 1 TO MARCA-MASI
                    GO TO CERRAR.
    ******   agregado 7-6-05   solo para adecuaciones en linea
            IF COD-RETRO = 'C'       
               THEN
              IF POS-3 NOT =  'B' THEN
              IF POS-9-10 NUMERIC AND (POS-11-12 NUMERIC    
                                  OR POS-11-12 = '  ')       
                   THEN MOVE POS-9-10  TO ANTIG1
                        IF POS-11-12 = '00'  OR '  '     
                            THEN      
                             MOVE 100   TO PORC
                             MOVE '00' TO POS-11-12   
                             GO TO FINVALID
                            ELSE
                             MOVE POS-11-12R TO PORC
                             GO TO FINVALID  
                   ELSE MOVE 1 TO MARCA-MASI
                        GO TO CERRAR 
                                  ELSE
                IF POS-11-12 NUMERIC OR POS-11-12 = '  '   
                   THEN IF POS-11-12 = '00'  OR '  '     
                           THEN      
                             MOVE 100   TO PORC
                             MOVE '00' TO POS-11-12   
                             GO TO FINVALID
                           ELSE
                             MOVE POS-11-12R TO PORC
                             GO TO FINVALID  
                   ELSE MOVE 1 TO MARCA-MASI
                        GO TO CERRAR.

           IF POS-3 NOT =  'B' THEN
              IF POS-9-10 NOT NUMERIC OR POS-11-12 NOT NUMERIC
                   THEN MOVE 1 TO MARCA-MASI
                        GO TO CERRAR 
                   ELSE 
                        MOVE POS-9-10  TO ANTIG1
                        MOVE POS-11-12R TO PORC 
                               ELSE
              IF POS-11-12 NOT NUMERIC
                   THEN MOVE 1 TO MARCA-MASI
                        GO TO CERRAR 
                   ELSE 
                        MOVE POS-11-12R TO PORC.

       FINVALID.
           IF POS-3 NOT =  'B' THEN
             IF ANTIG1 > 60
              THEN MOVE 1 TO MARCA-MASI
                   GO TO CERRAR.
   ********  DISPLAY 'sale ' NRO ' ' CODIGO  ' ' MARCA-MASI.
           MOVE 0 TO I.
           PERFORM CALCULA-ANTIGUEDAD THRU CALCULA-ANTIGUEDAD-X. 
      *
           PERFORM CALCULA-HABERES THRU CALCULA-HABERES-X.
      *
       CERRAR.
           GOBACK.

       CALCULA-ANTIGUEDAD.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I).
      ****   el codigo '02' FBB tiene antiguedad con valor cero ****
           IF POS-3 = 'B' THEN GO TO CALCULA-ANTIGUEDAD-X.
           MOVE ZEROS TO IMPOR1 IMPOR2 ANTIG-NEW.
      ****   CALCULO DE ANTIGUEDAD PARA TODOS *******

           IF ANTIG1 =  0
              THEN MOVE 0 TO ANTIG-AUX
                   GO TO  CALCULA-ANTIGUEDAD-X.
            IF FECHA-MES > '199601'
               THEN IF SI-LETRA-ANTIG  NOT = SPACE
                     THEN COMPUTE ANTIG1 = ANTIG1 - 1.                      
           MOVE ANTIG1 TO ANTIG2.
           IF ANTIG1 < 40
              THEN ADD 1 TO ANTIG1
                   COMPUTE ANTIG-AUX = H-ANTIG (ANTIG1)
              ELSE
                  COMPUTE ANTIG1 = ANTIG1 - 40 + 1
                  IF ANTIG1 > 40
                    THEN
      *****  COMO MAXIMO SE PAGAN 80 ANIOS DE ANTIGUEDAD *********
                    COMPUTE ANTIG-AUX = H-ANTIG (41) * 2
                    ELSE
                 COMPUTE ANTIG-AUX = H-ANTIG (41) + H-ANTIG (ANTIG1).
     *********
            COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
            MOVE ANTIG-AUX TO A-ANTIG (I).
       CALCULA-ANTIGUEDAD-X.
            EXIT.
      *
       CALCULA-HABERES.
 ******** calculo del haber
           IF FECHA-MES < '201509'                                                                 
              THEN MOVE 1 TO A-HABER(I) A-ANTIG(I)
                   GO TO CALCULA-HABERES-X.      
      ****   el codigo '02' FBB tiene haber con valor cero ****
           IF POS-3 = 'B' THEN                               
           COMPUTE A-HABER (I) = ZEROS                            
                          ELSE
           COMPUTE A-HABER (I) = H-HABER * PORC / 100.            
           PERFORM ESCALAS THRU ESCALAS-X.
  *******
           IF POS-3 = '3' OR '7'
              THEN PERFORM HAB-DIRECCION THRU HAB-DIR-X
              ELSE
           IF POS-3 = '0'
              THEN PERFORM HAB-JERARQUICO THRU HAB-JER-X
              ELSE
           IF POS-3 = '4'                              
              THEN PERFORM HAB-SENIALERO  THRU HAB-SEN-X
              ELSE
           IF POS-3 = '1' OR 'B'
              THEN PERFORM HAB-UFERROV THRU HAB-UFERRO-X
              ELSE
           IF POS-3 = '2'                               
              THEN PERFORM HAB-FRATERNID THRU HAB-FRAT-X
              ELSE
                   MOVE 5 TO MARCA-MASI
                   GO TO CERRAR.
       CALCULA-HABERES-X.   EXIT.
                  
       HAB-DIRECCION.                    
           PERFORM PERMANENCIA121 THRU PERMA-X.      
           PERFORM TITULOS THRU TITULO-X.      
           PERFORM DEDICAC83 THRU DEDICAC-X.      
           PERFORM RESPONS85 THRU RESPONS-X.      
           PERFORM RECOMPSAL THRU RECOMP-X.      
       HAB-DIR-X.     EXIT.

       HAB-JERARQUICO.                    
           PERFORM PERMANENCIA121 THRU PERMA-X.      
           PERFORM TITULOS THRU TITULO-X.      
           PERFORM DEDICAC83 THRU DEDICAC-X.      
           PERFORM RECOMPSAL THRU RECOMP-X.      
           PERFORM RESPONS85 THRU RESPONS-X.      
           PERFORM PERSSUP182 THRU PERSSUP-X.         
       HAB-JER-X.    EXIT.

       HAB-SENIALERO.                  
           PERFORM PERMANENCIA121 THRU PERMA-X.      
           PERFORM TITULOS THRU TITULO-X.      
           PERFORM BONVS178 THRU BONVS-X.      
           PERFORM CONCABINA104 THRU CCABI-X.      
           PERFORM SENIAL-INT116 THRU S-INT-X.      
           PERFORM CABINACAB075 THRU CABCA-X.      
           PERFORM ATENCBARR118 THRU ABARR-X.      
       HAB-SEN-X.
            EXIT.

       HAB-UFERROV.                    
           IF POS-3 = 'B' THEN GO TO VERCTO02.
           PERFORM PERMANENCIA121 THRU PERMA-X.      
           PERFORM TITULOS THRU TITULO-X.      
           PERFORM BONVS174 THRU BONVS174-X.      
************** faltan desarrollar los perf q estan abajo:
           PERFORM ELECT58  THRU ELECT58-X.      
           PERFORM VIADOB59 THRU VIADOB59-X.      
           PERFORM RETRIB60 THRU RETRIB60-X.      
           PERFORM PERSMOV62 THRU PERSMOV62-X.      
           PERFORM INCTALL64 THRU INCTALL64-X.      
           PERFORM CABGRUP70 THRU CABGRUP70-X.      
           PERFORM DESGHER71 THRU DESGHER71-X.      
           PERFORM VALORLO72 THRU VALORLO72-X.      
           PERFORM BSVSEXT73 THRU BSVSEXT73-X.      
           GO TO HAB-UFERRO-X.
       VERCTO02.        
           PERFORM BICICLE84 THRU BICICLE84-X.      
           PERFORM BSLOCOM98 THRU BSLOCOM98-X.      
           PERFORM BSCAMB100 THRU BSCAMB100-X.      
           PERFORM JEF-AU102 THRU JEF-AU102-X.      
           PERFORM VESTUA114 THRU VESTUA114-X.      
           PERFORM RAD-TE115 THRU RAD-TE115-X.      
           PERFORM INCEND168 THRU INCEND168-X.      
           PERFORM CONDZM171 THRU CONDZM171-X.      
           PERFORM PREMCI501 THRU PREMCI501-X.      
           PERFORM GDATRE502 THRU GDATRE502-X.      
           PERFORM INSPTR503 THRU INSPTR503-X.      
           PERFORM TRAFIN506 THRU TRAFIN506-X.      
       HAB-UFERRO-X.
             EXIT.
                 
       HAB-FRATERNID.                    
           PERFORM PERMANENCIA121 THRU PERMA-X.      
           PERFORM TITULOS THRU TITULO-X.      
           PERFORM RECOMPSAL THRU RECOMP-X.      
           PERFORM INSTRUC66 THRU INSTRU-X.      
           PERFORM BONVS73 THRU BONVS73-X.      
           PERFORM EXAMAPR103 THRU EXAAPR-X.      
           PERFORM TRAFINT169 THRU TRAFIN-X.      
           PERFORM CERTCON171 THRU CERTCO-X.      
           PERFORM BONMENS176 THRU BONMEN-X.      
       HAB-FRAT-X.
             EXIT.
                 
    **** Cargo los basicos de las escalas q se usan para calc titulos, bonvs174 y ....
       ESCALAS.         
            IF FECHA-MES > '201608'
              THEN COMPUTE ESC405 = 4906.19 
                   COMPUTE ESC111 = 4001.62
                   COMPUTE ESC118 = 5364.71
                   COMPUTE ESC125 = 7780.12 
                   COMPUTE ESC201 = 4489.60 
                   COMPUTE ESC223 = 7923.27 
              ELSE
            IF FECHA-MES > '201607'
              THEN COMPUTE ESC405 = 4906.19 
                   COMPUTE ESC111 = 4001.62
                   COMPUTE ESC118 = 5364.71
                   COMPUTE ESC125 = 7780.12 
                   COMPUTE ESC201 = 4461.23 
                   COMPUTE ESC223 = 7873.21 
              ELSE
            IF FECHA-MES > '201603'
              THEN COMPUTE ESC405 = 4550.09
                   COMPUTE ESC111 = 3711.18
                   COMPUTE ESC118 = 4975.34
                   COMPUTE ESC125 = 7215.43 
                   COMPUTE ESC201 = 4113.70 
                   COMPUTE ESC223 = 7259.87 
              ELSE
            IF FECHA-MES > '201601'
              THEN COMPUTE ESC405 = 4158.39
                   COMPUTE ESC111 = 3391.69
                   COMPUTE ESC118 = 4547.03
                   COMPUTE ESC125 = 6594.28 
                   COMPUTE ESC201 = 3727.15 
                   COMPUTE ESC223 = 6577.69 
              ELSE
            IF FECHA-MES > '201508'
              THEN COMPUTE ESC405 = 3956.60
                   COMPUTE ESC111 = 3227.11
                   COMPUTE ESC118 = 4326.38
                   COMPUTE ESC125 = 6274.29 
                   COMPUTE ESC201 = 3546.29 
                   COMPUTE ESC223 = 6258.51.
       ESCALAS-X.      EXIT.

     **** sin antiguedad
       PERMANENCIA121.      
            IF A-ANTIG (1) = 0 THEN GO TO PERMA-X.
            COMPUTE BONIFIC = A-ANTIG(1) / 3. 
            MOVE '*B.PERMAN.FB' TO LEYENDA.
            PERFORM CARGAR THRU CARGAR-X.
       PERMA-X.   EXIT.

     **** sin antiguedad
       TITULOS.           
           MOVE 0 TO TITU.
           IF POS-6 = '0'  THEN GO TO TITULO-X.                    
      ************* pers.de.dccion y jerarquicos:
           IF POS-3 = '3' OR '7' OR '0' THEN          
              IF POS-6 = '1' THEN
                 PERFORM 1UNIV1A THRU 1UNIV1A-X
                             ELSE                         
              IF POS-6 = '2' THEN
                 PERFORM 2UNIV4A THRU 2UNIV4A-X
                             ELSE
              IF POS-6 = '3' THEN                           
                 PERFORM 3UNIV5A THRU 3UNIV5A-X
                             ELSE
              IF POS-6 = '4' THEN                           
                 PERFORM 4INGEN THRU 4INGEN-X
                             ELSE
              IF POS-6 = '5' THEN                           
                 PERFORM 5MEDICO THRU 5MEDIC-X
                             ELSE
              IF POS-6 = '6' THEN                           
                 PERFORM 6PROFEN THRU 6PROFE-X
                             ELSE
              IF POS-6 = '7' THEN                           
                 PERFORM 7PROFSIN THRU 7PROFS-X
                             ELSE GO TO TITULO-X 
                                       ELSE
      ************* senialeros y union ferroviaria:
           IF POS-3 = '4' OR '1' THEN          
              IF POS-6 = '1' THEN
                 PERFORM 1UNIV1A THRU 1UNIV1A-X
                             ELSE                         
              IF POS-6 = '2' THEN
                 PERFORM 2UNIV4A THRU 2UNIV4A-X
                             ELSE
              IF POS-6 = '3' THEN                           
                 PERFORM 3UNIV5A THRU 3UNIV5A-X
                             ELSE
              IF POS-6 = '8' THEN                           
                 PERFORM 8SECUN  THRU 8SECUN-X
                                 ELSE GO TO TITULO-X 
                                       ELSE
      ************* la fraternidad:
           IF POS-3 = '2' THEN          
              IF POS-6 = '1' THEN
                 PERFORM 1UNIV1A THRU 1UNIV1A-X
                             ELSE                         
              IF POS-6 = '2' THEN
                 PERFORM 2UNIV4A THRU 2UNIV4A-X
                             ELSE
              IF POS-6 = '3' THEN                           
                 PERFORM 3UNIV5A THRU 3UNIV5A-X
                             ELSE
              IF POS-6 = '6' THEN                           
                 PERFORM 6PROFEN THRU 6PROFE-X
                             ELSE
              IF POS-6 = '8' THEN                           
                 PERFORM 8SECUN THRU 8SECUN-X
                             ELSE GO TO TITULO-X 
                          ELSE GO TO TITULO-X. 

           IF TITU > 0                  
                      THEN
              COMPUTE BONIFIC =  TITU      
              MOVE '*BON.TIT.FB ' TO LEYENDA 
              PERFORM CARGAR THRU CARGAR-X.
       TITULO-X.    EXIT.
                 

       1UNIV1A.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 8 / 100 * PORC / 100
            ELSE
           IF POS-3 = '4'        
            THEN COMPUTE TITU = ESC405 * 8 / 100 * PORC / 100
            ELSE
           IF POS-3 = '1'        
            THEN COMPUTE TITU = ESC111 * 8 / 100 * PORC / 100
            ELSE
           IF POS-3 = '2'        
            THEN COMPUTE TITU = ESC201 * 8 / 100 * PORC / 100.
       1UNIV1A-X.      EXIT.

       2UNIV4A.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 10 / 100 * PORC / 100
            ELSE
           IF POS-3 = '4'        
            THEN COMPUTE TITU = ESC405 * 10 / 100 * PORC / 100
            ELSE
           IF POS-3 = '1'        
            THEN COMPUTE TITU = ESC111 * 10 / 100 * PORC / 100
            ELSE
           IF POS-3 = '2'        
            THEN COMPUTE TITU = ESC201 * 10 / 100 * PORC / 100.
       2UNIV4A-X.      EXIT.

       3UNIV5A.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 12 / 100 * PORC / 100
            ELSE
           IF POS-3 = '4'        
            THEN COMPUTE TITU = ESC405 * 12 / 100 * PORC / 100
            ELSE
           IF POS-3 = '1'        
            THEN COMPUTE TITU = ESC111 * 12 / 100 * PORC / 100
            ELSE
           IF POS-3 = '2'        
            THEN COMPUTE TITU = ESC201 * 12 / 100 * PORC / 100.
       3UNIV5A-X.      EXIT.

       4INGEN.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 15 / 100 * PORC / 100.
       4INGEN-X.     EXIT.

       5MEDICO.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 20 / 100 * PORC / 100.
       5MEDIC-X.      EXIT.

       6PROFEN.                
           IF POS-3 = '7' OR '3' OR '0' OR '2'   
            THEN COMPUTE TITU = H-HABER * 25 / 100 * PORC / 100.
       6PROFE-X.       EXIT.

       7PROFSIN.                 
           IF POS-3 = '7' OR '3' OR '0'
            THEN COMPUTE TITU = H-HABER * 20 / 100 * PORC / 100.
       7PROFS-X.       EXIT.

       8SECUN.                
           IF POS-3 = '0'
            THEN COMPUTE TITU = H-HABER * 7 / 100 * PORC / 100 
            ELSE
           IF POS-3 = '4'        
            THEN COMPUTE TITU = ESC405 * 7 / 100 * PORC / 100
            ELSE
           IF POS-3 = '1'        
            THEN COMPUTE TITU = ESC111 * 7 / 100 * PORC / 100
            ELSE
           IF POS-3 = '2'        
            THEN COMPUTE TITU = ESC201 * 7 / 100 * PORC / 100.
       8SECUN-X.      EXIT.

       DEDICAC83.                     
           IF POS-7 = 'D' THEN NEXT SENTENCE
                          ELSE GO TO DEDICAC-X.
           COMPUTE BONIFIC = H-HABER * 10 / 100 * PORC / 100.
           MOVE '*BON.DED.FB ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       DEDICAC-X.      EXIT.      

       RESPONS85.                     
           IF SUMAFIJ2 > ZEROS THEN NEXT SENTENCE
                               ELSE GO TO RESPONS-X.
           COMPUTE BONIFIC = SUMAFIJ2 * PORC / 100.
           MOVE '*BON.RESP.FB ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       RESPONS-X.      EXIT.     

       RECOMPSAL.                    
           IF SUPL2 > ZEROS THEN NEXT SENTENCE
                            ELSE GO TO RECOMP-X.
           COMPUTE BONIFIC = SUPL2 * PORC / 100.
           MOVE '*BON.RECOM.FB ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       RECOMP-X.       EXIT.      

       PERSSUP182.                        
           COMPUTE BONIFIC = H-HABER * 1.19 * PORC / 100.
           MOVE '*BON.PERSS.FB ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       PERSSUP-X.       EXIT.        

       BONVS178.                   
           COMPUTE BONIFIC = H-HABER * 0.62 * PORC / 100.
           MOVE '*BON.BONVS.FB ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONVS-X.     EXIT.

       CONCABINA104.                   
           IF POS-3-4-5 = '424' THEN NEXT SENTENCE  
                                ELSE GO TO CCABI-X.
           COMPUTE BONIFIC = H-HABER * 0.10 * PORC / 100.
           MOVE '*BON.CCABINA.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       CCABI-X.     EXIT.     

     ** seniales luminosas o interruptor electrico.
       SENIAL-INT116.                   
           IF POS-7 = 'L' THEN NEXT SENTENCE  
                          ELSE GO TO S-INT-X.
           COMPUTE BONIFIC = H-HABER * 0.10 * PORC / 100.
           MOVE '*BON.CABINALM.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       S-INT-X.     EXIT.

       CABINACAB075.                   
           IF POS-7 = 'C' THEN NEXT SENTENCE  
                          ELSE GO TO CABCA-X.
           COMPUTE BONIFIC = H-HABER * 0.07 * PORC / 100.
           MOVE '*BON.CABINACA.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       CABCA-X.     EXIT.      

       ATENCBARR118.                   
           IF POS-7 = 'B' THEN NEXT SENTENCE  
                          ELSE GO TO ABARR-X.
           COMPUTE BONIFIC = H-HABER * 0.21 * PORC / 100.
           MOVE '*BON.CABINABA.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       ABARR-X.     EXIT.

       BONVS174.                   
           COMPUTE BONIFIC = (H-HABER * 0.45 + ESC125 * 0.2)
                                                * PORC / 100.
           MOVE '*BON.BONVS1.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONVS174-X.     EXIT.

       INSTRUC66.                    
           IF POS-7 = 'I' THEN NEXT SENTENCE  
                          ELSE GO TO INSTRU-X.
           COMPUTE BONIFIC = ESC223 * 0.05 * PORC / 100.
           MOVE '*BON.INSTRUCT.FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       INSTRU-X.     EXIT.

       BONVS73.                   
           IF POS-8 = 'B' THEN NEXT SENTENCE  
                          ELSE GO TO BONVS73-X.
           COMPUTE BONIFIC = ESC223 * 0.2 * PORC / 100.
           MOVE '*BON.VS73    FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONVS73-X.      EXIT.

       EXAMAPR103.                    
           IF POS-13 = 'E' THEN NEXT SENTENCE  
                           ELSE GO TO EXAAPR-X.
           IF POS-3-4-5 = '208' OR '210' OR '213'            
                        THEN NEXT SENTENCE  
                        ELSE GO TO EXAAPR-X.
           COMPUTE BONIFIC = H-HABER * 0.05 * PORC / 100.
           MOVE '*BON.EXAM.AP FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       EXAAPR-X.     EXIT.

       TRAFINT169.                    
           IF POS-14 = 'T' OR 'X' OR 'W' THEN NEXT SENTENCE  
                                         ELSE GO TO TRAFIN-X.
           IF POS-14 = 'T' OR 'X' THEN                                  
             COMPUTE BONIFIC = H-HABER * 0.225 * PORC / 100 
                                  ELSE
             COMPUTE BONIFIC = H-HABER * 0.125 * PORC / 100.
           MOVE '*BON.TRAF.IN FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       TRAFIN-X.     EXIT.

       CERTCON171.                    
           IF POS-15 = 'C'  THEN NEXT SENTENCE  
                            ELSE GO TO CERTCO-X.
           IF POS-3-4-5 = '223' OR '217' THEN NEXT SENTENCE  
                                         ELSE GO TO CERTCO-X.
           COMPUTE BONIFIC = H-HABER * 0.15 * PORC / 100.
           MOVE '*BON.CERTF.C FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       CERTCO-X.     EXIT.

       BONMENS176.                    
           IF POS-3-4-5 = '201' THEN GO TO BONMEN-X.
           IF POS-3-4-5 = '223' OR '217' THEN                 
             COMPUTE BONIFIC = H-HABER * 0.8 * PORC / 100 
                                         ELSE
           IF POS-3-4-5 = '213' THEN                 
             COMPUTE BONIFIC = H-HABER * 0.6 * PORC / 100 
                                ELSE
           IF POS-3-4-5 = '210' OR '208' THEN                 
             COMPUTE BONIFIC = H-HABER * 0.55 * PORC / 100 
                                         ELSE MOVE 0 TO BONIFIC.
           MOVE '*BON.MENSUAL FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONMEN-X.     EXIT.

************** faltan desarrollar los perf q estan abajo:
       ELECT58.                      
           IF POS-7 = 'E' THEN
             COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100 
             MOVE '*BON.ELECTRO FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       ELECT58-X.    EXIT.

       VIADOB59.                      
           IF POS-8 = 'V' THEN
             COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100 
             MOVE '*BON.VIADOBL FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       VIADOB59-X.   EXIT.

       RETRIB60.                      
           IF POS-13 = 'R' THEN
             COMPUTE BONIFIC = H-HABER * 0.5 * PORC / 100 
             MOVE '*BON.TRAB.ES FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       RETRIB60-X.   EXIT.

       PERSMOV62.                       
           IF POS-14 = '1' OR '2' OR '3'
              THEN NEXT SENTENCE
              ELSE GO TO PERSMOV62-X.
           IF POS-14 = '1' THEN                 
           COMPUTE BONIFIC = H-HABER * 0.03 * PORC / 100 
                           ELSE
           IF POS-14 = '2' THEN                 
           COMPUTE BONIFIC = H-HABER * 0.04 * PORC / 100 
                           ELSE
           COMPUTE BONIFIC = H-HABER * 0.12 * PORC / 100.
           MOVE '*BON.PERS.MO FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       PERSMOV62-X.     EXIT.

       INCTALL64.                       
           IF POS-15-16 NUMERIC AND POS-15-16N > 0
             THEN
              COMPUTE BONIFIC = H-HABER * POS-15-16N         
                                      / 100 * PORC / 100 
              MOVE '*BON.INC TA  FB' TO LEYENDA 
              PERFORM CARGAR THRU CARGAR-X.
       INCTALL64-X.     EXIT.

       CABGRUP70.                       
           IF POS-17 = 'C' THEN
             COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100 
             MOVE '*BON.CAB GRU FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       CABGRUP70-X.     EXIT.

       DESGHER71.                       
           IF POS-18 = 'H' THEN
             COMPUTE BONIFIC = H-HABER * 0.05 * PORC / 100 
             MOVE '*BON.DES HERRFB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       DESGHER71-X.     EXIT.

       VALORLO72.                       
           IF POS-19 = 'L' THEN
             COMPUTE BONIFIC = H-HABER * 0.3 * PORC / 100 
             MOVE '*BON.VAL LOC FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       VALORLO72-X.     EXIT.

       BSVSEXT73.                       
           IF POS-20 = '1' OR '2' THEN NEXT SENTENCE
                                  ELSE GO TO BSVSEXT73-X.
           IF POS-20 = '1' THEN                   
             COMPUTE BONIFIC = H-HABER * 0.3 * PORC / 100 
                           ELSE        
             COMPUTE BONIFIC = H-HABER * 0.15 * PORC / 100.
           MOVE '*BON.VS EXTR FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BSVSEXT73-X.     EXIT.

    ************* figura en codigo 02
       BICICLE84.                       
           IF POS-6  = 'B' THEN               
             COMPUTE BONIFIC = ESC111 * 0.05 * PORC / 100 
             MOVE '*BON.BICICL  FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       BICICLE84-X.     EXIT.

       BSLOCOM98.                       
           IF POS-7  = 'L' THEN               
             COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100 
             MOVE '*BON.LOCOMOT FB' TO LEYENDA 
             PERFORM CARGAR THRU CARGAR-X.
       BSLOCOM98-X.     EXIT.

       BSCAMB100.                       
           IF POS-8 = '1' OR '2' OR '3' OR '4' OR '5' OR '6' OR '7'
               THEN NEXT SENTENCE
               ELSE GO TO BSCAMB100-X.
           IF POS-8 = '1' THEN COMPUTE BONIFIC = H-HABER * 0.075             
                                                   * PORC / 100
                          ELSE
           IF POS-8 = '2' THEN COMPUTE BONIFIC = H-HABER * 0.08              
                                                   * PORC / 100
                          ELSE
           IF POS-8 = '3' THEN COMPUTE BONIFIC = H-HABER * 0.15              
                                                   * PORC / 100
                          ELSE
           IF POS-8 = '4' THEN COMPUTE BONIFIC = H-HABER * 0.17              
                                                   * PORC / 100
                          ELSE
           IF POS-8 = '5' THEN COMPUTE BONIFIC = H-HABER * 0.18              
                                                   * PORC / 100
                          ELSE
           IF POS-8 = '6' THEN COMPUTE BONIFIC = H-HABER * 0.19              
                                                   * PORC / 100
                          ELSE COMPUTE BONIFIC = H-HABER * 0.2               
                                                   * PORC / 100.
           MOVE '*BON.CAMBIST FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BSCAMB100-X.     EXIT.

       JEF-AU102.                       
           IF POS-9 = '1' OR '2'                                      
               THEN NEXT SENTENCE
               ELSE GO TO JEF-AU102-X.
           IF POS-9 = '1' THEN COMPUTE BONIFIC = H-HABER * 0.25      
                                                   * PORC / 100
                          ELSE COMPUTE BONIFIC = H-HABER * 0.2      
                                                   * PORC / 100.
           MOVE '*BON.JEF AUX FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       JEF-AU102-X.     EXIT.

       VESTUA114.                       
           IF POS-10 = 'V' THEN COMPUTE BONIFIC = H-HABER * 0.1        
                                                   * PORC / 100 
                                MOVE '*BON.VESTUAR FB' TO LEYENDA 
                                PERFORM CARGAR THRU CARGAR-X.
       VESTUA114-X.     EXIT.

       RAD-TE115.                       
           IF POS-13 = 'R' THEN COMPUTE BONIFIC = H-HABER * 0.15       
                                                   * PORC / 100 
                                MOVE '*BON.RAD TEL FB' TO LEYENDA 
                                PERFORM CARGAR THRU CARGAR-X.
       RAD-TE115-X.     EXIT.

       INCEND168.                       
           IF POS-14 = '1' OR '2' OR '3'                                     
               THEN NEXT SENTENCE
               ELSE GO TO INCEND168-X.
           IF POS-14 = '1' THEN COMPUTE BONIFIC = ESC118 * 0.1       
                                                   * PORC / 100
                           ELSE
           IF POS-14 = '2' THEN COMPUTE BONIFIC = ESC118 * 0.15       
                                                   * PORC / 100
                           ELSE
           IF POS-14 = '3' THEN COMPUTE BONIFIC = ESC118 * 0.2        
                                                   * PORC / 100.
           MOVE '*BON.SER INC FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       INCEND168-X.     EXIT.

       CONDZM171.                       
           IF POS-15 = 'C' THEN COMPUTE BONIFIC = H-HABER * 0.05       
                                                   * PORC / 100 
                                MOVE '*BON.CONDUCC FB' TO LEYENDA 
                                PERFORM CARGAR THRU CARGAR-X.
       CONDZM171-X.     EXIT.

       PREMCI501.                       
           IF POS-16-17 NUMERIC AND POS-16-17N > 0
             THEN
              COMPUTE BONIFIC = H-HABER * POS-16-17N         
                                      / 100 * PORC / 100 
              MOVE '*BON.PREM IN FB' TO LEYENDA 
              PERFORM CARGAR THRU CARGAR-X.
       PREMCI501-X.     EXIT.

       GDATRE502.                       
           IF POS-18 = '1' OR '2' OR '3' OR '4' OR '5' OR '6' OR '7' OR      
                       '8' OR '9' OR 'A' OR 'B' OR 'C' OR 'D'                
               THEN NEXT SENTENCE
               ELSE GO TO GDATRE502-X.
           IF POS-18 = '1' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.08 * PORC / 100  
                           ELSE
           IF POS-18 = '2' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.09 * PORC / 100  
                           ELSE
           IF POS-18 = '3' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.1  * PORC / 100  
                           ELSE
           IF POS-18 = '4' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.11 * PORC / 100  
                           ELSE
           IF POS-18 = '5' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.12 * PORC / 100  
                           ELSE
           IF POS-18 = '6' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.13 * PORC / 100  
                           ELSE
           IF POS-18 = '7' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.14 * PORC / 100  
                           ELSE
           IF POS-18 = '8' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.15 * PORC / 100  
                           ELSE
           IF POS-18 = '9' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.16 * PORC / 100  
                           ELSE
           IF POS-18 = 'A' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.17 * PORC / 100  
                           ELSE
           IF POS-18 = 'B' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.18 * PORC / 100  
                           ELSE
           IF POS-18 = 'C' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.19 * PORC / 100  
                           ELSE
              COMPUTE BONIFIC = H-HABER * 0.2  * PORC / 100. 
           MOVE '*BON.GDATREN FB' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       GDATRE502-X.     EXIT.

       INSPTR503.                       
           IF POS-19 = 'I' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100  
              MOVE '*BON.INSPECT FB' TO LEYENDA 
              PERFORM CARGAR THRU CARGAR-X.
       INSPTR503-X.     EXIT.

       TRAFIN506.                       
           IF POS-20 = 'T' THEN                                                                                    
              COMPUTE BONIFIC = H-HABER * 0.1 * PORC / 100  
              MOVE '*BON.TRAFIC  FB' TO LEYENDA 
              PERFORM CARGAR THRU CARGAR-X.
       TRAFIN506-X.     EXIT.

       CARGAR.
           IF BONIFIC  > ZEROS OR                      
                 ANTIGBON > ZEROS THEN NEXT SENTENCE    
                                  ELSE GO TO CARGAR-X.    
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS TO  A-IOMA (I).
           MOVE ANTIGBON TO A-ANTIG (I).
           MOVE BONIFIC TO A-HABER (I).                  
           MOVE '02' TO A-CONC (I).
           MOVE LEYENDA TO A-CODIGO (I).
           MOVE ZEROS TO BONIFIC.
           MOVE ZEROS TO ANTIGBON.
           MOVE SPACES TO LEYENDA. 
       CARGAR-X.     EXIT.
                    

