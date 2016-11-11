       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. GMAEDEFPA.                                               000
       AUTHOR. 'MONICA 13-10-15'                                            000
      *REMARKS.'CALCULA HABER Y BONIFICACIONES DE la DEFENSORIA del PUEBLO'.000
      *REMARKS.' haberes desde marzo 2010, codigos 'FD''.                   000
       ENVIRONMENT DIVISION.                                                000
       DATA DIVISION.                                                       000
       WORKING-STORAGE SECTION.                                             001
           COPY WSFECHA.CPY.
       77  I                 PIC 99 VALUE ZEROS.
       77  INDX              PIC 99 VALUE ZEROS.
       77  I-LETRA           PIC 99 VALUE ZEROS.
       77  J                 PIC 99 VALUE ZEROS.
       77  P                 PIC 99 VALUE ZEROS.
       77  JJB               PIC 99 VALUE ZEROS.
       77  K                 PIC 99 VALUE ZEROS.
       77  PORC              PIC 999 VALUE ZEROS.
       77  ANTIG1            PIC 99 VALUE ZEROS.
       77  MARCA1            PIC 9  VALUE ZEROS.
       77  MARCA2            PIC 9  VALUE ZEROS.
       77  ANTIG1-N          PIC S99 VALUE ZEROS.
       77  LEYENDA           PIC X(16).
       77  BONIFIC           PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR1            PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR2            PIC S9(7)V99 VALUE ZEROS.
       77  BON-20            PIC AA VALUE 'NO'.
       77  ANTIG-AUX         PIC S9(10)V99 VALUE ZEROS.
       77  ANTIG-NEW         PIC S9(10)V99 VALUE ZEROS.
       77  ANTIGBON          PIC S9(7)V99 VALUE ZEROS.
       77  PORCANT           PIC 99V999999 VALUE ZEROS.
      ***
      *
       01  HORAPROC.
           02 H-PROC       PIC XX.
           02 M-PROC       PIC XX.
           02 S-PROC       PIC XX.
           02 S-PROC1      PIC XXX.

      ***TABLA DE LETRAS DE ANTIGUEDAD DESDE PPTO. 9601
       01  TABLA-LETRAS-1ER.
           02  FILLER  PIC X(10) VALUE '01 A 00000'.                      
    *****                                              1996
           02  FILLER  PIC X(10) VALUE '02 B 00100'.                      
    *****                                              1997
           02  FILLER  PIC X(10) VALUE '03 C 00100'.                      
    *****                                              1998
           02  FILLER  PIC X(10) VALUE '04 D 00100'.                      
    *****                                              1999
           02  FILLER  PIC X(10) VALUE '05 E 00100'.                      
    *****                                              2000
           02  FILLER  PIC X(10) VALUE '06 F 00100'.                      
    *****                            1er semenstre     2001
           02  FILLER  PIC X(10) VALUE '07 H 00100'.                      
    *****                            1er semestre      2002
           02  FILLER  PIC X(10) VALUE '08 J 00100'.                      
    *****                            1er semestre      2003
           02  FILLER  PIC X(10) VALUE '09 L 00100'.                      
    *****                            1er semestre      2004
           02  FILLER  PIC X(10) VALUE '10 M 00200'.                      
    *****                                              2005
           02  FILLER  PIC X(10) VALUE '11 N 00300'.                      
    *****                                              2006
           02  FILLER  PIC X(10) VALUE '12 O 00300'.                      
    *****                                              2007
           02  FILLER  PIC X(10) VALUE '13 P 00300'.                      
    *****                                              2008
           02  FILLER  PIC X(10) VALUE '14 Q 00300'.                      
    *****                                              2009
           02  FILLER  PIC X(10) VALUE '15 R 00300'.                      
    *****                                              2010
           02  FILLER  PIC X(10) VALUE '16 S 00300'.                      
    *****                                              2011
           02  FILLER  PIC X(10) VALUE '17 T 00300'.                      
    *****                                              2012
           02  FILLER  PIC X(10) VALUE '18 U 00300'.                      
    *****                                              2013
           02  FILLER  PIC X(10) VALUE '19 V 00300'.                      
    *****                                              2014
           02  FILLER  PIC X(10) VALUE '20 W 00300'.                      
    *****                                              2015
           02  FILLER  PIC X(10) VALUE '21 X 00300'.                      
    *****                                              2016
           02  FILLER  PIC X(10) VALUE '22 Y 00000'.                      
           02  FILLER  PIC X(10) VALUE '23 Z 00000'.                      
       01  T-LETRAS-1ER REDEFINES TABLA-LETRAS-1ER.
           02  LETRAX1 OCCURS 23.       
               03  ANIOSX1      PIC 99.
               03  F            PIC X.
               03  LETRA1       PIC X.
               03  F            PIC X.
               03  LETRA-PORC1  PIC 9(3)V99.
      *
       01  TABLA-LETRAS-2DO.
           02  FILLER  PIC X(10) VALUE '01 A 00000'.                      
    *****                                              1996
           02  FILLER  PIC X(10) VALUE '02 B 00100'.                      
    *****                                              1997
           02  FILLER  PIC X(10) VALUE '03 C 00100'.                      
    *****                                              1998
           02  FILLER  PIC X(10) VALUE '04 D 00100'.                      
    *****                                              1999
           02  FILLER  PIC X(10) VALUE '05 E 00100'.                      
    *****                                              2000
           02  FILLER  PIC X(10) VALUE '06 G 00100'.                      
    *****                            2er semestre      2001
           02  FILLER  PIC X(10) VALUE '07 I 00100'.                      
    *****                            2er semestre      2002
           02  FILLER  PIC X(10) VALUE '08 K 00100'.                      
    *****                            2er semestre      2003
           02  FILLER  PIC X(10) VALUE '09 L 00100'.                      
    *****                            2er semestre      2004
           02  FILLER  PIC X(10) VALUE '10 M 00200'.                      
    *****                                              2005
           02  FILLER  PIC X(10) VALUE '11 N 00300'.                      
    *****                                              2006
           02  FILLER  PIC X(10) VALUE '12 O 00300'.                      
    *****                                              2007
           02  FILLER  PIC X(10) VALUE '13 P 00300'.                      
    *****                                              2008
           02  FILLER  PIC X(10) VALUE '14 Q 00300'.                      
    *****                                              2009
           02  FILLER  PIC X(10) VALUE '15 R 00300'.                      
    *****                                              2010
           02  FILLER  PIC X(10) VALUE '16 S 00300'.                      
    *****                                              2011
           02  FILLER  PIC X(10) VALUE '17 T 00300'.                      
    *****                                              2011
           02  FILLER  PIC X(10) VALUE '18 U 00000'.                      
           02  FILLER  PIC X(10) VALUE '19 V 00000'.                      
           02  FILLER  PIC X(10) VALUE '20 W 00000'.                      
           02  FILLER  PIC X(10) VALUE '21 X 00000'.                      
           02  FILLER  PIC X(10) VALUE '22 Y 00000'.                      
           02  FILLER  PIC X(10) VALUE '23 Z 00000'.                      
       01  T-LETRAS-2DO REDEFINES TABLA-LETRAS-2DO.
           02  LETRAX2 OCCURS 23.       
               03  ANIOSX2      PIC 99.
               03  F            PIC X.
               03  LETRA2       PIC X.
               03  F            PIC X.
               03  LETRA-PORC2  PIC 9(3)V99.
      ***

     ***********************************************                                   

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
              03 POS-N-17-20A REDEFINES POS-17-20-A        
                               PIC 99V99.

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
                    06 POS-1-2.                                             009
                      07 POS-1     PIC X.                                   009
                      07 POS-2     PIC X.                                   009
                    06 POS-3-4-5.                                           009
                      07 POS-3-4   PIC XX.                                  009
                      07 POS-5     PIC X.                                   009
                  05 R-POS-1-5 REDEFINES POS-1-5.                           009
                    06 FILLER      PIC X.
                    06 POS-2-3     PIC XX.
                    06 FILLER      PIC XX.
                  05 R1-POS-1-5 REDEFINES POS-1-5.                          009
                    06 POS-1-3     PIC XXX.
                    06 FILLER      PIC XX.
                  05 POS-6         PIC X.                                   009
                 04 POS-7-8        PIC XX.                                  009
                 04 POSN-7-8 REDEFINES POS-7-8 PIC 99.                          
                 04 POSN-7-8R REDEFINES POS-7-8.                                 
                  05 POS-7         PIC X.                                   009
                  05 POS-8         PIC X.                                   009
                 04 POS-9-10       PIC XX.                                  009
                 04 POS-11-12.                                              009
                   05 POS-11       PIC X.                                   009
                   05 POS-12       PIC X.                                   009
                 04 POS-11-12R REDEFINES POS-11-12 PIC 99.                  009
                 04 POS-13-14.                                              009
                    05 POS-13      PIC X.                                   009
                    05 POS-14      PIC X.                                   009
                 04 POS-N-13-14 REDEFINES POS-13-14 PIC 99.                 009
                 04 POS-15-16.                                              009
                    05 POS-15      PIC X.                                   009
                    05 POS-16      PIC X.                                   009
               03 POS-17-20.                            
                  04 POS-17        PIC X.
                  04 POS-18-19     PIC X(2).
                  04 MARDGI        PIC X.
               03 POS-N-17-20 REDEFINES POS-17-20        
                                     PIC 99V99.
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
                 04 H-CATE               PIC 99.                            004
              03 H-JORN                  PIC 99.                            004
              03 H-RAMA                  PIC X.                             004
              03 FILLER                  PIC X(4).                          004
              03 BONIF                   PIC 99.                            004
              03 BONIF1 REDEFINES BONIF.                                    004
                 04 LET1                 PIC X.                             004
                 04 LET2                 PIC X.                             004
           02 F                          PIC XXX.                           004
           02 H-HABER                    PIC S9(9)V99   COMP-3.             004
      **                                              * POS 20 - 25
           02 SUPL1                      PIC S9(9)V99   COMP-3.             005
      **                                              * POS 26 - 31
           02 SUPL2                      PIC S9(9)V99   COMP-3.             005
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
              04    FILLER       PIC X(3).
           03 A-MARDGI           PIC X.
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
    ******      solo para adecuaciones en linea
            IF COD-RETRO = 'C'       
               THEN
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
                         GO TO CERRAR.

              IF POS-9-10 NOT NUMERIC OR POS-11-12 NOT NUMERIC
                   THEN MOVE 1 TO MARCA-MASI
                        GO TO CERRAR
                   ELSE MOVE POS-9-10  TO ANTIG1 
                        MOVE POS-11-12R TO PORC.  

       FINVALID.
           IF ANTIG1 > 60
              THEN MOVE 1 TO MARCA-MASI
                   GO TO CERRAR.
     ******  DISPLAY 'sale ' NRO ' ' CODIGO  ' ' MARCA-MASI.
           MOVE 0 TO I.
           MOVE 0 TO BONIFIC.
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
           MOVE ZEROS TO IMPOR1 IMPOR2 ANTIG-NEW.
      ****   CALCULO DE ANTIGUEDAD PARA TODOS *******

           IF ANTIG1 =  0
              THEN MOVE 0 TO ANTIG-AUX
                   GO TO  CALCULA-ANTIGUEDAD-X.
            IF FECHA-MES > '199601'
               THEN IF SI-LETRA-ANTIG  NOT = SPACE
                     THEN PERFORM SEPARA-ANTIG THRU FIN-SEPARA-ANTIG.

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

            IF FECHA-MES > '199601'
              THEN IF SI-LETRA-ANTIG NOT = SPACE       
                    THEN
    *******         MOVE ANTIG-AUX TO ANTIG-NEW
                    PERFORM RECALCULA-ANTIG THRU FIN-RECALCULA-ANTIG. 

            COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
            MOVE ANTIG-AUX TO A-ANTIG (I).
            COMPUTE PORCANT = ANTIG-AUX / (SUMAFIJ3 * PORC / 100).
            DISPLAY 'PORCANT    ' PORCANT.
       CALCULA-ANTIGUEDAD-X.
            EXIT.
      *
       SEPARA-ANTIG.
            MOVE 0 TO MARCA1 MARCA2.
            PERFORM BUSCA-IND-LETRA-1 THRU FIN-BUSCA-IND-LETRA-1
             VARYING J FROM 1 BY 1 UNTIL J > 23.
            IF MARCA1 = 0 THEN
            PERFORM BUSCA-IND-LETRA-2 THRU FIN-BUSCA-IND-LETRA-2
             VARYING J FROM 1 BY 1 UNTIL J > 23.
            IF J > 30
               THEN MOVE ANTIG1 TO ANTIG1-N
                    COMPUTE ANTIG1-N = ANTIG1-N - I-LETRA
                    IF ANTIG1-N NOT > ZEROS
                       THEN DISPLAY '** ANTIGUEDAD MENOR O IGUAL A 0 '
                            A-NRO (1) ' ' A-APYN (1) ' ANTIG ' ANTIG1
                            ' LETRA ' SI-LETRA-ANTIG
                            COMPUTE JJB = I-LETRA - ANTIG1 + 1
                            MOVE 0 TO ANTIG1
                       ELSE MOVE ANTIG1-N TO ANTIG1
                            MOVE 1 TO JJB.
       FIN-SEPARA-ANTIG.        
            EXIT.
      *
       BUSCA-IND-LETRA-1.
            IF SI-LETRA-ANTIG = LETRA1 (J)
               THEN MOVE ANIOSX1 (J)  TO I-LETRA
                    MOVE J TO INDX
                    MOVE 1 TO MARCA1
                    MOVE 90 TO J.
       FIN-BUSCA-IND-LETRA-1. EXIT.
      *
       BUSCA-IND-LETRA-2.
            IF SI-LETRA-ANTIG = LETRA2 (J)
               THEN MOVE ANIOSX2 (J)  TO I-LETRA
                    MOVE J TO INDX
                    MOVE 1 TO MARCA2
                    MOVE 90 TO J.
       FIN-BUSCA-IND-LETRA-2. EXIT.

       RECALCULA-ANTIG.
            IF MARCA1 = 1 THEN
            PERFORM SUMA-PORCENTAJES-1 THRU F-SUMA-PORCENTAJES-1
                    VARYING K FROM JJB BY 1 UNTIL K > INDX 
                          ELSE
            PERFORM SUMA-PORCENTAJES-2 THRU F-SUMA-PORCENTAJES-2
                    VARYING K FROM JJB BY 1 UNTIL K > INDX.
       FIN-RECALCULA-ANTIG. EXIT.

       SUMA-PORCENTAJES-1.
           COMPUTE ANTIG-AUX = ANTIG-AUX +
                (SUMAFIJ3 * LETRA-PORC1 (K) / 100).
       F-SUMA-PORCENTAJES-1. EXIT.

       SUMA-PORCENTAJES-2.
           COMPUTE ANTIG-AUX = ANTIG-AUX +
                 (SUMAFIJ3 * LETRA-PORC2 (K) / 100).
       F-SUMA-PORCENTAJES-2. EXIT.

       CALCULA-HABERES.
           COMPUTE A-HABER (I) = H-HABER * PORC / 100.              
    ********* bonificacion ********************************************cobran antig sobre los gastos.
           PERFORM GTOS-REP    THRU GTOS-REP-X.
       CALCULA-HABERES-X.
            EXIT.


       GTOS-REP.                        
           IF FECHA-MES < '201204' THEN GO TO GTOS-REP-X.
           IF POS-5 NOT = 'J' THEN GO TO GTOS-REP-X.                     
           IF POS-3-4 = '50' THEN GO TO GTOS-REP-X.
           IF FECHA-MES > '201508'                  
              THEN COMPUTE BONIFIC = A-HABER (1) * 0.7 
              ELSE                                 
           IF FECHA-MES > '201503'                  
              THEN COMPUTE BONIFIC = A-HABER (1) * 0.6 
              ELSE                                 
           IF FECHA-MES > '201501'                  
              THEN COMPUTE BONIFIC = A-HABER (1) * 0.5 
              ELSE                                 
           IF FECHA-MES > '201403'                  
              THEN COMPUTE BONIFIC = A-HABER (1) * 0.4 
              ELSE                                 
           IF FECHA-MES > '201203'                  
              THEN COMPUTE BONIFIC = A-HABER (1) * 0.2. 
           COMPUTE ANTIGBON = BONIFIC * PORCANT.
           MOVE '*FD-GASTOS' TO LEYENDA.
           MOVE 'SI' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       GTOS-REP-X.     EXIT.


       CARGAR.
           IF BONIFIC NOT > ZEROS THEN GO TO CARGA1.     
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS TO  A-IOMA (I).               
           IF BON-20 = 'SI'                                        
              THEN COMPUTE A-HABER (I) = BONIFIC
                   COMPUTE A-ANTIG (I) = ANTIGBON 
              ELSE COMPUTE A-HABER (I) = BONIFIC * PORC / 100 
                   COMPUTE A-ANTIG (I) = ANTIGBON * PORC / 100.
           MOVE '02' TO A-CONC (I).
           MOVE LEYENDA TO A-CODIGO (I).
       CARGA1.                     
           MOVE ZEROS TO BONIFIC ANTIGBON.
           MOVE 'NO' TO BON-20.
       CARGAR-X.     EXIT.
                    
