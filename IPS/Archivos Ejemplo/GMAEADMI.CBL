       IDENTIFICATION DIVISION.                                             000
       PROGRAM-ID. GMAEADMIAA.                                              000
       AUTHOR. 'MONICA 23-5-94'                                             000
      *REMARKS.'CALCULA HABER Y BONIFICACIONES DE ADMINISTRACION'.          000
      *REMARKS.'24-9-97      ARREGLO PARA ANTIGUEDAD                  '.    000
      *REMARKS.'27-12-00     ACTUALIZO TABLA DE FIS-CON-TES           '.    000
      *REMARKS.'MOV.SET02.PPTO.9707DTOR.EJ.Y AS.DTO.2368/97           '.    000
      *REMARKS.'MOV.NOV05.SI R EN POS-15 LA BON EN 08-2005 PASA A 705 '.    000
      *REMARKS.'y en 10-2005 a 825 .                                 '.     000
      *REMARKS.'1-4-08 con bonif de fisca en hiscar                  '.     000
      *REMARKS.'CON BONI ARBA                                        '.     000
      *REMARKS.'MOV JUN13 ya no genero bonifregistro aca, pasa al gehi con otrs codigos.
      *REMARKS.'DDE PPTO SET13 BONIF DTO 169/03 solo para TESORERIA  '.     000
      *REMARKS.'MOV ABR14 cargo % de antig del 2014, letra V.        '.     000
      *REMARKS.'MOV MAY14 cargo bon CHOFER para fiscalia, sin reaj.  '.     000
      *REMARKS.'MOV AGO14 BONIF DTO 169/03 ppto set13 para los otros org de control que no tenian aplicado.
      *REMARKS.'MOV DIC14 agr bonif para scria de la ninez mrio de desarrollo, ppto mar07.                      
      *REMARKS.' en este pgm se aplican aum a bonif de org de control seg % aum categ 2, dp de 2013 o 2014.            
       ENVIRONMENT DIVISION.                                                000
       DATA DIVISION.                                                       000
       WORKING-STORAGE SECTION.                                             001
           COPY WSFECHA.CPY.
       77  I                 PIC 99 VALUE ZEROS.
       77  INDX              PIC 99 VALUE ZEROS.
       77  I-LETRA           PIC 99 VALUE ZEROS.
       77  J                 PIC 99 VALUE ZEROS.
       77  JJB               PIC 99 VALUE ZEROS.
       77  K                 PIC 99 VALUE ZEROS.
       77  M                 PIC 999 VALUE ZEROS.
       77  PORC              PIC 999 VALUE ZEROS.
       77  PORNOM            PIC 99V9 VALUE ZEROS.
       77  PORVIV            PIC 9V999 VALUE ZEROS.
       77  POS-2-3-ORI       PIC XX   VALUE ZEROS.
       77  ANTIG1            PIC 99 VALUE ZEROS.
       77  ANTIGT            PIC 99 VALUE ZEROS.
       77  MARCA1            PIC 9  VALUE ZEROS.
       77  MARCA2            PIC 9  VALUE ZEROS.
       77  ANTIG1-N          PIC S99 VALUE ZEROS.
       77  IND-F             PIC 99 VALUE ZEROS.
       77  LEYENDA           PIC X(16).
       77  BONIFIC           PIC S9(7)V99 VALUE ZEROS.
       77  BONI-ANT          PIC S9(7)V99 VALUE ZEROS.
       77  CATE1             PIC S9(7)V99 VALUE ZEROS.
       77  TOPEDEL           PIC S9(7)V99 VALUE ZEROS.
       77  DELEGADO          PIC S9(7)V99 VALUE ZEROS.
       77  ARBAD             PIC S9(7)V99 VALUE ZEROS.
       77  ARBAS             PIC S9(7)V99 VALUE ZEROS.
       77  BONIFICT          PIC S9(7) VALUE ZEROS.
       77  BONEJ             PIC S9(7)V99 VALUE ZEROS.
       77  BONI-FIS          PIC S9(7)V99 VALUE ZEROS.
       77  BONI-FI           PIC S9(7)V99 VALUE ZEROS.
       77  BONI-AUX          PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR1            PIC S9(7)V99 VALUE ZEROS.
       77  IMPOR2            PIC S9(7)V99 VALUE ZEROS.
       77  BON-20            PIC AA VALUE 'NO'.
       77  BONCONHIS         PIC S9(7)V99 VALUE ZEROS.
       77  SUM25             PIC S9(7)V99 VALUE ZEROS.
       77  DIFREMMIN         PIC S9(7)V99 VALUE ZEROS.
       77  HAB-AUX           PIC S9(7)V99 VALUE ZEROS.
       77  SUPL-AUX          PIC S9(7)V99 VALUE ZEROS.
       77  NOMINAL           PIC S9(7)V99 VALUE ZEROS.
       77  NOMINAL1           PIC S9(7)V99 VALUE ZEROS.
       77  ENCONTRE          PIC AA VALUE 'NO'.
       77  BON-REA           PIC S9(7)V99  VALUE ZEROS.
       77  HAB-TOPE          PIC S9(7)V99  VALUE ZEROS.
       77  ANTIG-AUX         PIC S9(10)V99 VALUE ZEROS.
       77  ANTIG-NEW         PIC S9(10)V99 VALUE ZEROS.
      *** cargo bas cat 24 p/una bonif de INST.DE LA VIVIENDA y base para la otra bon.
      *77  VIV-24            PIC S9(10)V99 VALUE 1703.2. cargo <> valores en perform, segun aum del v.mod.en perf.
       77  VIV-24            PIC S9(10)V99 VALUE ZEROS.  
       77  VIV-J             PIC S9(10)V99 VALUE 2000.00.   
       77  PTO-VIVIEN        PIC S9(05)V999999 VALUE ZEROS.  
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
    *****                                              2012
           02  FILLER  PIC X(10) VALUE '18 U 00300'.                      
    *****                                              2013
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

 ****** esta tabla tiene los valores p/la boni de vivienda de 1000 o 1200 Res5602/09, act con % de v.m.,fech-his.
       01  TABLA-DE-1000.  
           02  FILLER  PIC X(22) VALUE '200811 0100000 0120000'.
           02  FILLER  PIC X(22) VALUE '200904 0103676 0124411'.
           02  FILLER  PIC X(22) VALUE '201004 0111052 0133262'.
           02  FILLER  PIC X(22) VALUE '201008 0118426 0142111'.
           02  FILLER  PIC X(22) VALUE '201104 0149216 0179059'.
           02  FILLER  PIC X(22) VALUE '201204 0178709 0214451'.
           02  FILLER  PIC X(22) VALUE '201304 0196401 0235681'.
           02  FILLER  PIC X(22) VALUE '201310 0212620 0255143'.
           02  FILLER  PIC X(22) VALUE '201502 0227503 0273003'.
           02  FILLER  PIC X(22) VALUE '201504 0251095 0301313'.
           02  FILLER  PIC X(22) VALUE '201509 0271735 0326080'.
  *************************** no actualizar mas pq tiene una deuda pendiente, 25-8-15.
       01  T-1000       REDEFINES TABLA-DE-1000.  
           02  PUNTOSS OCCURS 11.
               03  ANIOMES     PIC X(6).
               03  F           PIC X.
               03  VALMEN8     PIC 9(5)V99.
               03  F           PIC X.
               03  VALMAY7     PIC 9(5)V99.

 ****** esta tabla tiene los valores los valores de puntos para la bonif de vivienda, fech-his.
       01  TABLA-DE-PUNTOS-VIV.
           02  FILLER  PIC X(14) VALUE '200104 0316000'.
           02  FILLER  PIC X(14) VALUE '200105 0469000'.
           02  FILLER  PIC X(14) VALUE '200106 0283000'.
           02  FILLER  PIC X(14) VALUE '200107 0194000'.
           02  FILLER  PIC X(14) VALUE '200108 0262000'.
           02  FILLER  PIC X(14) VALUE '200109 0252000'.
           02  FILLER  PIC X(14) VALUE '200110 0139000'.
           02  FILLER  PIC X(14) VALUE '200111 0166000'.
           02  FILLER  PIC X(14) VALUE '200112 0149000'.
           02  FILLER  PIC X(14) VALUE '200201 0080000'.
           02  FILLER  PIC X(14) VALUE '200202 0268000'.
           02  FILLER  PIC X(14) VALUE '200203 0090000'.
           02  FILLER  PIC X(14) VALUE '200204 0133000'.
           02  FILLER  PIC X(14) VALUE '200205 0135000'.
           02  FILLER  PIC X(14) VALUE '200206 0113000'.
           02  FILLER  PIC X(14) VALUE '200207 0139000'.
           02  FILLER  PIC X(14) VALUE '200208 0179000'.
           02  FILLER  PIC X(14) VALUE '200209 0205000'.
           02  FILLER  PIC X(14) VALUE '200210 0182000'.
           02  FILLER  PIC X(14) VALUE '200211 0084000'.
           02  FILLER  PIC X(14) VALUE '200212 0310000'.
           02  FILLER  PIC X(14) VALUE '200301 0221000'.
           02  FILLER  PIC X(14) VALUE '200302 0430000'.
           02  FILLER  PIC X(14) VALUE '200303 0170000'.
           02  FILLER  PIC X(14) VALUE '200304 0192000'.
           02  FILLER  PIC X(14) VALUE '200305 0225000'.
           02  FILLER  PIC X(14) VALUE '200306 0289000'.
           02  FILLER  PIC X(14) VALUE '200307 0220000'.
           02  FILLER  PIC X(14) VALUE '200308 0254000'.
           02  FILLER  PIC X(14) VALUE '200309 0298000'.
           02  FILLER  PIC X(14) VALUE '200310 0298000'.
           02  FILLER  PIC X(14) VALUE '200311 0268000'.
           02  FILLER  PIC X(14) VALUE '200312 0285000'.
           02  FILLER  PIC X(14) VALUE '200401 0164000'.
           02  FILLER  PIC X(14) VALUE '200402 0328000'.
           02  FILLER  PIC X(14) VALUE '200403 0277000'.
           02  FILLER  PIC X(14) VALUE '200404 0331000'.
           02  FILLER  PIC X(14) VALUE '200405 0413000'.
           02  FILLER  PIC X(14) VALUE '200406 0247000'.
           02  FILLER  PIC X(14) VALUE '200407 0213000'.
           02  FILLER  PIC X(14) VALUE '200408 0337000'.
           02  FILLER  PIC X(14) VALUE '200409 0264000'.
           02  FILLER  PIC X(14) VALUE '200410 0335000'.
           02  FILLER  PIC X(14) VALUE '200411 0292000'.
           02  FILLER  PIC X(14) VALUE '200412 0217000'.
           02  FILLER  PIC X(14) VALUE '200501 0192000'.
           02  FILLER  PIC X(14) VALUE '200502 0270000'.
           02  FILLER  PIC X(14) VALUE '200503 0371000'.
           02  FILLER  PIC X(14) VALUE '200504 0302000'.
           02  FILLER  PIC X(14) VALUE '200505 0667000'.
           02  FILLER  PIC X(14) VALUE '200506 0254000'.
           02  FILLER  PIC X(14) VALUE '200507 0285000'.
           02  FILLER  PIC X(14) VALUE '200508 0319000'.
           02  FILLER  PIC X(14) VALUE '200509 0402000'.
           02  FILLER  PIC X(14) VALUE '200510 0359000'.
           02  FILLER  PIC X(14) VALUE '200511 0374000'.
           02  FILLER  PIC X(14) VALUE '200512 0457000'.
           02  FILLER  PIC X(14) VALUE '200601 0342000'.
           02  FILLER  PIC X(14) VALUE '200602 0533000'.
           02  FILLER  PIC X(14) VALUE '200603 0272000'.
           02  FILLER  PIC X(14) VALUE '200604 0248000'.
           02  FILLER  PIC X(14) VALUE '200605 0843000'.
           02  FILLER  PIC X(14) VALUE '200606 0672000'.
           02  FILLER  PIC X(14) VALUE '200607 0460000'.
           02  FILLER  PIC X(14) VALUE '200608 0436000'.
           02  FILLER  PIC X(14) VALUE '200609 0918000'.
           02  FILLER  PIC X(14) VALUE '200610 0949000'.
           02  FILLER  PIC X(14) VALUE '200611 0853000'.
           02  FILLER  PIC X(14) VALUE '200612 0850000'.
           02  FILLER  PIC X(14) VALUE '200701 0913000'.
           02  FILLER  PIC X(14) VALUE '200702 1026000'.
           02  FILLER  PIC X(14) VALUE '200703 0634000'.
           02  FILLER  PIC X(14) VALUE '200704 0930000'.
           02  FILLER  PIC X(14) VALUE '200705 0940000'.
           02  FILLER  PIC X(14) VALUE '200706 0828000'.
           02  FILLER  PIC X(14) VALUE '200707 0724000'.
           02  FILLER  PIC X(14) VALUE '200708 1072000'.
           02  FILLER  PIC X(14) VALUE '200709 0688000'.
           02  FILLER  PIC X(14) VALUE '200710 0509000'.
           02  FILLER  PIC X(14) VALUE '200711 0664000'.
           02  FILLER  PIC X(14) VALUE '200712 0574000'.
           02  FILLER  PIC X(14) VALUE '200801 0570000'.
           02  FILLER  PIC X(14) VALUE '200802 0941000'.
           02  FILLER  PIC X(14) VALUE '200803 0621000'.
           02  FILLER  PIC X(14) VALUE '200804 0734000'.
           02  FILLER  PIC X(14) VALUE '200805 0428000'.
           02  FILLER  PIC X(14) VALUE '200806 0649000'.
           02  FILLER  PIC X(14) VALUE '200807 0476000'.
           02  FILLER  PIC X(14) VALUE '200808 0463000'.
           02  FILLER  PIC X(14) VALUE '200809 0492000'.
           02  FILLER  PIC X(14) VALUE '200810 0478000'.
           02  FILLER  PIC X(14) VALUE '200811 0593000'.
           02  FILLER  PIC X(14) VALUE '200812 0738000'.
           02  FILLER  PIC X(14) VALUE '200901 0792000'.
           02  FILLER  PIC X(14) VALUE '200902 1250000'.
           02  FILLER  PIC X(14) VALUE '200903 0556000'.
           02  FILLER  PIC X(14) VALUE '200904 0566000'.
           02  FILLER  PIC X(14) VALUE '200905 0845000'.
           02  FILLER  PIC X(14) VALUE '200906 0857000'.
           02  FILLER  PIC X(14) VALUE '200907 0635000'.
           02  FILLER  PIC X(14) VALUE '200908 0738000'.
           02  FILLER  PIC X(14) VALUE '200909 0678000'.
           02  FILLER  PIC X(14) VALUE '200910 0445000'.
           02  FILLER  PIC X(14) VALUE '200911 0620000'.
           02  FILLER  PIC X(14) VALUE '200912 0624000'.
           02  FILLER  PIC X(14) VALUE '201001 0640000'.
           02  FILLER  PIC X(14) VALUE '201002 1048000'.
           02  FILLER  PIC X(14) VALUE '201003 0590000'.
           02  FILLER  PIC X(14) VALUE '201004 0728000'.
           02  FILLER  PIC X(14) VALUE '201005 0677000'.
           02  FILLER  PIC X(14) VALUE '201006 0651000'.
           02  FILLER  PIC X(14) VALUE '201007 0584000'.
           02  FILLER  PIC X(14) VALUE '201008 0708000'.
           02  FILLER  PIC X(14) VALUE '201009 0890000'.
           02  FILLER  PIC X(14) VALUE '201010 0814000'.
           02  FILLER  PIC X(14) VALUE '201011 0642000'.
           02  FILLER  PIC X(14) VALUE '201012 0482000'.
           02  FILLER  PIC X(14) VALUE '201101 0681000'.
           02  FILLER  PIC X(14) VALUE '201102 0904000'.
           02  FILLER  PIC X(14) VALUE '201103 0661000'.
           02  FILLER  PIC X(14) VALUE '201104 0861000'.
           02  FILLER  PIC X(14) VALUE '201105 1029000'.
           02  FILLER  PIC X(14) VALUE '201106 0786000'.
           02  FILLER  PIC X(14) VALUE '201107 0986000'.
           02  FILLER  PIC X(14) VALUE '201108 1038000'.
           02  FILLER  PIC X(14) VALUE '201109 0880000'.
           02  FILLER  PIC X(14) VALUE '201110 1128000'.
           02  FILLER  PIC X(14) VALUE '201111 0677000'.
           02  FILLER  PIC X(14) VALUE '201112 1184000'.
           02  FILLER  PIC X(14) VALUE '201201 1066000'.
           02  FILLER  PIC X(14) VALUE '201204 0884050'.
           02  FILLER  PIC X(14) VALUE '201205 0946937'.
           02  FILLER  PIC X(14) VALUE '201206 1193000'.
           02  FILLER  PIC X(14) VALUE '201207 0972000'.
           02  FILLER  PIC X(14) VALUE '201208 0650000'.
           02  FILLER  PIC X(14) VALUE '201209 1090000'.
           02  FILLER  PIC X(14) VALUE '201210 0763000'.
           02  FILLER  PIC X(14) VALUE '201211 0862000'.
           02  FILLER  PIC X(14) VALUE '201212 1133000'.
           02  FILLER  PIC X(14) VALUE '201301 0946000'.
           02  FILLER  PIC X(14) VALUE '201302 1518000'.
           02  FILLER  PIC X(14) VALUE '201303 0734000'.
           02  FILLER  PIC X(14) VALUE '201304 0951000'.
           02  FILLER  PIC X(14) VALUE '201305 1095000'.
           02  FILLER  PIC X(14) VALUE '201306 1095000'.
           02  FILLER  PIC X(14) VALUE '201307 0967000'.
           02  FILLER  PIC X(14) VALUE '201308 1065000'.
           02  FILLER  PIC X(14) VALUE '201309 1367000'.
           02  FILLER  PIC X(14) VALUE '201310 0958000'.
           02  FILLER  PIC X(14) VALUE '201311 1098000'.
           02  FILLER  PIC X(14) VALUE '201312 1637000'.
           02  FILLER  PIC X(14) VALUE '201401 1197000'.
           02  FILLER  PIC X(14) VALUE '201402 1429000'.
           02  FILLER  PIC X(14) VALUE '201403 1083000'.
           02  FILLER  PIC X(14) VALUE '201404 1151000'.
           02  FILLER  PIC X(14) VALUE '201405 1013000'.
           02  FILLER  PIC X(14) VALUE '201406 1396000'.
           02  FILLER  PIC X(14) VALUE '201407 1377000'.
           02  FILLER  PIC X(14) VALUE '201408 1963000'.
           02  FILLER  PIC X(14) VALUE '201409 1857000'.
           02  FILLER  PIC X(14) VALUE '201410 1798000'.
           02  FILLER  PIC X(14) VALUE '201411 2336235'.
           02  FILLER  PIC X(14) VALUE '201412 2593604'.
           02  FILLER  PIC X(14) VALUE '201501 2593512'.
           02  FILLER  PIC X(14) VALUE '201502 3204548'.
           02  FILLER  PIC X(14) VALUE '201503 1585120'.
           02  FILLER  PIC X(14) VALUE '201504 2475780'.
           02  FILLER  PIC X(14) VALUE '201505 4678293'.
           02  FILLER  PIC X(14) VALUE '201506 4482014'.
           02  FILLER  PIC X(14) VALUE '201507 3938771'.
           02  FILLER  PIC X(14) VALUE '201508 5826687'.
           02  FILLER  PIC X(14) VALUE '201509 5540064'.
           02  FILLER  PIC X(14) VALUE '201510 6233879'.
           02  FILLER  PIC X(14) VALUE '201511 5621308'.
           02  FILLER  PIC X(14) VALUE '201512 4058790'.
           02  FILLER  PIC X(14) VALUE '201601 4237016'.
           02  FILLER  PIC X(14) VALUE '201602 3437854'.
           02  FILLER  PIC X(14) VALUE '201603 2808149'.
           02  FILLER  PIC X(14) VALUE '201604 6558267'.
           02  FILLER  PIC X(14) VALUE '201605 3725850'.
           02  FILLER  PIC X(14) VALUE '201606 4555465'.
           02  FILLER  PIC X(14) VALUE '201607 4748617'.

     **************** CUANDO SE MODIF ESTA TAB TB HAY Q MODIF LA DE GMAEOSANI.
       01  TABLA-VIV    REDEFINES TABLA-DE-PUNTOS-VIV.
           02  PPUNTOS OCCURS  182.
               03  ANIOMESV    PIC X(6).
               03  F           PIC X.
               03  VALPUN      PIC 9(2)V9(5).

      ***  MODIFICACIÏN 07-05-96. POR EQUIPARACIÏN DTO. 4273/94. ***  BONIF dto 3464/93  ORG.CTROL.
      ***  SE AGR. CARGOS J CON DISTINTAS FECHAS DE VIG. J1701 DDE
      ***  PPTO. 9501 Y J1801 DESDE 9312

      ***  MOD.13-06-96. EQUI. DTO. 3948/94.AGR.J5401 Y J5501 PPTO 9412
      ***  MOV.9912 AGREGO J1802 POR EQUIV.AL DIRECTOR *********
      ***  MOV.200001 LA TABLA VALE PARA CONTADURIA Y TESORERIA
      ***  MOV.200002 LA TABLA NO VALE MAS DESDE PPTO. 200001
      ***  MOV.200305 HAY COEF.PARA 319J Y 317J,ES 321J % 321P QUE SE
      ***   APLICA SOBRE 319P Y 317P

     ***** si se modifica en esta tabla cargo J0901 modificar
     ******                      GMAEPODJUD  (OJOOOOOOO!!!)

     ****** el % de aum de ago08 lo cargo en el perform, IDEM mar09 y sgtes hasta mar12 pq en abril12 amplian a todos y l
     ****** os agr como los 14 ultimos renglones.
     ****** Dde ppto mar11 pongo bonif J1902=J0102.                                                                          
       01  TABLA-FISC-CONT-TESOR.
           02  F PIC X(36) VALUE '324XXJ052500 0163316 0539786 0600332'.
           02  F PIC X(36) VALUE '323XXJ052000 0163316 0520243 0580213'.
           02  F PIC X(36) VALUE '322XXJ052000 0161760 0517375 0577345'.
           02  F PIC X(36) VALUE '321XXJ052000 0161760 0514630 0574599'.
           02  F PIC X(36) VALUE '317XXJ037375 0116200 0262184 0297316'.
           02  F PIC X(36) VALUE '319XXJ041843 0130100 0264958 0300090'.
           02  F PIC X(36) VALUE '324XXN052500 0163316 0539786 0600332'.
           02  F PIC X(36) VALUE '323XXN052000 0163316 0520243 0580213'.
           02  F PIC X(36) VALUE '322XXN052000 0161760 0517375 0577345'.
           02  F PIC X(36) VALUE '321XXN052000 0161760 0514630 0574599'.
           02  F PIC X(36) VALUE '321XXP064000 0199079 0530637 0604443'.
           02  F PIC X(36) VALUE '320XXP052000 0161760 0460147 0520117'.
           02  F PIC X(36) VALUE '319XXP051500 0160206 0451374 0510767'.
           02  F PIC X(36) VALUE '318XXP047500 0147761 0425633 0480413'.
           02  F PIC X(36) VALUE '317XXP046000 0143096 0416723 0469773'.
           02  F PIC X(36) VALUE '316XXP043500 0135318 0402246 0452413'.
           02  F PIC X(36) VALUE '315XXP040000 0124424 0382255 0428383'.
           02  F PIC X(36) VALUE '314XXP039000 0121319 0375535 0420512'.
           02  F PIC X(36) VALUE '313XXP039000 0121319 0374603 0419580'.
           02  F PIC X(36) VALUE '312XXP037500 0116654 0365544 0408792'.
           02  F PIC X(36) VALUE '311XXP037000 0115098 0362514 0405185'.
           02  F PIC X(36) VALUE '310XXP036500 0113542 0369608 0401702'.
           02  F PIC X(36) VALUE '309XXP035500 0110432 0352359 0393300'.
           02  F PIC X(36) VALUE '308XXP034500 0107320 0344254 0384041'.
           02  F PIC X(36) VALUE 'J1101X101500 0315771 0783314 0850939'.
           02  F PIC X(36) VALUE 'J2702X085000 0264441 0703252 0759883'.
           02  F PIC X(36) VALUE 'J2502X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J2202X064125 0199484 0528957 0571677'.
           02  F PIC X(36) VALUE 'J2302X060750 0188984 0501117 0541589'.
           02  F PIC X(36) VALUE 'J2402X057375 0178485 0473277 0511501'.
           02  F PIC X(36) VALUE 'J0301X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J0102X082000 0255141 0638199 0692839'.
           02  F PIC X(36) VALUE 'J1001X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J0602X076500 0237996 0599628 0650597'.
           02  F PIC X(36) VALUE 'J5401X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J5501X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J1701X136000 0902143 1626529 1789156'.
           02  F PIC X(36) VALUE 'J1801X136000 0902143 1626529 1789156'.
           02  F PIC X(36) VALUE 'J1802X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J2602X076500 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J14A2X076500 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J1402X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J0401X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J5201X101500 0315771 0638199 0692839'.
           02  F PIC X(36) VALUE 'J0702X085000 0237996 0599628 0650597'.
           02  F PIC X(36) VALUE 'J2902X085000 0237996 0599628 0650597'.
           02  F PIC X(36) VALUE 'J1301X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J04S1X045000 0139999 0331184 0361165'.
           02  F PIC X(36) VALUE 'J2802X085000 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J3102X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J0801X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J1701X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J5301X101500 0315771 0638199 0692839'.
           02  F PIC X(36) VALUE 'J6801X101500 0315771 0638199 0692839'.
           02  F PIC X(36) VALUE 'J1202X076500 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J0901X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J7201X101500 0315771 0638199 0692839'.
           02  F PIC X(36) VALUE 'J0701X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J7301X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J1601X322200 1002382 1807255 1987952'.
           02  F PIC X(36) VALUE 'J0802X085000 0237996 0599628 0650597'.
           02  F PIC X(36) VALUE 'J1502X076500 0237996 0632926 0683894'.
     ***** 02  F  PIC X(20) VALUE 'J1902X067500 0000000'.
           02  F PIC X(36) VALUE 'J1902X082000 0255141 0638199 0692839'.
           02  F PIC X(36) VALUE 'J2002X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J7501X136000 0423107 0944004 1034615'.
           02  F PIC X(36) VALUE 'J3702X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J3802X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J08S1X045000 0139999 0331184 0361165'.
           02  F PIC X(36) VALUE 'J07S1X045000 0139999 0331184 0361165'.
           02  F PIC X(36) VALUE 'J03S1X045000 0139999 0331184 0361165'.
           02  F PIC X(36) VALUE 'J09S1X045000 0139999 0331184 0361165'.
           02  F PIC X(36) VALUE 'J3902X076500 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J4102X085000 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J4202X085000 0237996 0632926 0683894'.
           02  F PIC X(36) VALUE 'J4302X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE 'J4402X067500 0209983 0556797 0601766'.
           02  F PIC X(36) VALUE '318XXX000000 0094764 0264958 0300090'.
           02  F PIC X(36) VALUE '317XXX000000 0094764 0262184 0297316'.
           02  F PIC X(36) VALUE '316XXX000000 0092492 0251966 0286256'.
           02  F PIC X(36) VALUE '315XXX000000 0088171 0237875 0270563'.
           02  F PIC X(36) VALUE '314XXX000000 0088171 0235624 0268312'.
           02  F PIC X(36) VALUE '313XXX000000 0083899 0226691 0257795'.
           02  F PIC X(36) VALUE '312XXX000000 0083899 0225712 0256816'.
           02  F PIC X(36) VALUE '311XXX000000 0079578 0216048 0245550'.
           02  F PIC X(36) VALUE '310XXX000000 0079578 0215871 0245373'.
           02  F PIC X(36) VALUE '309XXX000000 0074965 0207881 0235673'.
           02  F PIC X(36) VALUE '308XXX000000 0074965 0207658 0235450'.
           02  F PIC X(36) VALUE '307XXX000000 0074965 0207607 0235399'.
           02  F PIC X(36) VALUE '306XXX000000 0074965 0207329 0235121'.
           02  F PIC X(36) VALUE '305XXX000000 0074965 0204649 0232441'.
           02  F PIC X(36) VALUE 'J7601X000000 0315771 0783314 0850939'.
           02  F PIC X(36) VALUE 'J7701X000000 0315771 0783314 0850939'.
       01  TAB-FIS REDEFINES TABLA-FISC-CONT-TESOR.
           02  T-F  OCCURS 92.
               03 CARGO-F  PIC X(6).
               03 MONTO-F  PIC 9(4)V99.
               03 F        PIC X.         
               03 MONTO-F2012  PIC 9(5)V99.
               03 F        PIC X.         
               03 MONTO-F2014  PIC 9(5)V99.
               03 F        PIC X.         
               03 MONTO-F22014  PIC 9(5)V99.

  ******DDE PPTO SET13 aumento solo a tesoreria pq se quejaron q hay diferen.con otros organismos. SE vuelven a unificar en ago14.
     *****  Bonificacion dto.169/2003 ppto.enero2004                            ****** DESDE PPTO JUL2007 se carga en gehi *******
     ***** si se modifica en esta tabla cargo J0901 modificar
     ******                      GMAEPODJUD  (OJOOOOOOO!!!)
       01  TABLA-SOLO-FISCALIA.
           02  FILLER   PIC X(19) VALUE '321XXP064000 076000'.
           02  FILLER   PIC X(19) VALUE '320XXP064000 076000'.
           02  FILLER   PIC X(19) VALUE '319XXP064000 076000'.
           02  FILLER   PIC X(19) VALUE '318XXP064000 076000'.
           02  FILLER   PIC X(19) VALUE '317XXP061000 073000'.
           02  FILLER   PIC X(19) VALUE '316XXP061000 073000'.
           02  FILLER   PIC X(19) VALUE '315XXP061000 073000'.
           02  FILLER   PIC X(19) VALUE '314XXP061000 073000'.
           02  FILLER   PIC X(19) VALUE '313XXP058000 073000'.
           02  FILLER   PIC X(19) VALUE '312XXP058000 070000'.
           02  FILLER   PIC X(19) VALUE '311XXP058000 070000'.
           02  FILLER   PIC X(19) VALUE '310XXP058000 070000'.
           02  FILLER   PIC X(19) VALUE '309XXP055000 067000'.
           02  FILLER   PIC X(19) VALUE '308XXP055000 067000'.
           02  FILLER   PIC X(19) VALUE '324XXX065000 077000'.
           02  FILLER   PIC X(19) VALUE '323XXX064000 076000'.
           02  FILLER   PIC X(19) VALUE '322XXX064000 076000'.
           02  FILLER   PIC X(19) VALUE '321XXX064000 076000'.
           02  FILLER   PIC X(19) VALUE '319XXX064000 076000'.
           02  FILLER   PIC X(19) VALUE '318XXX048500 061000'.
           02  FILLER   PIC X(19) VALUE '317XXX046000 058000'.
           02  FILLER   PIC X(19) VALUE '316XXX043500 055000'.
           02  FILLER   PIC X(19) VALUE '315XXX040000 052000'.
           02  FILLER   PIC X(19) VALUE '314XXX039000 051000'.
           02  FILLER   PIC X(19) VALUE '313XXX039000 051000'.
           02  FILLER   PIC X(19) VALUE '312XXX037000 049000'.
           02  FILLER   PIC X(19) VALUE '311XXX036500 048000'.
           02  FILLER   PIC X(19) VALUE '310XXX032000 044000'.
           02  FILLER   PIC X(19) VALUE '309XXX030000 042000'.
           02  FILLER   PIC X(19) VALUE '308XXX025000 037000'.
           02  FILLER   PIC X(19) VALUE '307XXX024000 036000'.
           02  FILLER   PIC X(19) VALUE '306XXX023000 035000'.
           02  FILLER   PIC X(19) VALUE '305XXX022000 034000'.
           02  FILLER   PIC X(19) VALUE '304XXX021000 033000'.
           02  FILLER   PIC X(19) VALUE '303XXX020000 032000'.
           02  FILLER   PIC X(19) VALUE '302XXX019000 031000'.
           02  FILLER   PIC X(19) VALUE '301XXX018000 030000'.
           02  FILLER   PIC X(19) VALUE 'J2702X230000 230000'.
           02  FILLER   PIC X(19) VALUE 'J2502X078000 085000'.
           02  FILLER   PIC X(19) VALUE 'J2202X074100 080750'.
           02  FILLER   PIC X(19) VALUE 'J2302X070200 076500'.
           02  FILLER   PIC X(19) VALUE 'J2402X066300 072250'.
           02  FILLER   PIC X(19) VALUE 'J0301X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J0102X310000 150000'.
           02  FILLER   PIC X(19) VALUE 'J1001X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J0602X230000 095000'.
           02  FILLER   PIC X(19) VALUE 'J5401X310000 250000'.
           02  FILLER   PIC X(19) VALUE 'J5501X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J1801X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J1802X078000 120000'.
           02  FILLER   PIC X(19) VALUE 'J2602X207000 207000'.
           02  FILLER   PIC X(19) VALUE 'J14A2X207000 207000'.
           02  FILLER   PIC X(19) VALUE 'J1402X078000 085000'.
           02  FILLER   PIC X(19) VALUE 'J0401X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J5201X250000 310000'.
           02  FILLER   PIC X(19) VALUE 'J0702X230000 095000'.
           02  FILLER   PIC X(19) VALUE 'J2902X230000 230000'.
           02  FILLER   PIC X(19) VALUE 'J1301X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J04S1X045000 077000'.
           02  FILLER   PIC X(19) VALUE 'J2802X230000 230000'.
           02  FILLER   PIC X(19) VALUE 'J3102X078000 207000'.
           02  FILLER   PIC X(19) VALUE 'J0801X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J1701X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J5301X250000 250000'.
           02  FILLER   PIC X(19) VALUE 'J1101X250000 250000'.
           02  FILLER   PIC X(19) VALUE 'J6801X101500 250000'.
           02  FILLER   PIC X(19) VALUE 'J1202X207000 207000'.
           02  FILLER   PIC X(19) VALUE 'J0901X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J7201X250000 250000'.
           02  FILLER   PIC X(19) VALUE 'J0701X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J7301X310000 310000'.
           02  FILLER   PIC X(19) VALUE 'J1601X322200 310000'.
           02  FILLER   PIC X(19) VALUE 'J0802X230000 095000'.
********** 02  FILLER   PIC X(19) VALUE 'J1502X076500'.cod viejo
********** 02  FILLER   PIC X(19) VALUE 'J1902X067500'.cod viejo
           02  FILLER   PIC X(19) VALUE 'J2002X078000 120000'.
           02  FILLER   PIC X(19) VALUE 'J08S1X045000 077000'.
           02  FILLER   PIC X(19) VALUE 'J07S1X045000 077000'.
           02  FILLER   PIC X(19) VALUE 'J03S1X045000 077000'.
           02  FILLER   PIC X(19) VALUE 'J09S1X045000 077000'.
           02  FILLER   PIC X(19) VALUE 'J7501X000000 310000'.
           02  FILLER   PIC X(19) VALUE 'J3702X000000 085000'.
           02  FILLER   PIC X(19) VALUE 'J3802X000000 085000'.
           02  FILLER   PIC X(19) VALUE 'J3902X207000 207000'.
           02  FILLER   PIC X(19) VALUE 'J4102X207000 230000'.
       01  TAB-SOLO-FIS REDEFINES TABLA-SOLO-FISCALIA.              
           02  T-FI OCCURS 83.
               03 CARGO-FI PIC X(6).
               03 F        PIC X(7).
               03 MONTO-FI PIC 9(4)V99.

       01  TABLA-MAR09.                      
           02  FILLER   PIC X(09) VALUE '05 008937'.
           02  FILLER   PIC X(09) VALUE '06 009000'.
           02  FILLER   PIC X(09) VALUE '07 009105'.
           02  FILLER   PIC X(09) VALUE '08 009179'.
           02  FILLER   PIC X(09) VALUE '09 009258'.
           02  FILLER   PIC X(09) VALUE '10 009326'.
           02  FILLER   PIC X(09) VALUE '11 009389'.
           02  FILLER   PIC X(09) VALUE '12 009463'.
           02  FILLER   PIC X(09) VALUE '13 009553'.
           02  FILLER   PIC X(09) VALUE '14 009653'.
           02  FILLER   PIC X(09) VALUE '15 010079'.
           02  FILLER   PIC X(09) VALUE '16 010558'.
           02  FILLER   PIC X(09) VALUE '17 011326'.
           02  FILLER   PIC X(09) VALUE '18 011679'.
           02  FILLER   PIC X(09) VALUE '19 011842'.
           02  FILLER   PIC X(09) VALUE '20 012037'.
           02  FILLER   PIC X(09) VALUE '21 017789'.
           02  FILLER   PIC X(09) VALUE '22 018142'.
           02  FILLER   PIC X(09) VALUE '23 018511'.
           02  FILLER   PIC X(09) VALUE '24 020095'.
       01  TAB-BON-MAR09 REDEFINES TABLA-MAR09.                      
           02  T-MAR09 OCCURS 20.
               03 CAT-MAR09 PIC XX.   
               03 F         PIC X.
               03 MO-MAR09  PIC 9(4)V99.

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
                   06 POS-1-2.                                              009
                      07 POS-1     PIC X.                                   009
                      07 POS-2     PIC X.                                   009
                    06 POS-3-4-5.                                           009
                      07 POS-3     PIC X.                                   009
                      07 POS-4-5.                                           009
                         08 POS-4        PIC X.                             009
                         08 POS-5        PIC X.                             009
                      07 POS-N-4-5 REDEFINES POS-4-5 PIC 99.                009
                  05 R-POS-1-5 REDEFINES POS-1-5.                           009
                    06 FILLER      PIC X.
                    06 POS-2-3     PIC XX.
                    06 POS-2-3R REDEFINES POS-2-3 PIC 99.
                    06 FILLER      PIC XX.
                  05 R1-POS-1-5 REDEFINES POS-1-5.                           009
                    06 POS-1-3     PIC XXX.
                    06 FILLER      PIC XX.
                  05 POS-6           PIC X.                                 009
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
                 04 POS-15-16.                                              009
                    05 POS-15      PIC X.                                   009
                    05 POS-16      PIC X.                                   009
               03 POS-17-20.                            
                  04 POS-17-19        PIC X(3).
                  04 MARDGI           PIC X.
               03 POS-N-17-20 REDEFINES POS-17-20        
                                     PIC 99V99.
           02 CODIGOCOPA REDEFINES CODIGO.                                  009
               03 POS-1-A-12      PIC X(12).             
               03 POS-13-A-17     PIC X(5).               
               03 POS-13-A-17N REDEFINES POS-13-A-17 PIC 9(3)V99.               
               03 POS-17-AA-20     PIC X(03).             
           02 CODIGO2.                                                      009
              03 POS-1-DE-HISCAR     PIC X.      
              03 POS-2-3-DE-HISCAR   PIC XX.    
              03 FILLER            PIC X(12).                               009
           02 CODIGO2R REDEFINES CODIGO2. 
              03 POS-1-8-DE-HISCAR     PIC X(8).      
              03 POS-9-15-DE-HISCAR   PIC X(7).    
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
                 05 A-POS-9-12.                                             009
                   06 A-POS-9-10.                                             009
                    07 A-POS-9   PIC X.                                     009
                    07 A-POS-10  PIC X.                                     009
                   06 A-POS-11-12  PIC XX.                                    009
                 05 A-POS-9-12R REDEFINES A-POS-9-12 PIC 999V9.
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
 *********** son las que trsladan porcentaje de calculo de boni
           IF (POS-1 = '3' OR 'J') AND SI-ES-VIAL = 'A'
             AND (POS-15 = 'Q' OR 'Z' OR 'U' OR 'Y' OR '1' OR '3'      
                  OR '0' OR ' ' OR '2' OR 'E' OR '4' OR '5' OR '6' 
                  OR 'L' OR '8' OR 'W' OR 'X')
                   THEN
                   MOVE ZEROS TO ANTIG1 
                   GO TO ALHABER.
    ******   agregado 7-6-05   solo para adecuaciones en linea
            IF COD-RETRO = 'C'       
               THEN
               IF POS-1 = 'V'      
                THEN
                  IF POS-11-12 NUMERIC AND (POS-13-14 NUMERIC           
                                  OR POS-13-14 = '  ')       
                    THEN MOVE POS-11-12 TO ANTIG1
                         IF POS-13-14 = '00' OR '  '           
                              THEN
                                 MOVE 100 TO PORC
                                 MOVE '00' TO POS-13-14   
                                 GO TO FINVALID
                              ELSE
                                 MOVE POS-N-13-14 TO PORC
                                 GO TO FINVALID   
                     ELSE
                          MOVE '1' TO MARCA-MASI 
                          GO TO CERRAR     
                 ELSE
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

    ******   agregado 7-6-05   solo para adecuaciones en linea
      *  DISPLAY 'ingresa ' NRO ' ' CODIGO  ' ' MARCA-MASI.
      * MASIVO POR ERROR EN ANIOS O PORCENTAJE PARA VIALIDAD *****



   ********     OJO  AMPARO instalado el 28-09-07 ******************
   ********        PEDIDO : 11097     del 5-7-16
           IF MARCA = '1' AND NRO = '1047798240' AND 
                            POS-1-A-16 = 'J0901J0A510000T0'    
                   THEN MOVE 100 TO PORC 
                        MOVE POS-9-10  TO ANTIG1
                        GO TO FINVALID.

   ********        PEDIDO : 11097     del 5-7-16


           IF POS-1 = 'V'      
             THEN
              IF POS-11-12 NOT NUMERIC OR POS-13-14 NOT NUMERIC
                 OR POS-13-14 = '00'
                 THEN MOVE 1 TO MARCA-MASI
                      GO TO CERRAR
                 ELSE MOVE POS-11-12 TO ANTIG1
                      MOVE POS-N-13-14 TO PORC

              ELSE
      *    PERMITE PORC 00 para leyes  Y consejero escolar con cat13 ******
      *      DISPLAY 'ingresa ' NRO ' ' CODIGO  ' ' MARCA-MASI

              IF POS-9-10 NOT NUMERIC OR POS-11-12 NOT NUMERIC
                   THEN MOVE 1 TO MARCA-MASI
                        GO TO CERRAR
                   ELSE MOVE POS-9-10  TO ANTIG1
     *****    DISPLAY 'sigue ' NRO ' ' ANTIG1 ' ' POS-11-12
     *****              ' ' FECHA-MES
                        IF POS-1 = '3' 
                         THEN IF POS-11-12 = '00' 
                             THEN IF POS-6 = 'L' OR     
                                     POS-1-A-6 = '31330C'
                               THEN IF FECHA-MES < '199808'
                                    THEN MOVE 1 TO MARCA-MASI
                                         GO TO CERRAR 
                                    ELSE MOVE 100   TO PORC 
                               ELSE MOVE 1 TO MARCA-MASI
                                     GO TO CERRAR 
                             ELSE MOVE POS-11-12R TO PORC  
                         ELSE IF POS-11-12 = '00' 
                             THEN MOVE 1 TO MARCA-MASI
                                     GO TO CERRAR 
                             ELSE MOVE POS-11-12R TO PORC.  

   ********     OJO  AMPARO instalado el 28-09-07 ******************
   ********IF MARCA = '1' AND NRO = '7017872740' AND 
  *********                               FECHA-MES > '200710'
   **********      THEN MOVE 100 TO PORC.
************
   ********     OJO  AMPARO instalado el 28-09-07 ******************
  *******************************   dado de baja 2015
       FINVALID.
           MOVE 0 TO I.
           IF POS-1 = '3' AND SI-ES-VIAL = 'A' AND
              (POS-15 = 'G' OR 'I' OR 'J' OR 'S' OR 'M' OR '1'      
              OR 'O' OR 'P' OR 'Q' OR 'W' OR 'Z' OR 'U' OR 'Y'            
              OR '3' OR '0' OR ' ' OR '2' OR 'E' OR '4' OR '5'                              
              OR '6' OR 'L' OR '7' OR '8' OR 'N' OR 'X')                                                    
                   THEN
                   MOVE ZEROS TO ANTIG1 
                   GO TO ALHABER.
     ****** agrego dde mov.julio 08 para cargo por coeficiente
           IF FECHA-MES < '200808' THEN NEXT SENTENCE                                          
                                   ELSE
              IF (POS-1-A-6 = '32430J' OR 'J2502J') AND                          
                                          POS-15 = 'K'                          
              THEN
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
              IF POS-17-20 NOT NUMERIC 
               THEN DISPLAY ' sin inform en coefic ' REGISTRO (1) 
                    COMPUTE A-HABER (I) = H-HABER * PORC / 000    
                    GO TO CERRAR                
               ELSE
               COMPUTE A-HABER (I) =                                       
                              H-HABER * POS-N-17-20 * PORC / 100    
                   GO TO CERRAR.                     
     ****** agrego dde mov.julio 08 para cargo por coeficiente

     ****** agrego dde mov.dic 12 para cargo con indice copa, dto 386/12, dde 23-05-12.
           IF POS-1-5 = '3COPA'                                           
                  THEN
           IF FECHA-MES < '201206'                                                             
                    THEN MOVE 1 TO MARCA-MASI
                         GO TO CERRAR 
                    ELSE
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
              IF POS-13-A-17 NOT NUMERIC 
               THEN DISPLAY ' sin inform en coefic ' REGISTRO (1) 
                    MOVE ZEROS TO POS-13-A-17N
                    COMPUTE A-HABER (I) =                                       
                              H-HABER * POS-13-A-17N * PORC / 100    
 ********************* saco la cancelada 23-10-15
 ****************   COMPUTE A-HABER (I) = H-HABER * PORC / 000    
                    GO TO CERRAR                
               ELSE
                COMPUTE A-HABER (I) =                                       
                              H-HABER * POS-13-A-17N * PORC / 100    
                  PERFORM VER-TOPE  THRU VER-TOPE-X 
     ****** agrego              PERFORm A TOPE 2016/05                                                      
                   GO TO CERRAR.                     
     ****** agrego dde mov.dic.2012 para cargo por coeficiente

     ****** agrego dde Mayo 14    Resolucion 147                                                  
           IF POS-1-2 = '3R' AND POS-3 = '0'                                          
                  THEN
           IF FECHA-MES < '201406'                                                             
                    THEN MOVE 1 TO MARCA-MASI
                         GO TO CERRAR 
                    ELSE
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
              COMPUTE A-HABER (I) = H-HABER * PORC / 100    
              GO TO CERRAR.               
     ****** agrego dde Mayo 14    Resolucion 147                                                  
                   
     ****** agrego dde AGOS/16    Resolucion 1785/15 ART.3                                        
           IF POS-1-2 = '3R' AND POS-3 = 'X'                                          
                  THEN
           IF FECHA-MES < '201512'                                                             
                    THEN MOVE 1 TO MARCA-MASI
                         GO TO CERRAR 
                    ELSE
              ADD 1 TO I 
              MOVE REG-MAE TO REGISTRO (I) 
              MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
              COMPUTE A-HABER (I) = H-HABER * PORC / 100    
              GO TO CERRAR.               
     ****** agrego dde AGOS/16    Resolucion 1785/15 ART.3                                        


           MOVE ANTIG1 TO ANTIGT.
           IF ANTIG1 > 60
              THEN MOVE 1 TO MARCA-MASI
                   GO TO CERRAR.
     ******  DISPLAY 'sale ' NRO ' ' CODIGO  ' ' MARCA-MASI.
           MOVE 0 TO I.
           MOVE 0 TO BONIFIC.
           MOVE 'NO' TO BON-20.   
     ******  para cargar marca de DGI 17-06-08                 
           IF POS-1-5 = 'J1601' OR 'J0701'
                     THEN MOVE '*' TO MARDGI 
                     ELSE
     ******  para cargar marca de DGI 20-08-09                 
           IF POS-1-A-6 = '32430L'                                                 
     ****  IF POS-1-A-6 = '32430L' AND POS-7 = '0' AND POS-8 = 'A' AND SEC1 = 'H' dde mov set10 tb para '1A' en 7-8
                         THEN MOVE '*' TO MARDGI 
                         ELSE
           IF POS-1-A-6 = '9L1380' AND POS-7 = '7' AND POS-8 = '0'                 
                            THEN MOVE '*' TO MARDGI.
     ******  para cargar marca de DGI 20-08-09                 
      * MASIVOS POR ERROR EN LETRA DE ANIOS O HABER DE INVESTIGADORES
           IF (POS-1-5 = 'J4701' OR 'J4801' OR 'J4901' OR 'J5001'
                 OR 'J5101') AND (POS-6 NOT = 'J' AND NOT = 'A')
             THEN
                  MOVE 1 TO MARCA-MASI
                  GO TO CERRAR
             ELSE
           PERFORM CALCULA-ANTIGUEDAD THRU CALCULA-ANTIGUEDAD-X.
      *
       ALHABER.
           IF  SI-ES-VIAL = 'I'
                      THEN
                   MOVE POS-2-3 TO POS-2-3-ORI   
                   MOVE '24' TO POS-2-3 
                   MOVE ZEROS TO NOMINAL.
           PERFORM CALCULA-HABERES THRU CALCULA-HABERES-X.
           IF POS-1 = '3' AND POS-15 = 'V' AND FECHA-MES > '201101'
                                           AND SI-ES-VIAL = 'I'
                  THEN 
                     PERFORM BON-VIV-1661 THRU BON-VIV-1661-X. 
    ************* mov junio15 agr defensor del pueblo q solo cobra haber y antig.
           IF POS-1-5 = 'JDP01'
              THEN COMPUTE A-HABER (I) = H-HABER * PORC / 100    
                   GO TO CERRAR.               
       CERRAR.
           GOBACK.

       CALCULA-ANTIGUEDAD.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS   TO A-IOMA (I) A-HABER (I) A-ANTIG (I).
           MOVE ZEROS TO IMPOR1 IMPOR2 ANTIG-NEW.
      ****   CALCULO DE ANTIGUEDAD PARA TODOS *******
      ****  IF NRO = '1011314620' THEN
      ****   DISPLAY 'entra ' NRO ' ' ANTIG1 ' ' POS-11-12.

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

      ***** IF NRO = '1011314620' THEN
      ***** DISPLAY ' anti ' NRO ' ' ANTIG1 ' ' POS-11-12
      *****                 ' porc ' PORC  ' ' ANTIG-AUX.

            IF FECHA-MES > '199601'
              THEN IF SI-LETRA-ANTIG NOT = SPACE       
                    THEN
    *******         MOVE ANTIG-AUX TO ANTIG-NEW
                    PERFORM RECALCULA-ANTIG THRU FIN-RECALCULA-ANTIG. 

      ***** IF NRO = '1011314620' THEN
      ***** DISPLAY '  sale anti ' NRO ' ' ANTIG1 ' ' POS-11-12
      *****                 ' porc ' PORC  ' ' ANTIG-AUX.

            COMPUTE ANTIG-AUX = ANTIG-AUX * PORC / 100.
            COMPUTE ANTIG-NEW = ANTIG-NEW * PORC / 100.
            MOVE ANTIG-AUX TO A-ANTIG (I).

     *******IF NRO = '1011314620' THEN
     ***** DISPLAY '  termina anti ' NRO ' ' ANTIG1 ' ' POS-11-12
     ****                   ' porc ' PORC  ' ' ANTIG-AUX.
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
**********  agrego 6-7-15
           IF POS-1 = '3' AND SI-ES-VIAL = 'A'  AND    
            POS-15 = '7' AND POS-1-8-DE-HISCAR = '32130P0A' 
                           THEN
                MOVE 1 TO I
                MOVE REG-MAE TO REGISTRO (I) 
                MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                PERFORM BON-JUNTA1 THRU BON-JUNTA1-X 
                GO TO CALCULA-HABERES-X.  
           IF POS-1 = '3' AND SI-ES-VIAL = 'A'  AND    
            POS-15 = '7' AND POS-1-8-DE-HISCAR = '31730A0A' 
                           THEN
                MOVE 1 TO I
                MOVE REG-MAE TO REGISTRO (I) 
                MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                PERFORM BONELEC-131 THRU BONELEC-131-X 
                GO TO CALCULA-HABERES-X.  
*********
           IF POS-1 = '3' AND SI-ES-VIAL = 'A'     
                   THEN
                MOVE 1 TO I
                MOVE REG-MAE TO REGISTRO (I) 
                MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                   IF (POS-15 = 'G' OR 'I' OR 'J' OR 'M' OR 'P'      
                                    OR 'O' OR 'N')
                        THEN
                        PERFORM BON-URPE   THRU BON-URPE-X 
                        GO TO CALCULA-HABERES-X 
                        ELSE
                        IF POS-15 = 'S'      
                           THEN
                           PERFORM BON-IPS   THRU BON-IPS-X 
                           MOVE ZEROS TO BONIFIC
                           PERFORM BON-IPS-MAY10 THRU BON-IPS10-X 
                           GO TO CALCULA-HABERES-X 
                        ELSE
                        IF (POS-15 = 'Q' OR 'Z' OR '3' OR 'U' OR '1'                
                                OR 'Y' OR '0' OR ' ' OR '2' OR 'E'    
                                OR '4' OR '5' OR '6' OR 'L' OR '8')                  
                         OR (POS-15 = 'W' AND FECHA-MES > '201003')
                         OR (POS-15 = 'X' AND FECHA-MES > '201511')
                           THEN
                           PERFORM BON-ARMA  THRU BON-ARMA-X 
                           GO TO CALCULA-HABERES-X 
                           ELSE
                        IF POS-15 = 'W'      
                           THEN
                           PERFORM BON-TRA   THRU BON-TRA-X 
                           GO TO CALCULA-HABERES-X  
                           ELSE
                        IF POS-15 = '7' AND FECHA-MES > '201101'     
                           THEN
                            PERFORM BON-URPE   THRU BON-URPE-X 
                            GO TO CALCULA-HABERES-X 
                           ELSE
                        IF POS-15 = '7'                                   
                           THEN
                           PERFORM BON-ELEC  THRU BON-ELEC-X 
                           GO TO CALCULA-HABERES-X  
                           ELSE
                           DISPLAY ' VER POS-15 ' REG-MAE.
     ************** desde ppto marzo 08 la A se usa para astillero porq no se usaba
           IF POS-1 = 'A'
              THEN NEXT SENTENCE
              ELSE
           IF POS-1 = 'V' AND POS-1-DE-HISCAR = 'J'
              THEN PERFORM CALCULA-HAB-JERARQ THRU CALCULA-HAB-JERARQ-X
              ELSE
           IF POS-1 = 'V' AND POS-1-DE-HISCAR = '3'
              THEN PERFORM CALCULA-HAB-ADMI THRU CALCULA-HAB-ADMI-X
              ELSE
           IF POS-1 = '3'
              THEN PERFORM CALCULA-HAB-ADMI THRU CALCULA-HAB-ADMI-X
              ELSE
           IF POS-1 = 'X' AND
              (POS-1-5 NOT = 'X2502' AND NOT = 'X2602'
               AND NOT = 'X2702')
              THEN PERFORM CALCULA-HAB-ADMI THRU CALCULA-HAB-ADMI-X
              ELSE
           IF (POS-1-5 = 'J4701' OR 'J4801' OR 'J4901' OR 'J5001'
                           OR 'J5101') OR POS-1-2 = 'JA'
              THEN PERFORM CALCULA-HAB-INVEST THRU CALCULA-HAB-INVEST-X
              ELSE
           IF POS-1 = 'J' OR
                (POS-1-5 = 'X2502' OR 'X2602' OR 'X2702')
              THEN PERFORM CALCULA-HAB-JERARQ THRU CALCULA-HAB-JERARQ-X
              ELSE
                   MOVE 5 TO MARCA-MASI
                   GO TO CERRAR.
       CALCULA-HABERES-X.
            EXIT.
       CALCULA-HAB-ADMI.
           MOVE ZEROS TO BONCONHIS BON-REA.                
           IF SUMAFIJ1 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ1.
           IF SUMAFIJ2 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ2.
           IF SUMAFIJ3 NOT NUMERIC
              THEN MOVE ZEROS TO SUMAFIJ3.
           IF POS-1 = '3'
             THEN IF POS-1-A-6 = '31330C'                           
                 THEN MOVE CODIGO TO CODIGO-AUX
                        PERFORM HAB-CONSEJERO THRU HAB-CONSEJERO-X
                         GO TO CALCULA-HAB-ADMI-X
                 ELSE IF POS-1-A-6 = '3IOMAB' OR '3C001K'                          
                                     OR POS-6 = 'B'
                                     OR POS-2 = 'S'
                    THEN COMPUTE A-HABER (I) = H-HABER * PORC / 100              
                         GO TO CALCULA-HAB-ADMI-X
                    ELSE PERFORM PORC-DIRECTO THRU PORC-DIRECTO-X.
           IF FECHA-MES > '198908' AND FECHA-MES < '199012'
              THEN PERFORM SUMAS-REA THRU SUMAS-REA-X.
           IF POS-15 = 'B'
                 THEN
                IF FECHA-MES < '200604' THEN
              COMPUTE A-HABER (I) = H-HABER * PORC / 100 + BONCONHIS
              GO TO CALCULA-HAB-ADMI-X 
                                        ELSE
              COMPUTE A-HABER (I) = H-HABER * PORC / 100             
              GO TO CALCULA-HAB-ADMI-X.
    ********     ELSE
           IF FECHA-MES > '198906'
    **************************************      SE AGREGA POR ANTIG
                  THEN  IF FECHA-MES < '199602'
                           THEN
           COMPUTE A-HABER (I) = ((H-HABER + SUPL1 + SUPL2 + SUMAFIJ1
                                    + SUMAFIJ2 + SUMAFIJ3 + BON-REA)
                                    * PORC / 100) + BONCONHIS
                           ELSE
                  IF FECHA-MES < '200604' THEN
           COMPUTE A-HABER (I) = ((H-HABER + SUPL1 + SUPL2 + SUMAFIJ1
                                  + SUMAFIJ2 + BON-REA) * PORC / 100)
                                  + BONCONHIS
                                          ELSE
           COMPUTE A-HABER (I) = H-HABER * PORC / 100               
    **************************************      SE AGREGA POR ANTIG
                  ELSE
           COMPUTE A-HABER (I) = ((H-HABER + SUPL1 + SUPL2)
                                    * PORC / 100) + BONCONHIS.
           IF BONCONHIS > ZEROS AND FECHA-MES > '200603'
                      THEN MOVE BONCONHIS TO BONIFIC
                           MOVE '*PORC.DIRECT' TO LEYENDA      
                           MOVE 'SI' TO BON-20 
                           PERFORM CARGAR THRU CARGAR-X.
           COMPUTE NOMINAL = NOMINAL + A-HABER (I). 
           PERFORM BONIF-185 THRU BONIF-185-X.
           PERFORM BONIF-15-DTO518-93 THRU BONIF-15-DTO518-93-X.
           PERFORM BON-DTO100-2007 THRU BON-DTO100-2007-X.
           PERFORM BON-MAR-08-1 THRU BON-MAR-08-1-X. 
           PERFORM BON-MAR-08-2 THRU BON-MAR-08-2-X. 
           PERFORM BON-MAR09   THRU BON-MAR09-X. 
   *********    agrego 25-03-10 ---sacamos GMAEGRAFA
   *********    agrego 5-10-12 --- agrego agrup Y para pers de apoyo grafico
           IF (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y')                             
                  THEN 
                  PERFORM BON-GRAF THRU BON-GRAF-X  
                  PERFORM BON-GRAFENE15 THRU BON-GRAFEN-X  
                  PERFORM BON-GRAF18 THRU BON-GRAF18-X. 
   *********    agrego 25-03-10 ---sacamos GMAEGRAFA
           PERFORM BON-ENERO05 THRU BON-ENERO05-X.
           PERFORM BON-AGOST05 THRU BON-AGOST05-X.
           PERFORM BON-FEBRE06 THRU BON-FEBRE06-X.
           PERFORM BON-MAR2014 THRU BON-MAR2014-X. 
           IF POS-1 = '3' THEN
                PERFORM BON-CHOFER THRU BON-CHOFER-X 
                PERFORM BON-RADIO THRU BON-RADIO-X 
                PERFORM BON-FIS-CON-TES THRU BON-FIS-CON-TES-X 
                PERFORM BON-SOLO-FIS THRU BON-SOLO-FIS-X
                PERFORM BON-DTO207 THRU BON-DTO207-X 
                IF FECHA-MES > '200309' AND POS-15 = 'N'
                     THEN
                      MOVE CODIGO TO CODIGO-AUX            
                      PERFORM BON-12872 THRU BON-12872-X 
                     ELSE
  ************      IF FECHA-MES > '200304' AND POS-15 = 'R'
   ***************         THEN
   ******************     PERFORM BONIF-REG THRU BONIF-REG-X  dde ppto marzo14 dan una nueva bonif seg antiguedad.
   ****************        ELSE
                    IF FECHA-MES > '200712' AND POS-15 = 'L'
                            THEN
                         PERFORM BON-LOTERIA THRU BON-LOTERIA-X 
                           ELSE
   **************** las fechas de vigencia para las 'V' estan en el perform
                         IF POS-15 = 'V'
                            THEN
                         PERFORM BON-VIVIENDA THRU BON-VIVIEN-X 
                         PERFORM BON-VIVIEND2 THRU BON-VIVIEN2-X 
                            ELSE
                    IF FECHA-MES > '200907' AND POS-15 = 'D'
                                THEN
                         PERFORM BON-INFRAEST THRU BON-INFRAE-X 
                                ELSE
      *******        IF FECHA-MES > '201101' AND POS-15 = 'X'por nota pidieron cambio de fecha
      *******        IF FECHA-MES > '200803' AND POS-15 = 'X' pidieron cambio de fecha por reclamos
      *******         dde ppto mar14 agr bon exclus para 'R' en perform.                       
                 IF FECHA-MES > '200703' AND POS-15 = 'X' OR 'Q' OR 'R'
                                THEN
                         PERFORM BON-ECONOMIA THRU BON-ECONOM-X 
                                ELSE
                    IF FECHA-MES > '200808' AND POS-15 = '3'
                                THEN
                         PERFORM BON-TUTELA THRU BON-TUTELA-X 
                                ELSE
              IF FECHA-MES > '201007' AND POS-15 = '1' AND POS-7 = 'C'
                                THEN
                         PERFORM BON-ESCRIB THRU BON-ESCRIB-X 
                         PERFORM BON-ESCRIBBON THRU BON-ESCRIBB-X.
    ***********
           IF POS-1 = 'X' THEN                
                PERFORM BON-DTO207X THRU BON-DTO207X-X  
           IF POS-1 = 'V' THEN
                  PERFORM BON-DTO207 THRU BON-DTO207-X. 
    **** agrego apertura de bon.disponib.y funcion para 3 y equiv.
           IF  POS-1-DE-HISCAR = '3' OR 
               (POS-1-3 = 'X17' OR 'X19' OR 'X21' OR 'X22'
                         OR 'X23' OR 'X24')
               THEN PERFORM BON-3 THRU BON-3-X.
           IF FECHA-MES < '199808'
              THEN PERFORM BONIF-20 THRU BONIF-20-X.
    *********     Boni ruralidad para educacion, agrego desde ppto mar08 a pedido, y jornada cta.  
           IF FECHA-MES > '200803'
                 THEN IF POS-1 = '3' AND POS-15 = 'E'
                      THEN PERFORM BON-RURAL THRU BON-RURAL-X.
    *********     Bonis para salud
           IF FECHA-MES > '200703'
                 THEN IF POS-1 = '3' AND POS-15 = 'Z'
                       THEN PERFORM BON-ENF-PSIC-DESF THRU BON-ENF-X.
           IF FECHA-MES > '200808'
                 THEN IF POS-1 = '3' AND POS-15 = 'Z'
                      THEN PERFORM BON-HTAL THRU BON-HTAL-X.
    *********     Boni para auxiliares docentes dde marzo12, antes en call.
           IF FECHA-MES > '201203'
                 THEN IF POS-1 = '3' AND POS-15 = 'E'
                      THEN PERFORM BON-AUXD THRU BON-AUXD-X 
                           PERFORM BON-AUXD15 THRU BON-AUXD15-X.
      *       para delegados 31-08-10, de ioma, es dde ene06 pero cargo 2 anios atras.
           IF POS-1 = '3'                             
            THEN    
            IF POS-8 = 'D' AND POS-15 = 'I' AND FECHA-MES > '200808' 
                        THEN
                 MOVE ZEROS TO TOPEDEL DELEGADO
                 PERFORM SUMA-DELEGADO THRU SUMA-DELEGADO-X 
                    VARYING K FROM 1 BY 1 UNTIL K > I     
                 PERFORM BON-DELEGADO    THRU BON-DELEGADO-X  
      *       para delegados 17-2-11, del registro de las personas, es dde mar08, no pago reaj.              
                        ELSE
      **** IF (POS-8 = 'D' OR 'R' OR 'B' OR 'C') AND POS-15 = 'G'  por expte piden el cambio              
           IF (POS-8 = 'D' OR 'R' OR 'B' OR 'C') AND POS-15 = 'J'                          
                                        AND FECHA-MES > '200803' 
                        THEN
                 MOVE ZEROS TO TOPEDEL DELEGADO
                 PERFORM SUMA-DELEGADO THRU SUMA-DELEGADO-X 
                    VARYING K FROM 1 BY 1 UNTIL K > I     
                 PERFORM BON-DELEGADO    THRU BON-DELEGADO-X. 
    *********     Bonis para IPS    
           IF FECHA-MES > '201208'
                 THEN IF POS-1 = '3' AND POS-15 = 'S'
                      THEN PERFORM BON-IPSAGO12 THRU BON-IPSAGO12-X  
                           PERFORM BON-IPSJUL15 THRU BON-IPSJUL15-X. 
           IF FECHA-MES > '201209'
                 THEN IF POS-1 = '3' AND POS-15 = 'I'
                      THEN PERFORM BON-IOMASET12 THRU BON-IOMA12-X. 
           IF FECHA-MES > '201403'
                THEN IF (POS-1 = '3' OR 'X') AND (POS-2-3 = '21' OR '22'                 
                         OR '23' OR '24') AND POS-6 = 'J'
                      THEN PERFORM BON-RESPESTR  THRU BON-RESP-X. 
    **************************** mov dic14, ppto mar07 creo cod bonfic para scria de la ninez mrio de desarr.
    ****** IF (POS-8 = 'P' OR 'M' OR 'D' OR 'L' OR 'N') AND POS-15 = 'Y' en mov mar15 se hacen cambios pedidos por el 3ro.
           IF (POS-8 = 'M' OR 'D' OR 'N' OR 'P' OR 'L' OR 'Q')                                           
                              AND POS-15 = 'Y'                          
                              AND POS-1 = '3'  AND FECHA-MES > '200703'    
                        THEN
                 PERFORM BON-NINEZ  THRU BON-NINEZ-X. 
    **************************** mov jun14, ppto mar08 creo cod consej cat 1 con difer a categ 21.
    ****** IF FECHA-MES > '200803' mov oct14, cambio vigencia 
           IF FECHA-MES > '199801'
                 THEN IF POS-1-A-6 = '30130C'                           
                   THEN MOVE CODIGO TO CODIGO-AUX
                        PERFORM CONSEJERO THRU CONSEJERO-X.
    **************************** mov jun14, agrego sf para leyes especiales.                       
           IF FECHA-MES > '201406'
                 THEN IF POS-1-A-6 = '32430L'                           
                   THEN MOVE CODIGO TO CODIGO-AUX
                        PERFORM BON-LEYES THRU BON-LEYES-X.
    **************************** mov MAR15, agrego BON.PERM2015                                    
           IF FECHA-MES > '201501'
                 THEN IF POS-1 = '3'  AND                          
                     ((POS-2-3 = '15' AND POS-6 = 'S') OR
                      (POS-2-3 = '16' AND POS-6 = 'O') OR
                      (POS-2-3 = '17' AND POS-6 = 'A') OR
                      (POS-2-3 = '18' AND POS-6 = 'T') OR
                      (POS-2-3 = '21' AND POS-6 = 'P') OR
                      (POS-2-3 = '24' AND POS-6 = 'J') OR
                      (POS-2-3 = '24' AND POS-6 = 'N'))     
                   THEN PERFORM BON-PERM15 THRU BON-PERM15-X.
    **************************** mov Set16, agrego BON.TRAB2016 dto 678/16.                                    
           IF FECHA-MES > '201609'
                 THEN IF POS-1 = '3' AND POS-15 = 'W'                          
                   THEN PERFORM BON-TRAB16 THRU BON-TRAB16-X.

       CALCULA-HAB-ADMI-X.
            EXIT.

     ****** dde ppto mar08,mov nov09 agrego calculo para pers de apoyo
       CALCULA-HAB-INVEST.
           MOVE ZEROS TO BONCONHIS.
           IF POS-6 = 'A'
             THEN COMPUTE A-ANTIG (I) = H-HABER * PORC / 100
             ELSE PERFORM PORC-DIRECTO THRU PORC-DIRECTO-X
                  COMPUTE A-HABER (I) = ((H-HABER + SUPL1) * PORC
                                          / 100) + BONCONHIS
                  IF POS-2 NOT = 'A' THEN
                    PERFORM BONIF-JERARQ THRU BONIF-JERARQ-X 
                    PERFORM BONIF-DTO202 THRU BONIF-DTO202-X 
                    PERFORM BON-DTO100 THRU BON-DTO100-X 
                    PERFORM BON-AGOST05 THRU BON-AGOST05-X 
                    PERFORM BON-FEBRE06 THRU BON-FEBRE06-X 
                    PERFORM BON-ENERO05 THRU BON-ENERO05-X 
                    PERFORM BON-MAR-08-1 THRU BON-MAR-08-1-X 
  ********** la otra bonif de mar08 no les toca porq es hasta cat 20
  ********** y dde agosto08 es hasta cat 24                   
  ********** la bonif de mar09 no la pago                     
                    PERFORM BONIF-20 THRU BONIF-20-X 
                                     ELSE
                    PERFORM BON-AGOST05 THRU BON-AGOST05-X 
                    PERFORM BON-MAR-08-1 THRU BON-MAR-08-1-X 
                    PERFORM BON-CIC-TEC  THRU BON-CIC-TEC-X.
                    PERFORM BON-INV-D808086 THRU BON-INV-X.
       CALCULA-HAB-INVEST-X.
            EXIT.

      ******* SEPARO GASTOS FCIONALES DE BêSICO DESDE PPTO.9811
       CALCULA-HAB-JERARQ.
           IF POS-1 = 'J' AND SI-ES-VIAL = 'A'  AND POS-15 = 'Q'
                   THEN
                MOVE 1 TO I
                MOVE REG-MAE TO REGISTRO (I) 
                MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                PERFORM BON-ARBAJ  THRU BON-ARBAJ-X
                GO TO CALCULA-HAB-JERARQ-X.                                
           IF FECHA-MES < '199811'
              THEN MOVE ZEROS TO SUMAFIJ2.
           MOVE ZEROS TO BONCONHIS BON-REA.
           IF POS-1 = 'J'
             THEN PERFORM PORC-DIRECTO THRU PORC-DIRECTO-X.
           IF FECHA-MES > '199312'
             THEN  PERFORM SUMAS-REA-J THRU SUMAS-REA-J-X.

           COMPUTE A-HABER (I) = ((H-HABER + SUPL1 + BON-REA
                     + SUMAFIJ2) * PORC / 100) + BONCONHIS.
           IF FECHA-MES < '200601'
              THEN NEXT SENTENCE
              ELSE
                   PERFORM APERTURA1 THRU APERTURA1-X 
                   GO TO VER-FISCALIA.

      ******** CêLCULO DE REMUNERACIÏN MINIMA PARA J Y X JERARQ. ***
                   COMPUTE HAB-AUX =  (H-HABER + SUMAFIJ2 + SUPL1)
                                 + ((A-ANTIG(I) + IMPOR2) * 100 / PORC).
                   PERFORM CALC-DIFREMMIN THRU CALC-DIFREMMIN-X.
       VER-FISCALIA.
           IF POS-1 = 'J'
             THEN PERFORM BON-FIS-CON-TES THRU BON-FIS-CON-TES-X 
                  PERFORM BON-SOLO-FIS THRU BON-SOLO-FIS-X
                  IF FECHA-MES > '200309'
                     THEN
                  IF POS-15 = 'N'
                      THEN MOVE CODIGO TO CODIGO-AUX            
                           PERFORM BON-12872 THRU BON-12872-X.
    ***********   las fechas de vigencias enstan en el perform
           IF POS-1 = 'J' AND POS-15 = 'V'
                            THEN
                  PERFORM BON-VIVIENDA THRU BON-VIVIEN-X
                  PERFORM BON-VIVIEND2 THRU BON-VIVIEN2-X.
           PERFORM BONIF-JERARQ THRU BONIF-JERARQ-X.
           PERFORM BONIF-15-DTO518-93 THRU BONIF-15-DTO518-93-X.
     ******* DEJO LOS 15 $ DTO.518-93 PARA QUE NO BAJE HABERES DE 9801
           PERFORM BONIF-15-DTO491-98 THRU BONIF-15-DTO491-98-X.
           IF POS-15 = 'F'                  
                          AND (POS-1-5 = 'J0102' OR 'J0602' OR
                 'J1802' OR 'J1902' OR 'J2002' OR 'J2202' OR 'J2302'
                OR 'J2402' OR 'J2502' OR 'J2702' OR 'J2602' )
                         THEN
                  PERFORM BON-DTO207J THRU BON-DTO207J-X
                  PERFORM BON-DTO100-2007 THRU BON-DTO100-2007-X 
                  PERFORM BON-ENERO05 THRU BON-ENERO05-X 
                  PERFORM BON-AGOST05 THRU BON-AGOST05-X 
                  PERFORM BON-FEBRE06 THRU BON-FEBRE06-X 
                  PERFORM BON-MAR-08-1 THRU BON-MAR-08-1-X 
  ********** la otra bonif de mar08 no les toca porq es hasta cat 20
  ********** la  bonif de mar09 no les toca porq es hasta cat 24
                  PERFORM BON-MAR2014  THRU BON-MAR2014-X.
           PERFORM BONIF-20 THRU BONIF-20-X.
           PERFORM TITULO THRU TITULO-X.
           IF POS-1-5 = 'J3502'                              
                    THEN PERFORM BON-GRAF THRU BON-GRAF-X.
           IF POS-1-5 = 'J5302' OR 'J5402' OR 'J5502' OR 'J5602'                              
                    THEN PERFORM BON-CONSESC THRU BON-CONSESC-X.
    *********     Bonis para IPS    
           IF FECHA-MES > '201208'
                 THEN IF POS-1 = 'J' AND POS-15 = 'S'
                      THEN PERFORM BON-IPSAGO12 THRU BON-IPSAGO12-X  
                           PERFORM BON-IPSJUL15 THRU BON-IPSJUL15-X. 
           IF FECHA-MES > '201209'
                 THEN IF POS-1 = 'J' AND POS-15 = 'I'
                      THEN PERFORM BON-IOMASET12 THRU BON-IOMA12-X. 
           IF FECHA-MES > '201403'
             THEN IF POS-1-5 = 'J1501' OR 'J1901' OR                                                 
                    (POS-1-5 = 'J2502' AND POS-15 = '1')
                    THEN PERFORM BON-ESCRIBBON THRU BON-ESCRIBB-X.
           IF FECHA-MES > '201609'
             THEN IF POS-1 = 'J' AND POS-15 = 'W'                                              
                    THEN PERFORM BON-TRAB16 THRU BON-TRAB16-X.
           IF FECHA-MES > '201609' AND POS-15 = 'R' AND
                     (POS-1-5 = 'J2502' OR 'J2602' OR 'J2702')
                                THEN
                         PERFORM BON-ECONOMIA THRU BON-ECONOM-X.

       CALCULA-HAB-JERARQ-X.
           EXIT.
       APERTURA1.
           COMPUTE A-HABER (I) = H-HABER * PORC / 100.          
           COMPUTE HAB-AUX = (A-HABER (I) + A-ANTIG (I)) * 100 / PORC.
           IF SUMAFIJ3  > ZEROS
              THEN IF BONCONHIS > ZEROS
                   THEN ADD 1 TO I 
                   MOVE REG-MAE TO REGISTRO (I) 
                   MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                   MOVE '02' TO A-CONC (I)                                   
                   MOVE '*PORC.DIRECT' TO A-CODIGO (I) 
                   MOVE BONCONHIS TO A-HABER (I). 
 **********        COMPUTE A-HABER (I) = SUMAFIJ3 * POS-N-13-14 / 100.           
  *********        COMPUTE HAB-AUX = HAB-AUX + A-HABER (I).   
           IF SUMAFIJ1 > ZEROS
              THEN ADD 1 TO I 
                   MOVE REG-MAE TO REGISTRO (I) 
                   MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                   MOVE '02' TO A-CONC (I)                                   
                   COMPUTE A-HABER (I) = SUMAFIJ1 * PORC / 100           
                   COMPUTE HAB-AUX = HAB-AUX + SUMAFIJ1    
                   MOVE '*GASTOS REPR' TO A-CODIGO (I). 
           IF SUPL1 > ZEROS
              THEN ADD 1 TO I 
                   MOVE REG-MAE TO REGISTRO (I) 
                   MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                   MOVE '02' TO A-CONC (I)                                   
                   COMPUTE A-HABER (I) = SUPL1 * PORC / 100           
   ***********     COMPUTE HAB-AUX = HAB-AUX + SUPL1 lo agrego para prueba       
                   COMPUTE HAB-AUX = HAB-AUX + SUPL1       
                   MOVE '*DISPONIB.PERM' TO A-CODIGO (I). 
           PERFORM CALC-DIFREMMIN THRU CALC-DIFREMMIN-X.
       APERTURA1-X.
            EXIT.
       CALC-DIFREMMIN.
           IF (POS-1 = 'J'  AND POS-15 NOT = 'B') OR POS-1 = 'V' OR
               (POS-1-5 = 'X2502' OR 'X2602' OR 'X2702')
             THEN
               IF HAB-AUX < SUPL2           
                      THEN
                   COMPUTE DIFREMMIN = SUPL2 - HAB-AUX
                   MOVE DIFREMMIN TO BONIFIC
                   MOVE '*BON.DIF.18%' TO LEYENDA
                   PERFORM CARGAR THRU CARGAR-X.
       CALC-DIFREMMIN-X.
            EXIT.
       PORC-DIRECTO.
           IF POS-13-14 NUMERIC AND POS-13-14 > '00'
                  THEN
             IF POS-1 = 'J' THEN
               IF FECHA-MES < '200601' THEN          
                  COMPUTE BONCONHIS = H-HABER * POS-N-13-14 / 100 
                                       ELSE
                  COMPUTE BONCONHIS = SUMAFIJ3 * POS-N-13-14 / 100 
                            ELSE
             IF POS-1 = '3' THEN
               IF FECHA-MES < '200604' THEN          
                  COMPUTE BONCONHIS = H-HABER * POS-N-13-14 / 100 
                                       ELSE
                  COMPUTE BONCONHIS = SUMAFIJ3 * POS-N-13-14 / 100. 
       PORC-DIRECTO-X.
            EXIT.
       BON-12872.                          
            IF POS-17-20-A NOT NUMERIC 
               THEN DISPLAY ' sin informacion ' REGISTRO (1) 
    *****           GO TO BON-12872-X.
               ELSE
              COMPUTE BONIFIC = H-HABER * POS-N-17-20A * PORC / 100.   
            IF A-NRO (1) = '1083835820'
               THEN COMPUTE BONIFIC = H-HABER * 8.16 * PORC / 100
               ELSE                       
            IF A-NRO (1) = '8046338040'
               THEN COMPUTE BONIFIC = H-HABER * 4.17 * PORC / 100.
    ******     ELSE GO TO BON-12872-X.
            MOVE 'SI' TO BON-20.
            MOVE '*BON.L 12872    ' TO LEYENDA.
            PERFORM CARGAR THRU CARGAR-X.
       BON-12872-X.                                
            EXIT.
       SUMAS-REA.
      ***  CALCULO DE BONIFICACIONES ANTIGUAS, QUE NO SE CONSULTAN
      ***  EN LA HISCAR Y QUE SE SUMAN AL HABER.
           MOVE ZEROS TO BON-REA.
           IF FECHA-MES < '198911'
             THEN MOVE 100.63 TO BON-REA
             ELSE
           IF FECHA-MES < '199001'
             THEN MOVE 175.32 TO BON-REA
             ELSE
           IF FECHA-MES = '199002'
             THEN MOVE 700.00 TO BON-REA
             ELSE
           IF FECHA-MES = '199004'
             THEN MOVE 1000.00 TO BON-REA
             ELSE
           IF FECHA-MES NOT > '199010'
             THEN
              IF POS-4-5 NOT > '30'
                THEN MOVE 1000.00 TO BON-REA
                ELSE COMPUTE BON-REA = 1000.00 * POS-N-4-5 / 30
             ELSE
           IF FECHA-MES = '199011'
             THEN
              IF POS-4-5 NOT > '30'
                THEN MOVE  500.00 TO BON-REA
                ELSE COMPUTE BON-REA =  500.00 * POS-N-4-5 / 30.
       SUMAS-REA-X.
           EXIT.
       SUMAS-REA-J.
           MOVE ZEROS TO BON-REA.
      ***************  VER SI LES CORRESPONDE ALGUNA
       SUMAS-REA-J-X.
           EXIT.
       BONIF-185.
           IF FECHA-MES > '199807'
                THEN  GO TO BONIF-185-X.
      *
           IF POS-1 = '3'
              THEN IF POS-15 = 'B'
                      THEN GO TO BONIF-185-X.
           IF FECHA-MES > '199009'
                THEN
                 IF FECHA-MES < '199104'
                   THEN MOVE 1500.00 TO BONIFIC
                        MOVE '*BON.NO REM.' TO LEYENDA
                        PERFORM CARGAR THRU CARGAR-X
                   ELSE
                        MOVE '*BON.REMUNE.' TO LEYENDA
                        IF FECHA-MES < '199204'
                          THEN MOVE 1850.00 TO BONIFIC
                               PERFORM CARGAR THRU CARGAR-X
                          ELSE MOVE 18.50 TO BONIFIC
                               PERFORM CARGAR THRU CARGAR-X.
       BONIF-185-X.              
           EXIT.

     ********** esta bonif no se genera mas aca, pasa al gehi con cod propios 3REG     *****
     ********** esta bonif no se genera mas aca, pasa al gehi con cod propios **************
       BONIF-REG.
     ********** dde ppto agosto 08 agrego restriccion de horas y agrup J.oct08
     ********** dde ppto agosto 08 dejo restriccion de agrup J como en res.original.nov08
     ********** dde ppto agosto 08 mov dic 08 repongo a la jubil con cargo 31730J por expte.
           IF POS-1-A-6 = '31730J' 
                   THEN NEXT SENTENCE 
                   ELSE
           IF POS-6 = 'J'
                      THEN GO TO BONIF-REG-X 
                      ELSE
           IF POS-1-3 > '300' AND POS-1-3 < '322'
                      THEN NEXT SENTENCE 
                      ELSE GO TO BONIF-REG-X.
            IF FECHA-MES > '201303'
               THEN  
                IF POS-4-5 = '40'                        
                      THEN MOVE 4294.87 TO BONIFIC
                      ELSE
                       IF POS-4-5 = '30'                        
                        THEN MOVE 3098.55 TO BONIFIC 
                        ELSE MOVE 1924.97 TO BONIFIC 
               ELSE
            IF FECHA-MES > '201203'
               THEN  
                IF POS-4-5 = '40'                        
                      THEN MOVE 3907.98 TO BONIFIC
                      ELSE
                       IF POS-4-5 = '30'                        
                        THEN MOVE 2819.43 TO BONIFIC 
                        ELSE MOVE 1924.97 TO BONIFIC 
               ELSE
            IF FECHA-MES > '201103'
               THEN  
                IF POS-4-5 = '40'                        
                      THEN MOVE 3262.18 TO BONIFIC
                      ELSE
                       IF POS-4-5 = '30'                        
                        THEN MOVE 2354.24 TO BONIFIC 
                        ELSE MOVE 1924.97 TO BONIFIC 
               ELSE
            IF FECHA-MES > '201007'
               THEN  
                  MOVE 1924.97 TO BONIFIC 
               ELSE
            IF FECHA-MES > '201003'
               THEN  
                  MOVE 1805.15 TO BONIFIC 
               ELSE
            IF FECHA-MES > '200903'
               THEN  
                  MOVE 1685.32 TO BONIFIC 
               ELSE
            IF FECHA-MES > '200808'
               THEN  
                  MOVE 1555.30 TO BONIFIC 
               ELSE
            IF FECHA-MES > '200803'
               THEN  
                  MOVE 1381.25 TO BONIFIC 
               ELSE
                  IF FECHA-MES > '200710'
               THEN  
                  MOVE 1170.55 TO BONIFIC 
               ELSE
                  IF FECHA-MES > '200703'
               THEN  
                  MOVE 1095   TO BONIFIC  
               ELSE
                  IF FECHA-MES > '200603'
               THEN  
                  MOVE 952.00 TO BONIFIC  
               ELSE
                  IF FECHA-MES > '200510'
               THEN  
                  MOVE  825.00 TO BONIFIC  
               ELSE
                  IF FECHA-MES > '200508'
               THEN  
                  MOVE  705.00 TO BONIFIC  
               ELSE
                  MOVE  500    TO BONIFIC. 

            MOVE '*BON.REGISTRO' TO LEYENDA.
            PERFORM CARGAR THRU CARGAR-X.
       BONIF-REG-X.           
           EXIT.

     **************************************   ESTA NO SE ACTUALIZA MAS ACA PQ ESTA DENTRO DE LA bon-economia ********
     ********* cargo los valores para 30 h.   ESTA NO SE ACTUALIZA MAS PQ ESTA DENTRO DE LA bon-economia ********
       BONIF-REG14.           
           IF POS-4-5 = '30' OR '40' THEN NEXT SENTENCE
                                     ELSE GO TO BONIF-REG14-X.
           IF FECHA-MES > '201403'                         
             THEN
           IF POS-9-10 < '03' THEN MOVE 750  TO BONIFIC
                              ELSE
           IF POS-9-10 < '05' THEN MOVE 900  TO BONIFIC
                              ELSE
           IF POS-9-10 < '10' THEN MOVE 900  TO BONIFIC
                              ELSE
           IF POS-9-10 < '15' THEN MOVE 975  TO BONIFIC
                              ELSE
           IF POS-9-10 < '20' THEN MOVE 1088 TO BONIFIC
                              ELSE
           IF POS-9-10 < '25' THEN MOVE 1125 TO BONIFIC
                              ELSE
           IF POS-9-10 < '30' THEN MOVE 1275 TO BONIFIC
                              ELSE MOVE 1313 TO BONIFIC.
           IF FECHA-MES > '201408'                         
             THEN
           IF POS-9-10 < '03' THEN MOVE 750  TO BONIFIC
                              ELSE
           IF POS-9-10 < '05' THEN MOVE 975  TO BONIFIC
                              ELSE
           IF POS-9-10 < '10' THEN MOVE 1240 TO BONIFIC
                              ELSE
           IF POS-9-10 < '15' THEN MOVE 1315 TO BONIFIC
                              ELSE
           IF POS-9-10 < '20' THEN MOVE 1425 TO BONIFIC
                              ELSE
           IF POS-9-10 < '25' THEN MOVE 1500 TO BONIFIC
                              ELSE
           IF POS-9-10 < '30' THEN MOVE 1575 TO BONIFIC
                              ELSE MOVE 1725 TO BONIFIC.
           IF POS-4-5 = '40' THEN COMPUTE BONIFIC = BONIFIC * 40 / 30.
           MOVE '*BON.REG.2014' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONIF-REG14-X.      EXIT.          
     *************** la de arriba no se usa pq esta en el perf bon-economia. *********************
     *************** la de arriba no se usa pq esta en el perf bon-economia. *********************

       BONIF-15-DTO518-93.
           IF FECHA-MES > '199204' AND FECHA-MES < '199802'
              THEN
                IF POS-1 = 'V'
                            THEN MOVE 15.00 TO BONIFIC
                                 MOVE '*BON.DTO.518/93' TO LEYENDA
                                 PERFORM CARGAR THRU CARGAR-X
                            ELSE
                IF POS-1 = '3'
                    THEN IF CONC = '01' OR
                        (CONC = '02' AND POS-15 NOT = 'B')
                            THEN MOVE 15.00 TO BONIFIC
                                 MOVE '*BON.DTO.518/93' TO LEYENDA
                                 PERFORM CARGAR THRU CARGAR-X
                             ELSE NEXT SENTENCE
                     ELSE
                   IF (POS-1 = 'J' AND POS-15 NOT = 'B')
                         THEN
                     MOVE 15.00 TO BONIFIC
                     MOVE '*BON.DTO.518/93' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X
                         ELSE
                   IF POS-1 = 'X' THEN
                     MOVE 15.00 TO BONIFIC
                     MOVE '*BON.DTO.518/93' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X.
       BONIF-15-DTO518-93-X.
                EXIT.
       BONIF-15-DTO491-98.
           IF FECHA-MES < '199802' THEN GO TO BONIF-15-DTO491-98-X.
      **
           IF FECHA-MES < '200002'
             THEN
              IF (POS-1 = 'J' AND POS-15 NOT = 'B') OR POS-1 = 'X'
                                     OR POS-1-DE-HISCAR = 'J'
                  THEN MOVE 15.00 TO BONIFIC
                       MOVE '*BON.DTO.491/98' TO LEYENDA
                       PERFORM CARGAR THRU CARGAR-X
                  ELSE NEXT SENTENCE
             ELSE
     *****************  desde ppto 200001 saco al JJS y JSJ pq aumentan gtos de rep.
     ***********  desde ppto 200903 saco J4201 y J0402 pq no hay cod, y J2001 pq aplico equiv.
                IF FECHA-MES < '200904' THEN
           IF POS-1-A-6 = 'J2001J' OR 'J4201J' OR 'J0402J'
                       OR 'J0202J' 
                  THEN MOVE 15.00 TO BONIFIC
                       MOVE '*BON.DTO.491/98' TO LEYENDA
                       PERFORM CARGAR THRU CARGAR-X 
                                        ELSE
           IF POS-1-A-6 = 'J0202J'                           
                  THEN MOVE 15.00 TO BONIFIC
                       MOVE '*BON.DTO.491/98' TO LEYENDA
                       PERFORM CARGAR THRU CARGAR-X.
       BONIF-15-DTO491-98-X.           
           EXIT.

    ************* BON dto 3464 para organis de la constitucion. DDE ENE15 aplico aum de v.mod, antes hubo <> aumentos.
    ************* MOV JUL15: agr esta boni para jerarq de la h.junta electoral, ppto abril12, la otra la agr en mov oct15.
       BON-FIS-CON-TES.
           IF FECHA-MES < '199210' THEN GO TO BON-FIS-CON-TES-X.
      ********   MOVIM.ENERO2001 ACT.BON.PARA TODOS J Y REAJ.PARA
      **      A,C,T,ALGUNOS F,H POR EL ANIO 2000 Y 3..N NOV.2000
           IF ( POS-1 = 'J' AND (POS-15 = 'F' OR  'C'      
                         OR  'T' OR  'H'  OR  'A'))    
              OR ( POS-1 = '3' AND ( POS-15 = 'F' OR 'A' OR 'T' 
                      OR 'C' OR 'H' ))
                      THEN NEXT SENTENCE
                      ELSE
           IF (POS-1-5 = 'J7601' OR 'J7701') OR                              
              ((POS-1-5 = 'J2502' OR 'J2702') AND POS-15 = '7')                                         
              THEN IF FECHA-MES > '201204'
                      THEN GO TO HAGA
                      ELSE GO TO BON-FIS-CON-TES-X 
              ELSE GO TO BON-FIS-CON-TES-X.
      ***
      ***  MODIFICADO EL 07-05-96. SE AGREGAN CARGOS Y FECHAS PARA FISC.
      ***  MOV.ENERO'2000 SE AGREGA J5301 PARA CONTADURIA
       SALTO-POS-1.
             IF POS-1-5  = 'J1801'
              THEN
                IF FECHA-MES > '199312'
                   THEN GO TO HAGA
                   ELSE GO TO BON-FIS-CON-TES-X.
             IF POS-1-5 = 'J1701'
              THEN
                IF FECHA-MES > '199501'
                   THEN GO TO HAGA
                   ELSE GO TO BON-FIS-CON-TES-X.
             IF POS-1-5 = 'J5401' OR 'J5501'
              THEN
                IF FECHA-MES > '199412'
                   THEN GO TO HAGA
                   ELSE GO TO BON-FIS-CON-TES-X.
              IF  POS-15 = 'C'
                  THEN IF POS-1-5 = 'J5301'  
                       THEN
                       IF FECHA-MES > '199909' 
      **** AND FECHA-MES < '200002'
                          THEN GO TO HAGA
                          ELSE GO TO BON-FIS-CON-TES-X 
                          ELSE
                       IF POS-1 = 'J'
                          THEN
                       IF FECHA-MES > '199910'
      *** AND FECHA-MES < '200002'
                              THEN GO TO HAGA
                              ELSE GO TO BON-FIS-CON-TES-X.
           IF POS-15 = 'F'
              THEN IF FECHA-MES > '199209'
                      THEN GO TO HAGA.
           IF POS-15 = 'C' OR 'T' OR 'A' OR 'H'
                THEN IF FECHA-MES > '199910'
                   THEN GO TO HAGA
                   ELSE GO TO BON-FIS-CON-TES-X
                ELSE GO TO BON-FIS-CON-TES-X.
       HAGA.
           PERFORM ARMACODI THRU ARMACODI-X.
           IF BONI-FIS > 0
              THEN MOVE BONI-FIS TO BONIFIC
                   PERFORM CARGAR THRU CARGAR-X.
       BON-FIS-CON-TES-X.
           EXIT.
       ARMACODI.
           MOVE CODIGO TO CODIGO-AUXILIAR-X.
           IF POS-1 = '3'
               THEN MOVE '3' TO POS-1-AUX
                    MOVE 'XX' TO POS-4-5-X
                    IF POS-6 = 'A' OR 'T' OR 'S' OR 'O'
                        THEN MOVE 'X' TO POS-6-X 
                        ELSE NEXT SENTENCE
               ELSE MOVE 'X' TO POS-6-X.
           MOVE 0 TO BONI-FIS.
           IF FECHA-MES > '201204'
             THEN PERFORM BUS-EN-TAB THRU BUS-EN-TAB-X
                   VARYING IND-F FROM 1 BY 1 UNTIL IND-F > 92 
             ELSE PERFORM BUS-EN-TAB THRU BUS-EN-TAB-X
                   VARYING IND-F FROM 1 BY 1 UNTIL IND-F > 76.
       ARMACODI-X.
            EXIT.
       BUS-EN-TAB.
            IF POS-1-6-X = CARGO-F (IND-F)
              THEN NEXT SENTENCE                   
              ELSE GO TO BUS-EN-TAB-X.             
     ****   IF POS-1 = '3' AND (POS-6 = 'A' OR 'T' OR 'S' OR 'O')
     ****       THEN GO TO DESDE-ABR12.                 
           IF (POS-1-5 = 'J7601' OR 'J7701') OR ((POS-1-5 = 'J2502'
              OR 'J2702') AND POS-15 = '7')
                THEN GO TO DESDE-ABR12.                 
            IF FECHA-MES > '201403'
                THEN GO TO DESDE-MAR14.                 
            IF FECHA-MES > '201204'
                THEN GO TO DESDE-ABR12.                 
            IF FECHA-MES < '200808'
                THEN MOVE MONTO-F (IND-F) TO BONI-FIS 
                     GO TO SIGOA77.
            IF FECHA-MES > '200807'
                THEN COMPUTE BONI-FIS = MONTO-F (IND-F) * 1.1 
                      IF FECHA-MES > '200903'
                THEN COMPUTE BONI-FIS = BONI-FIS * 1.0374229167      
                        IF FECHA-MES > '201003'
                THEN COMPUTE BONI-FIS = BONI-FIS * 1.0711351351  
                      IF FECHA-MES > '201007'
                THEN COMPUTE BONI-FIS = BONI-FIS * 1.066411
                      IF FECHA-MES > '201101' AND
                  (POS-1-A-6 = 'J1701J' OR 'J1801J')
                  THEN COMPUTE BONI-FIS = 3780.                   
             IF FECHA-MES > '201103'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.2603.     
             IF FECHA-MES = '201204'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.1976.     
       DESDE-ABR12.                 
             IF FECHA-MES > '201204'
                  THEN COMPUTE BONI-FIS = MONTO-F2012 (IND-F).   
             IF FECHA-MES > '201303'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.099.      
             IF FECHA-MES > '201309'
               THEN IF POS-1-3 = '323'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.0722      
                  ELSE COMPUTE BONI-FIS = BONI-FIS * 1.0825.     
             IF FECHA-MES > '201310'                 
              THEN IF POS-1 = '3'                            
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.4195      
                  ELSE COMPUTE BONI-FIS = BONI-FIS * 1.18.       
    *****         THEN COMPUTE BONI-FIS = BONI-FIS * 1.53675 vino primero oct13 y desp set13,por eso antes dio un % mayor.
    *****         ELSE COMPUTE BONI-FIS = BONI-FIS * 1.2774.     
       DESDE-MAR14.                 
             IF FECHA-MES > '201403'
                  THEN COMPUTE BONI-FIS = MONTO-F2014 (IND-F).   
             IF FECHA-MES > '201405'
                  THEN COMPUTE BONI-FIS = MONTO-F22014 (IND-F).   
             IF FECHA-MES > '201501'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.07.           
             IF FECHA-MES > '201503'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.103701.       
             IF FECHA-MES > '201508'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.0822.         
             IF FECHA-MES > '201601'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.051.          
             IF FECHA-MES > '201603'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.09473923.     
             IF FECHA-MES > '201607'
                  THEN COMPUTE BONI-FIS = BONI-FIS * 1.13844.      
       SIGOA77.
            MOVE 97 TO IND-F.
            IF POS-15 = 'H'
                      THEN MOVE '*RESOL.104/99' TO LEYENDA
                      ELSE MOVE '*BON DTO.3464/93' TO LEYENDA.
       BUS-EN-TAB-X.
           EXIT.
       BONIF-JERARQ.
     ***********   desde ppto 200001 saco al JJS y JSJ 
     ***********   porque aumentan los gastos de repres.
           IF POS-1-A-6 = 'J0202J'                               
               THEN GO TO SALTO-FECHA.
     ****                OR 'J4201J' OR 'J0402J'saco dde ppto mar09 pq no hay cod
     ****              OR 'J2001J' lo saco dde ppto ago9 pq aplico equival.
           IF (POS-1-A-6 = 'J2001J' OR 'J4201J' OR 'J0402J')                              
                    AND FECHA-MES < '200904'     
               THEN GO TO SALTO-FECHA.

           IF FECHA-MES > '200001' THEN GO TO BONIF-JERARQ-X.
      *
       SALTO-FECHA.
           IF POS-1 = 'J' AND POS-15 = 'B'
             THEN GO TO BONIF-JERARQ-X
             ELSE IF FECHA-MES NOT < '199208'
                     THEN MOVE 100.00 TO BONIFIC
                          MOVE '*BON.JERARQUIZ ' TO LEYENDA
                          PERFORM CARGAR THRU CARGAR-X.
       BONIF-JERARQ-X.
           EXIT.
       BONIF-DTO202.
            IF FECHA-MES NOT < '200403' AND FECHA-MES < '200904'
                     THEN MOVE 35.00 TO BONIFIC
                          MOVE '*D202/04 ' TO LEYENDA
                          PERFORM CARGAR THRU CARGAR-X.
       BONIF-DTO202-X.
           EXIT.
       BONIF-DTO203.
            IF FECHA-MES NOT < '200403'
                     THEN MOVE 50.00 TO BONIFIC
                          MOVE '*D203/04 ' TO LEYENDA
                          PERFORM CARGAR THRU CARGAR-X.
       BONIF-DTO203-X.
           EXIT.
       BON-DTO207.
           IF FECHA-MES  < '200403'
             THEN GO TO BON-DTO207-X.
           IF POS-1-A-6 = '30130L' OR '32430L'
             THEN GO TO BON-DTO207-X.
            IF POS-2-3 NOT < '01' AND POS-2-3 NOT > '24'  
              THEN IF FECHA-MES  < '200904'      
                 THEN MOVE 25 TO SUM25
                 ELSE MOVE 0  TO SUM25 
              ELSE GO TO BON-DTO207-X.
           COMPUTE BONI-AUX = H-HABER * 30 / POS-N-4-5.
           COMPUTE BONIFIC = (BONI-AUX * 0.04) + SUM25.
           COMPUTE NOMINAL = NOMINAL + (BONIFIC * PORC / 100).
           MOVE '*D207/04 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-DTO207-X.
           EXIT.
       BON-DTO207X.
           IF FECHA-MES  < '200403'
             THEN GO TO BON-DTO207X-X.
           IF POS-2-3 NOT < '01' AND POS-2-3 NOT > '24'  
                     THEN                            
           IF FECHA-MES  < '200904'
                 THEN MOVE 25 TO SUM25
                 ELSE MOVE 0  TO SUM25 
                     ELSE GO TO BON-DTO207X-X.
           COMPUTE BONIFIC = (H-HABER * 0.04) + SUM25.
           MOVE '*D207/04 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-DTO207X-X.
           EXIT.
       BON-DTO207J.
           IF FECHA-MES  < '200403'
             THEN GO TO BON-DTO207J-X.
           IF FECHA-MES < '200904'
                 THEN MOVE 25 TO SUM25
                 ELSE MOVE 0  TO SUM25.
           COMPUTE BONIFIC = (SUMAFIJ3 * 0.04) + 25.00.
           MOVE '*D207/04 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-DTO207J-X.
           EXIT.
       BON-DTO100.
           IF POS-1-A-6 = '30130L'
             THEN GO TO BON-DTO100-X.
            IF FECHA-MES > '200407' AND FECHA-MES < '200804'
                     THEN                            
                     MOVE 100 TO  BONIFIC                                         
                     MOVE '*D1511/04' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X.
       BON-DTO100-X.
           EXIT.
       BON-DTO100-2007.
           IF POS-1-A-6 = '30130L'
             THEN GO TO BON-DTO100-2007-X.
            IF FECHA-MES > '200803'                            
                   THEN GO TO BON-DTO100-2007-X.
            IF FECHA-MES > '200710'
                     THEN                            
                     MOVE 52  TO  BONIFIC                                         
                     MOVE '*D1511/04' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X 
                     ELSE
            IF FECHA-MES > '200703'
                     THEN                            
                     MOVE 90  TO  BONIFIC                                         
                     MOVE '*D1511/04' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X 
                     ELSE
            IF FECHA-MES NOT < '200408'
                     THEN                            
                     MOVE 100  TO  BONIFIC                                         
                     MOVE '*D1511/04' TO LEYENDA
                     PERFORM CARGAR THRU CARGAR-X.
       BON-DTO100-2007-X.
           EXIT.
       BON-ENERO05.    
            IF FECHA-MES  < '200502'
                     THEN GO TO BON-ENERO05-X.       
            IF FECHA-MES  > '200703'
                     THEN GO TO BON-ENERO05-X.       
           IF POS-1-A-6 = '30130L'
             THEN GO TO BON-ENERO05-X.
           IF POS-1 = '3' OR 'V'   
             THEN IF POS-N-4-5 < 30 THEN MOVE 85.00 TO BONIFIC
                                    ELSE
                  IF POS-N-4-5 = 30 THEN MOVE 100.00 TO BONIFIC
                                    ELSE
                  IF POS-N-4-5 < 40 OR POS-N-4-5 = 40                 
                                    THEN MOVE 110.00 TO BONIFIC
                                    ELSE MOVE 120.00 TO BONIFIC.
           IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
             THEN MOVE 100.00 TO BONIFIC.
           IF POS-1 = 'J' AND POS-4-5 = '01' 
             THEN MOVE 120.00 TO BONIFIC.
           MOVE '*ENERO/05' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-ENERO05-X.              
           EXIT.
     ****** bon-agost05: para graficos se estaba pagando mal, dde ppto mar16 cambio.
       BON-AGOST05.    
            IF FECHA-MES < '200509'
                     THEN GO TO BON-AGOST05-X.       
           IF POS-1-A-6 = '30130L' OR '32430L'
                                   OR '3C001K'
                     THEN GO TO BON-AGOST05-X.
           IF FECHA-MES < '200804' 
              THEN MOVE  30.00 TO BONIFIC 
                   GO TO PON-AGOST05-X.
     *********************** dde 2016 modif valor de boni 1823/05 pq para graficos estaba mal.
           IF POS-1 = '3' AND (POS-6 = 'G' OR 'X' OR 'Y' OR 'Z')
              THEN 
                  IF FECHA-MES > '201607' 
                     THEN MOVE 636.13 TO BONIFIC 
                     ELSE
                  IF FECHA-MES > '201603' 
                     THEN MOVE 550 TO BONIFIC 
                          GO TO PON-AGOST05-X.
     **********      ELSE  no aplico ene16 a graf pq bajaria mucho mas. abr16.
     **********   IF FECHA-MES > '201601' 
     **********      THEN MOVE 478.29 TO BONIFIC 
     **********           GO TO PON-AGOST05-X 
     **********      ELSE                        
           IF POS-1 = '3' OR 'X'                   
              THEN
                  IF FECHA-MES > '201607' 
                     THEN MOVE 1598.29 TO BONIFIC 
                          GO TO PON-AGOST05-X 
                     ELSE                        
                  IF FECHA-MES > '201603' 
                     THEN MOVE 1248.29 TO BONIFIC 
                          GO TO PON-AGOST05-X 
                     ELSE                        
                  IF FECHA-MES > '201601' 
                     THEN MOVE 978.29 TO BONIFIC.
                          GO TO PON-AGOST05-X.
           IF POS-1 = '3' OR 'X'                   
                     THEN                        
                  IF FECHA-MES > '201508' 
                     THEN MOVE 828.29 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201503' 
                     THEN MOVE 678.29 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201501' 
                     THEN MOVE 478.29 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201312' 
                     THEN MOVE 447.00 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201309' 
                     THEN MOVE 422.00 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201303' 
                     THEN MOVE 372.00 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES > '201203' 
                     THEN MOVE 230.00 TO BONIFIC 
                     ELSE                        
                  IF FECHA-MES < '200809' 
                     THEN MOVE 215.00 TO BONIFIC 
                     ELSE MOVE 250.00 TO BONIFIC 
              ELSE
           IF (POS-1-A-6 =
                'J4701J' OR 'J4801J' OR 'J4901J' OR     
                'J5001J' OR 'J5101J') OR POS-1-2 = 'JA' 
              THEN
                  IF FECHA-MES < '200809' 
                     THEN MOVE 215.00 TO BONIFIC 
                     ELSE
                  IF FECHA-MES < '201104' 
                     THEN MOVE 250.00 TO BONIFIC 
                     ELSE GO TO BON-AGOST05-X.
       PON-AGOST05-X.
            COMPUTE NOMINAL = NOMINAL + (BONIFIC * PORC / 100). 
           MOVE '*D1823/05 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-AGOST05-X.               
           EXIT.
       BONIF-20.
           IF CONC = '02' THEN GO TO BONIF-20-X.
           IF FECHA-MES < '199102' THEN GO TO BONIF-20-X.
           IF FECHA-MES < '199204'
              THEN MOVE 2000.00 TO BONIFIC
              ELSE MOVE 20.00 TO BONIFIC.
     ******   desde ppto. 200001 saco al JJS y JSJ 
     ******   porque aumentan los gastos de repres.
           IF POS-1-A-6 = 'J2001J' OR 'J4201J' OR 'J0402J'
                       OR 'J0202J'  
              THEN GO TO SALTO-A-20.
  
           IF FECHA-MES < '199808'
              THEN NEXT SENTENCE
              ELSE IF FECHA-MES < '200002'
                       THEN PERFORM OTRA-BONIF-20 THRU OTRA-BONIF-20-X
                            GO TO BONIF-20-X
                        ELSE MOVE ZEROS TO BONIFIC
                             GO TO BONIF-20-X.
       SALTO-A-20.
           MOVE '*BONI.NO REM.' TO LEYENDA.
           MOVE 'SI' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BONIF-20-X.
             EXIT.
       OTRA-BONIF-20.
           IF  POS-1 = 'J' OR POS-1 = 'V' OR
               (POS-1-5 = 'X2502' OR 'X2602' OR 'X2702')
             THEN
                   MOVE '*BONI.NO REM.' TO LEYENDA
                   MOVE 'SI' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
       OTRA-BONIF-20-X.
             EXIT.
       BON-FEBRE06.    
            IF FECHA-MES  < '200603'                           
                     THEN GO TO BON-FEBRE06-X.       
            IF FECHA-MES  > '200803'                           
                     THEN GO TO BON-FEBRE06-X.       
           IF POS-1-A-6 = '30130L'
             THEN GO TO BON-FEBRE06-X.
           MOVE 'NO' TO BON-20.
           IF POS-1 = '3'          
             THEN COMPUTE BONIFIC = 50.00 * POS-N-4-5 / 30     
             ELSE
           IF POS-1 = 'X'                                    
             THEN COMPUTE BONIFIC = 50.00                       
             ELSE 
           IF POS-1 = 'J' AND POS-4-5 = '02' 
             THEN MOVE  50.00 TO BONIFIC 
             ELSE             
           IF POS-1 = 'J' AND POS-4-5 = '01' 
             THEN MOVE  80.00 TO BONIFIC.
           MOVE '*D198/06' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-FEBRE06-X.                   
           EXIT.
    ***TITULO.AGREGO DESDE MOV.MAYO 2001 
       TITULO.
           IF POS-1 = 'J' AND POS-15 = 'B'
             THEN GO TO TITULO-X
             ELSE IF FECHA-MES NOT < '198701'
                   THEN IF POS-16 = 'T'
                     THEN COMPUTE BONIFIC = SUMAFIJ3 * 25 / 100 
                          MOVE '*TITULO JERARQ.' TO LEYENDA
                          PERFORM CARGAR THRU CARGAR-X.
       TITULO-X.    EXIT.
                        
       BON-SOLO-FIS.                       
            IF POS-15 = 'F' OR 'A' OR 'C' OR 'H' OR 'T'   
                 THEN PERFORM ARMOYPAGO THRU ARMOYPAGO-X 
                 ELSE    
             IF POS-1-5 = 'J7601' OR 'J7701' OR                                      
                (POS-15 = '7' AND (POS-1-5 =  'J2502' OR
                   'J2702' OR 'J2202' OR 'J2302' OR 'J2402'))                                             
               THEN IF FECHA-MES > '201204'               
                 THEN PERFORM ARMOYPAGO THRU ARMOYPAGO-X.
       BON-SOLO-FIS-X.
           EXIT.
       ARMOYPAGO.
             MOVE CODIGO TO CODIGO-AUXILIAR-X.
             IF POS-1 = '3'
               THEN
                    MOVE 'XX' TO POS-4-5-X
                    IF POS-6-X NOT = 'P'
                      THEN MOVE 'X' TO POS-6-X
                      ELSE NEXT SENTENCE
               ELSE MOVE 'X' TO POS-6-X.
             MOVE 0 TO BONI-FI. 
             IF FECHA-MES > '200401' AND FECHA-MES < '200708'
                           THEN
                   PERFORM BUS-EN-TABF THRU BUS-EN-TABF-X
                   VARYING IND-F FROM 1 BY 1 UNTIL IND-F > 83 
                           ELSE
                  IF FECHA-MES > '200707'                                
                    THEN
                    IF POS-1 = '3'
                        THEN MOVE SUMAFIJ1 TO BONI-FI BONIFIC
                        ELSE
                        IF POS-1 = 'J'
                        THEN PERFORM VERFECHA THRU VERFECHA-X.
   ***************           MOVE SUMAFIJ2 TO BONI-FI BONIFIC.

*************    agrego por amparos 26-4-13
             IF FECHA-MES > '201203' AND MARCA = '1'                            
                                     AND NRO = '1051352310'                     
                            THEN MOVE 7080.99 TO BONI-FI BONIFIC 
                                 MOVE 'SI' TO BON-20.
             IF FECHA-MES > '201203' AND MARCA = '1'                            
                                     AND NRO = '7046729080'                     
                            THEN MOVE 8092.56 TO BONI-FI BONIFIC 
                                 MOVE 'SI' TO BON-20.
*************    agrego por amparo 26-4-13

     ******* IF FECHA-MES > '201309' AND POS-15 = 'T' En mov ago14 se aplica set13 para el resto de los organismos.         
             IF FECHA-MES > '201203'                                             
                   THEN
                    COMPUTE BONI-FI = BONI-FI * 1.0825
                    COMPUTE BONIFIC = BONIFIC * 1.0825.
             IF FECHA-MES > '201501'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.07.  
             IF FECHA-MES > '201503'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.103701.
             IF FECHA-MES > '201508'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.0822.   
             IF FECHA-MES > '201601'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.051.   
             IF FECHA-MES > '201603'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.093923. 
   *********** aplico % de julio16 seg escala de la Tesoreria.
             IF FECHA-MES > '201607'                                             
                   THEN  COMPUTE BONIFIC = BONIFIC * 1.1392. 
             IF BONI-FI > 0
                   THEN
                    MOVE '*BON DTO.169/03'  TO LEYENDA 
                    PERFORM CARGAR THRU CARGAR-X.
       ARMOYPAGO-X.                
           EXIT.
       BUS-EN-TABF.                            
             IF POS-1-6-X = CARGO-FI (IND-F)
               THEN MOVE MONTO-FI (IND-F) TO BONI-FI BONIFIC
                    MOVE 83 TO IND-F.
       BUS-EN-TABF-X.                   
           EXIT.
       VERFECHA.                               
             IF POS-1-5 = 'J7601' OR 'J7701'                                    
                 THEN
             IF FECHA-MES > '201203' AND FECHA-MES < '201304'                                             
                      THEN MOVE 19115.33 TO BONI-FI BONIFIC
                      ELSE
             IF FECHA-MES > '201303' AND FECHA-MES < '201401'                                             
                      THEN MOVE 21011.57 TO BONI-FI BONIFIC
                      ELSE MOVE SUMAFIJ2 TO BONI-FI BONIFIC 
                 ELSE MOVE SUMAFIJ2 TO BONI-FI BONIFIC. 
                    
       VERFECHA-X.                     
           EXIT.
       BON-3.                                  
             IF FECHA-MES < '200604' THEN GO TO BON-3-X.
             IF POS-1-A-6 = '32430L' AND SUPL1 > ZEROS THEN
                   COMPUTE BONIFIC =  SUPL1
                   MOVE '*BONI.DISP.PTE.' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
             IF POS-1 = '3' THEN
             IF POS-7 = 'E' OR 'N' OR 'F' OR 'O' OR 'H' OR 'Q'
                            OR 'D' OR 'M'                        
               THEN 
             IF FECHA-MES < '200802' THEN                
                   COMPUTE BONIFIC = (H-HABER + SUPL1 + SUPL2)
                                    * 0.65
                   MOVE '*BONI.COMP.65%' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X 
                                     ELSE                    
                 COMPUTE BONIFIC = H-HABER * 0.6
                 MOVE '*BONI.COMP.60%' TO LEYENDA
                 MOVE 'NO' TO BON-20
                 PERFORM CARGAR THRU CARGAR-X 
                 COMPUTE BONIFIC = (H-HABER * 1.6 + SUPL1 + SUPL2)
                                    * 0.65
                 MOVE '*BONI.COMP.65%' TO LEYENDA
                 MOVE 'NO' TO BON-20
                 PERFORM CARGAR THRU CARGAR-X.
             IF POS-1 = '3' THEN
             IF (POS-7 = 'E' OR 'N') AND SUMAFIJ2 > ZEROS                             
               THEN 
                   COMPUTE BONIFIC = SUMAFIJ2               
                   MOVE '*BONI.COMP.10%' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X 
               ELSE
             IF (POS-7 = 'F' OR 'O') AND SUMAFIJ2 > ZEROS                             
               THEN 
                   COMPUTE BONIFIC = SUMAFIJ2               
                   MOVE '*BONI.COMP.15%' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X 
               ELSE
             IF (POS-7 = 'H' OR 'Q') AND SUMAFIJ2 > ZEROS                             
               THEN 
                   COMPUTE BONIFIC = SUMAFIJ2               
                   MOVE '*BONI.COMP.12%' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
             IF ((POS-6 = 'J' OR 'N' OR 'X' OR 'Z' OR 'Y') OR
  **********      POS-1-3 = 'X17') AND SUPL1 > ZEROS lo saco el 2-3-11
                  POS-1-3 = 'X17')                          
               THEN 
                   COMPUTE BONIFIC = H-HABER * 0.50          
                   COMPUTE NOMINAL = NOMINAL + (BONIFIC * PORC / 100)                 
                   MOVE '*BONI.DISP.PTE.' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
             IF ((POS-6 = 'J' OR 'N' OR 'X' OR 'Z') OR                                 
                  POS-1-3 = 'X17') AND SUPL2 > ZEROS
               THEN 
                   COMPUTE BONIFIC = SUPL2                   
                   MOVE '*BONI.FUNCION  ' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
       BON-3-X.       EXIT.                    
                  
       BON-CHOFER.
   **********                'O' obrero  'S' servicio
           IF POS-6 = 'O' OR 'S'
   **********                '1' OR '2' chofer
             THEN IF (POS-7 = '1' OR '2') AND SUPL2 > ZEROS
   ***************THEN IF (FECHA-MES > '200912'    mod fecha por la del dto, mov oct14.
                  THEN IF (FECHA-MES > '200812'                            
                             AND (POS-15 = 'E' OR 'Y'))
                       OR (FECHA-MES > '200903'                            
                             AND POS-15 = 'Z')
                       OR (FECHA-MES > '200907' AND                            
                         (POS-15 = 'T' OR 'I' OR 'J' OR 'M'))
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO 181/09' TO LEYENDA
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'H')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3695/09' TO LEYENDA
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'X')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3313/09' TO LEYENDA
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'L')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3186/09' TO LEYENDA
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X  
                     ELSE
                     IF (FECHA-MES > '201001'                            
                             AND POS-15 = 'Q')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO2567/10' TO LEYENDA
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X  
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'D')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3429/09' TO LEYENDA                                                              
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'F')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3319/09' TO LEYENDA                                                              
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
     ***************** agrego chof de mrio de producc en mov jul15, sin retro a pedido del 3ro,mail.
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = '2')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3470/09' TO LEYENDA                                                              
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
     ***************** agrego chof de patron de liber en mov nov15, sin retro,  pedido 8593.           
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = '3')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3303/09' TO LEYENDA                                                              
                          MOVE 'NO' TO BON-20
                          PERFORM CARGAR THRU CARGAR-X 
                     ELSE
                     IF (FECHA-MES > '200907'                            
                             AND POS-15 = 'S')
                     THEN COMPUTE BONIFIC = SUPL2
                          MOVE '*BON.DTO3311/09' TO LEYENDA                                              
                          MOVE 'NO' TO BON-20
                          DISPLAY 'ACA CHOF IPS  '  BONIFIC
                          PERFORM CARGAR THRU CARGAR-X 
                          ADD 1 TO I
                MOVE REG-MAE TO REGISTRO (I) 
                MOVE ZEROS TO A-IOMA (I) A-HABER (I) A-ANTIG (I) 
                          PERFORM BON-IPS THRU BON-IPS-X.
       BON-CHOFER-X.      EXIT.

    **********  valido hasta 22-07-10
       BON-CHOFER1.   
           IF FECHA-MES < '200901'
                THEN  GO TO BON-CHOFER1-X.
           IF POS-2-3 < '17' AND POS-N-4-5 = 48
              AND (POS-6 = 'O' OR 'S')
                THEN IF POS-7 = '1' OR '2'
                    THEN IF POS-15 = 'E' OR 'Y'
                   THEN COMPUTE BONIFIC = SUPL2
                   MOVE '*BON.DTO 181/09' TO LEYENDA
                   MOVE 'NO' TO BON-20
                   PERFORM CARGAR THRU CARGAR-X.
       BON-CHOFER1-X.      EXIT.

       BON-RADIO.
           IF FECHA-MES < '200704'
                THEN  GO TO BON-RADIO-X
                ELSE
                  IF POS-15 = 'P'
                      THEN NEXT SENTENCE 
                      ELSE GO TO BON-RADIO-X.
           COMPUTE BONIFIC = H-HABER * 0.5.
           MOVE '*BON.RADIO   ' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-RADIO-X.     EXIT.
       BON-GRAF. 
           IF FECHA-MES < '200811'
                        THEN GO TO BON-GRAF-X.         
           COMPUTE BONIFIC = H-HABER * 1.25.
           MOVE '*BON.GRAFICO ' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-GRAF-X.     EXIT.
                              
     ***** bonif p/categ maximas.
       BON-GRAFENE15.
           IF FECHA-MES < '201502'
                        THEN GO TO BON-GRAFEN-X.         
           IF POS-2-3 = '18' OR '21' THEN NEXT SENTENCE
                                     ELSE GO TO BON-GRAFEN-X.
           COMPUTE BONIFIC = H-HABER * 0.10.
           MOVE '*BON.GRAF.ENE15' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-GRAFEN-X. 

       BON-GRAF18. 
           IF FECHA-MES < '201504'
                        THEN GO TO BON-GRAF18-X.         
           IF POS-2-3 = '18' THEN NEXT SENTENCE
                             ELSE GO TO BON-GRAF18-X.
           IF FECHA-MES > '201508'
             THEN COMPUTE BONIFIC = H-HABER * 0.15 
             ELSE COMPUTE BONIFIC = H-HABER * 0.10.
           MOVE '*BON.GRAFICO18 ' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-GRAF18-X.     EXIT.
                              
       BON-CONSESC. 
           IF FECHA-MES < '201204'
                        THEN GO TO BON-CONSESC-X.         
           IF POS-1-5 = 'J5302' 
                 THEN
           COMPUTE BONIFIC = H-HABER * 0.70 
                 ELSE
           IF POS-1-5 = 'J5402' 
                 THEN
           COMPUTE BONIFIC = H-HABER * 0.80 
                 ELSE
           IF POS-1-5 = 'J5502' 
                 THEN
           COMPUTE BONIFIC = H-HABER * 0.90 
                 ELSE
           COMPUTE BONIFIC = H-HABER * 1.00.                 
           MOVE '*BON.XESTABLEC' TO  LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       PORPERMANENCIA.

           COMPUTE BONIFIC = H-HABER + BONIFIC * 0.50.                 
           MOVE '*BON.JPERMANENC' TO  LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-CONSESC-X.     EXIT.
                              
                              
       HAB-CONSEJERO.
             IF POS-17-20-A NOT NUMERIC 
               THEN DISPLAY ' sin coeficiente ' REGISTRO (1) 
               ELSE
            COMPUTE A-HABER(I) = H-HABER * POS-N-17-20A * PORC / 100.   
       HAB-CONSEJERO-X.     EXIT.
       CONSEJERO.                           
             IF SUPL2 > 0 
                 THEN
                  DISPLAY 'SUP 'SUPL2 '   HAB ' H-HABER
                  COMPUTE BONIFIC = SUPL2 - H-HABER            
                  MOVE '*BON.CONS.DIF21' TO  LEYENDA 
                  MOVE 'NO' TO BON-20 
                  PERFORM CARGAR THRU CARGAR-X.
       CONSEJERO-X.     EXIT.
 
       BON-MAR-08-1G.                        
           IF FECHA-MES  > '201607'
             THEN COMPUTE BONIFIC = 1515
             ELSE
           IF FECHA-MES  > '201603'
             THEN COMPUTE BONIFIC = 1310
             ELSE
           IF FECHA-MES  > '201503'
             THEN COMPUTE BONIFIC = 1139
             ELSE
           IF FECHA-MES  > '201501'
             THEN COMPUTE BONIFIC = 1032
             ELSE
           IF FECHA-MES  > '201303'
             THEN COMPUTE BONIFIC = 964
             ELSE
           IF FECHA-MES  > '201203'
             THEN COMPUTE BONIFIC = 857
             ELSE
           IF FECHA-MES  > '201103'
             THEN COMPUTE BONIFIC = 576
             ELSE
           IF FECHA-MES  > '201007'
             THEN COMPUTE BONIFIC = 243
             ELSE
           IF FECHA-MES  > '201004'
             THEN COMPUTE BONIFIC = 176
             ELSE COMPUTE BONIFIC = 109.
           
           MOVE '*DMAR1/08 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR-08-1G-X. EXIT.                          

       BON-MAR-08-1.                        
           IF FECHA-MES < '200804'
                     THEN GO TO BON-MAR-08-1-X.      
           IF POS-1-A-6 = '30130L' OR '32430L'
                                   OR '3C001K'
                     THEN GO TO BON-MAR-08-1-X.      
   *******************  agrego 25-03-10   saco GMAEGRAFA

           IF (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y') AND                                           
                                             FECHA-MES > '200903'                      
                 THEN PERFORM BON-MAR-08-1G THRU BON-MAR-08-1G-X 
                      GO TO BON-MAR-08-1-X. 

   *******************  agrego 25-03-10   saco GMAEGRAFA
************* set 2013
           IF FECHA-MES > '201309'  THEN                                     
             IF POS-1 = 'J' AND (POS-4-5 = 'A1'            
                             OR POS-4-5 = '01')
                     THEN                         
                      IF FECHA-MES > '201607'                                           
                        THEN MOVE 5622.03 TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201603'                                           
                        THEN MOVE 4974.14 TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201601'                                           
                        THEN MOVE 4388.95 TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201508'                                           
                        THEN MOVE 4179.95 TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201503'                                           
                        THEN MOVE 3675.98 TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201408'                                           
                        THEN MOVE 2964.5  TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201403'                                           
                        THEN MOVE 2164.5  TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE    
                      IF FECHA-MES > '201312'                                           
                        THEN MOVE 1364.5  TO BONIFIC 
                             GO TO PARAINVEST 
                        ELSE MOVE 1314.5  TO BONIFIC 
                             GO TO PARAINVEST.
           IF FECHA-MES > '201508'  THEN                                     
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 1554 TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 1554 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
           IF FECHA-MES > '201503'  THEN                                     
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 854  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 854 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
****************   agregyyo 8-01-15
           IF FECHA-MES > '201501'  THEN                                     
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 774  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND POS-4-5 = 'A1'            
                     THEN MOVE 1174.5  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND POS-4-5 = '01'
                     THEN MOVE 1084.5  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 774 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
****************   agreo 8-01-15

           IF FECHA-MES > '201303'  THEN                                     
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 723  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND POS-4-5 = 'A1'            
                     THEN MOVE 1174.5  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND POS-4-5 = '01'
                     THEN MOVE 1084.5  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 723 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
           IF FECHA-MES > '201203' THEN                                  
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 643  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 964.5  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 643 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
           IF FECHA-MES > '201103'  THEN
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
                     THEN MOVE 432  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 648  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 432 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                   ELSE
           IF FECHA-MES > '201007'  THEN
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
                     THEN MOVE 182  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 273  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 182 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                    ELSE
           IF FECHA-MES > '201003'  THEN
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
                     THEN MOVE 132  TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 198  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFICT = POS-N-4-5 * 132 / 30      
                       MOVE BONIFICT TO BONIFIC
                  ELSE GO TO BON-MAR-08-1-X 
                                    ELSE
           IF FECHA-MES > '200903'  THEN
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
                     THEN MOVE 82   TO BONIFIC 
                     ELSE
             IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 123  TO BONIFIC 
                     ELSE
             IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 82 / 30      
                  ELSE GO TO BON-MAR-08-1-X 
                                    ELSE
           IF FECHA-MES > '200808' THEN
             IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')
                     THEN MOVE 30   TO BONIFIC 
                     ELSE
           IF POS-1 = 'J' AND (POS-4-5 = '01' OR 'A1')
                     THEN MOVE 45   TO BONIFIC 
                     ELSE
           IF POS-1 = '3'
                     THEN
                      MOVE POS-N-4-5 TO BONIFIC 
                     ELSE GO TO BON-MAR-08-1-X
                                   ELSE
           IF POS-1 = 'X'      
                     THEN MOVE 45   TO BONIFIC 
                     ELSE
           IF POS-1 = 'J' AND POS-4-5 = '01'
                     THEN MOVE 68   TO BONIFIC 
                     ELSE
           IF POS-1 = 'J' AND (POS-4-5 = '02' OR 'A1')
                     THEN MOVE 45   TO BONIFIC 
                     ELSE
           IF POS-1 = '3'
                     THEN
                      MOVE POS-N-4-5 TO BONIFIC 
                      COMPUTE BONIFIC = BONIFIC * 1.5
                     ELSE GO TO BON-MAR-08-1-X.
       PARAINVEST.                      
           COMPUTE NOMINAL = NOMINAL + (BONIFIC * PORC / 100).
           MOVE '*DMAR1/08 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR-08-1-X.    EXIT.

       BON-MAR-08-2G.                      
 ****** son % del basico de la cat.1 con 30 h.
           IF FECHA-MES  < '201004' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 937.33 * 5 / 100
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 937.33 * 7.5 / 100
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 937.33 * 10 / 100
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 937.33 * 12.5 / 100
                             ELSE GO TO BON-MAR-08-2G-X
                                    ELSE
           IF FECHA-MES  < '201008' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 55.22
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 82.83
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 110.44
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 138.05
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201104' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 58.89
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 88.33
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 117.78
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 147.22
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201204' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 74.22
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 111.32
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 148.43
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 185.54
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201304' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 88.88
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 133.32
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 177.76
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 222.20
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201310' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 97.68
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 146.52
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 195.36
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 244.20
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201502' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 105.75
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 158.62
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 211.50
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 264.37
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201504' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 113.15
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 169.73
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 226.31
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 282.88
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201509' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 124.89
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 187.33
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 249.77
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 312.22
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201602' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 135.15
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 202.73
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 270.31
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 337.88
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201604' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 142.04
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 213.07
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 284.10
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 355.11
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF FECHA-MES  < '201608' THEN
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 155.50
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 233.26
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 311.01
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 388.76
                             ELSE GO TO BON-MAR-08-2G-X 
                                    ELSE
           IF POS-2-3 = '05' THEN COMPUTE BONIFIC = 177.03
                           ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = 265.55
                           ELSE
           IF POS-2-3 < '15' THEN COMPUTE BONIFIC = 354.08
                           ELSE
           IF POS-2-3 < '22' THEN COMPUTE BONIFIC = 442.58
                             ELSE GO TO BON-MAR-08-2G-X.
           MOVE '*DMAR2/08 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR-08-2G-X.    EXIT.

       BON-MAR-08-2.                      
            IF FECHA-MES < '200804'
                     THEN GO TO BON-MAR-08-2-X.      
           IF POS-1-A-6 = '30130L' OR '32430L'
                     THEN GO TO BON-MAR-08-2-X.      
    ****** son % del basico de la cat.1 con 30 h.
           IF POS-1 = '3' OR 'X' THEN NEXT SENTENCE
                                 ELSE GO TO BON-MAR-08-2-X.      

   *******************  agrego 25-03-10   saco GMAEGRAFA

           IF (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y') AND                                           
                                             FECHA-MES > '200903'                      
                 THEN PERFORM BON-MAR-08-2G THRU BON-MAR-08-2G-X 
                      GO TO BON-MAR-08-2-X.       

   *******************  agrego 25-03-10   saco GMAEGRAFA
             IF FECHA-MES > '201603'   THEN MOVE 2120.51 TO CATE1
                                       ELSE
             IF FECHA-MES > '201601'   THEN MOVE 1937.00 TO CATE1
                                       ELSE
             IF FECHA-MES > '201508'   THEN MOVE 1843.00 TO CATE1
                                       ELSE
             IF FECHA-MES > '201503'   THEN MOVE 1703.01 TO CATE1
                                       ELSE
             IF FECHA-MES > '201501'   THEN MOVE 1542.99 TO CATE1
                                       ELSE
             IF FECHA-MES > '201309'   THEN MOVE 1442.05 TO CATE1
                                       ELSE
             IF FECHA-MES > '201303'   THEN MOVE 1332.05 TO CATE1
                                       ELSE
             IF FECHA-MES > '201203'   THEN MOVE 1212.05 TO CATE1
                                       ELSE
             IF FECHA-MES > '201103'   THEN MOVE 1012.02 TO CATE1
                                       ELSE
             IF FECHA-MES > '201007'   THEN MOVE 803.02  TO CATE1
                                       ELSE
             IF FECHA-MES > '201003'   THEN MOVE 753.01 TO CATE1
                                       ELSE
             IF FECHA-MES > '200903'   THEN MOVE 703.00 TO CATE1
                                       ELSE
             IF FECHA-MES > '200808'   THEN                              
                     IF POS-6 = 'G' OR 'X' OR 'Z' OR 'Y'     
                            THEN MOVE 904.10 TO CATE1
                            ELSE MOVE 678.07 TO CATE1
                                       ELSE MOVE 616.06 TO CATE1.
***********    CATE1 = Categoria 1 con 30 horas     
           IF POS-2-3 < '06' THEN COMPUTE BONIFIC = CATE1 * 5 / 100
                             ELSE
           IF POS-2-3 < '11' THEN COMPUTE BONIFIC = CATE1 * 7.5 / 100
                             ELSE
           IF POS-2-3 < '16' THEN COMPUTE BONIFIC = CATE1 * 10 / 100
                             ELSE
           IF POS-2-3 < '25' THEN COMPUTE BONIFIC = CATE1 * 12.5 / 100 
                             ELSE GO TO BON-MAR-08-2-X.       
           COMPUTE NOMINAL = NOMINAL + (BONIFIC * PORC / 100).
           MOVE '*DMAR2/08 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR-08-2-X.    EXIT.

     *****    premios prescriptos.      
       BON-LOTERIA.                     
           IF POS-6 = 'L' THEN GO TO BON-LOTERIA-X.
           IF FECHA-MES > '201312' THEN                    
             COMPUTE BONIFIC = 3200                     
                                   ELSE
           IF FECHA-MES > '201212' THEN                    
             COMPUTE BONIFIC = 2350                     
                                   ELSE
           IF FECHA-MES > '201112' THEN                    
             COMPUTE BONIFIC = 2000                     
                                   ELSE
           IF FECHA-MES > '201012' THEN                    
             COMPUTE BONIFIC = 1650                     
                                   ELSE
           IF FECHA-MES > '200912' THEN                    
             COMPUTE BONIFIC = 1350                     
                                   ELSE
           IF FECHA-MES > '200812' THEN                    
             COMPUTE BONIFIC = 1200                     
                                   ELSE
             COMPUTE BONIFIC = 1000.                    
           MOVE '*BON.LOTERIA' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-LOTERIA-X.     EXIT.

       BON-VIVIENDA.                    
           IF POS-1 = '3' AND POS-6 = 'L' THEN GO TO BON-VIVIEN-X.
    ************ esta se calcula en base al basico de la cat. 24, hay que aplicar aum del v.modulo
           IF FECHA-MES < '200810' THEN GO TO BON-VIVIEN-X.
 ***************************************************************** no seguir actulizando, pq hay una deuda pendiente.25-8-15.
 ***************************************************************** se pago en DIC15 el reaj.                                 
           IF FECHA-MES > '201603'                    
              THEN MOVE 5326.38 TO VIV-24
              ELSE
           IF FECHA-MES > '201601'                    
              THEN MOVE 4865.43 TO VIV-24
              ELSE
           IF FECHA-MES > '201508'                    
              THEN MOVE 4629.33 TO VIV-24
              ELSE
           IF FECHA-MES > '201503'                    
              THEN MOVE 4277.69 TO VIV-24
              ELSE
           IF FECHA-MES > '201501'                    
              THEN MOVE 3875.76 TO VIV-24
              ELSE
           IF FECHA-MES > '201309'                    
              THEN MOVE 3622.20 TO VIV-24
              ELSE
           IF FECHA-MES > '201303'                    
              THEN MOVE 3345.90 TO VIV-24
              ELSE
           IF FECHA-MES > '201203'                    
              THEN MOVE 3044.47 TO VIV-24
              ELSE
           IF FECHA-MES > '201103'                    
              THEN MOVE 2542.02 TO VIV-24
              ELSE
           IF FECHA-MES > '201007'                    
              THEN MOVE 2017.04 TO VIV-24
              ELSE
           IF FECHA-MES > '201003'                    
              THEN MOVE 1891.44 TO VIV-24
              ELSE
           IF FECHA-MES > '200903'                    
              THEN MOVE 1765.82 TO VIV-24
              ELSE
           IF FECHA-MES > '200809'                    
              THEN MOVE 1703.20 TO VIV-24.
           IF POS-1 = '3' AND (FECHA-MES > '200810' AND                    
                               FECHA-MES < '201512')
                         THEN
                 IF POS-2-3R < 8   THEN                       
                 COMPUTE BONIFIC = 0.41 * VIV-24             
                 MOVE '*BON.RES.5892V' TO LEYENDA 
     *****       MOVE '*BON.VIVIENDA3' TO LEYENDA cambio leyendas dde mayo 2010.
                 PERFORM CARGAR THRU CARGAR-X 
                                   ELSE
                 COMPUTE BONIFIC = 0.49 * VIV-24             
                 MOVE '*BON.RES.5892V' TO LEYENDA 
     *****       MOVE '*BON.VIVIENDA3' TO LEYENDA 
                 PERFORM CARGAR THRU CARGAR-X.
           IF POS-1 = '3' AND (FECHA-MES > '200810' AND                    
                               FECHA-MES < '201512')
                         THEN
           PERFORM MILVIV THRU MILV-X VARYING M FROM 1 BY 1 UNTIL M > 11
                   PERFORM CARGAR THRU CARGAR-X.
           MOVE ZEROS TO PORVIV.
     ****        MOVE '*BON.VIVIENDAJ' TO LEYENDA          ******** viv-j no actualizo como v.modulo.
           IF FECHA-MES > '201309'       
               THEN
                  MOVE 6889.69 TO VIV-J
                  MOVE '*RES.5264/11' TO LEYENDA 
                  MOVE 0.15 TO PORVIV                        
               ELSE
           IF FECHA-MES > '201303'       
               THEN
                  MOVE 6414.18 TO VIV-J
                  MOVE '*RES.5264/11' TO LEYENDA 
                  MOVE 0.15 TO PORVIV                        
               ELSE
           IF FECHA-MES > '201203'       
               THEN
                  MOVE 5712.49 TO VIV-J
                  MOVE '*RES.5264/11' TO LEYENDA 
                  MOVE 0.15 TO PORVIV                        
               ELSE
           IF FECHA-MES > '201108'       
               THEN
                  MOVE 3120 TO VIV-J
                  MOVE '*RES.5264/11' TO LEYENDA 
                  MOVE 0.15 TO PORVIV                        
               ELSE
           IF FECHA-MES > '200901'       
                      THEN
                 MOVE '*BON.RES.7241V' TO LEYENDA 
                      ELSE
                 GO TO  BON-VIVIEN-X.                       
           IF (POS-1-3 = '321' OR '322' OR '323')                            
                                    AND POS-6 = 'J'                            
                         THEN  MOVE 0.625 TO PORVIV        
                         ELSE
     ****              pedido por expte                
             IF POS-1-3 = '321' AND POS-6 = 'N'                            
                         THEN MOVE  0.4  TO PORVIV         
                         ELSE
              IF POS-1-3 = '324' AND POS-6 = 'J'                               
                         THEN MOVE  0.85 TO PORVIV         
                         ELSE
              IF (POS-1-3 = '317' AND POS-6 = 'J')  OR                             
                 (POS-1-3 = '324' AND POS-6 = 'N')                                 
                         THEN MOVE  0.425 TO PORVIV         
                         ELSE
             IF POS-1-5 = 'J2601' OR 'J3701'                         
                         THEN MOVE  1.000 TO PORVIV         
                         ELSE
            IF POS-1-5 = 'J2502'            
                         THEN MOVE  0.85  TO PORVIV         
                         ELSE
             IF POS-1-5 = 'J2702'                              
                         THEN MOVE  0.90  TO PORVIV          
                         ELSE
     ****              pedido por expte, mov feb16, rea dde feb13, se toma como encargado de sector.
             IF POS-1-3 = '317' AND POS-6 = 'A'              
                         THEN MOVE  0.25  TO PORVIV.         

            IF PORVIV > ZEROS
                        THEN
                     COMPUTE BONIFIC =  PORVIV * VIV-J       
                     PERFORM CARGAR THRU CARGAR-X.
       BON-VIVIEN-X.     EXIT.

       BON-VIVIEND2.           
           IF POS-6 = 'L' THEN GO TO BON-VIVIEN2-X.
     ****  IF FECHA-MES < '200807' THEN GO TO BON-VIVIEN2-X. en movfeb13 se carga la tabla dde mar01 por pedido.
           IF FECHA-MES < '200104' THEN GO TO BON-VIVIEN2-X.
           IF POS-17-20 NOT NUMERIC
               THEN GO TO BON-VIVIEN2-X. 
           IF POS-N-17-20 = ZEROS    
             THEN GO TO BON-VIVIEN2-X. 
           PERFORM BUSCO-PTOS THRU BUSCO-PTOS-X 
               VARYING M FROM 1 BY 1 UNTIL M > 182.
           COMPUTE BONIFIC = POS-N-17-20 * PTO-VIVIEN * 100.                                       
           MOVE '*BON.VIVIEN2%' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-VIVIEN2-X.     EXIT.
       
       BUSCO-PTOS.
           IF POS-1 = '3' AND POS-6 = 'L' THEN GO TO BUSCO-PTOS-X.
           IF FECHA-MES = ANIOMESV (M)
                  THEN MOVE  VALPUN  (M) TO PTO-VIVIEN 
                  MOVE 900   TO M
                ELSE
           IF FECHA-MES > ANIOMESV (M)
                  THEN MOVE VALPUN  (M) TO PTO-VIVIEN.
       BUSCO-PTOS-X.     EXIT.

       BON-ECONOMIA.                    
           IF POS-15 = 'R' THEN                                           
              IF POS-4-5 = '40'                             
                      THEN NEXT SENTENCE
                      ELSE GO TO REGMAR2012-2014.     
                        
           IF FECHA-MES > '201306' AND
              POS-15 = 'X' AND (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y') 
                          THEN GO TO BON-ECONOM-X.   
 *************
           IF POS-13-14 NUMERIC AND POS-13-14 > '00'
                          THEN GO TO BON-ECONOM-X.   
  ********                       es el haber de su propia categ
  ********                       mov nov12 agr preg por centro de comp de arba q tienen el 60%
  ********                       mov JUL13 agr preg por centro de comp de arba con letras M-N-O-Q q faltaban
           IF POS-4-5 = '40' 
     ********        THEN IF (POS-7 = 'D' OR 'E' OR 'F' OR 'H' dde ppto mayo14 pasan al 125% tb cto de computos de arba.  
     ********                      OR 'M' OR 'N' OR 'O' OR 'Q')  
     ********                 AND POS-15 = 'Q'
     ********            THEN COMPUTE BONIFIC  = H-HABER  * 0.6             
                     THEN COMPUTE BONIFIC  = H-HABER  * 1.25            
                     ELSE                    
           IF POS-4-5 = '48' 
                         THEN COMPUTE BONIFIC = H-HABER                    
                         ELSE GO TO REGMAR2012-2014.
           COMPUTE BONIFIC = BONIFIC * PORC / 100. 
           IF POS-15 = 'X' OR 'R'
                 THEN
                 MOVE '*DTO 1535/07' TO LEYENDA              
     ****        MOVE '*DTO 1537/07' TO LEYENDA lo cambio en nov12 por pedido.
     ****        MOVE '*DBON ECONOM ' TO LEYENDA              
                 ELSE
                  IF POS-4-5 = 40
                   THEN MOVE '*DTO 2599/05' TO LEYENDA              
                   ELSE MOVE '*DTO 1563/93' TO LEYENDA.             
     *****       MOVE '*DBON ARBAQ' TO LEYENDA.             
     *****       MOVE '*DBON ECONOMQ' TO LEYENDA.  pidio el 3ro que se cambie la descrip.
           MOVE 'SI' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       REGMAR2012-2014.                      
     ********* bonif solo para registro    
     ***** IF POS-15 = 'R' AND FECHA-MES > '201203' THEN  en mov.abr15 BONREG12 se paga dde mar14 seg indico el dtor del 3ro. 
     ******** Mov.set16: exceptuo de la bon.reg.2012 a los 3..J y 3..N dde set16, preguntar por fecha para proximo reajuste.     
           IF POS-15 = 'R' AND FECHA-MES > '201403'                       
                                               THEN NEXT SENTENCE
                                               ELSE GO TO BON-ECONOM-X.
*********
           IF (POS-6 = 'J' OR 'N') AND FECHA-MES > '201609'
                                       THEN GO TO BONIFICA2014.
*********
           IF FECHA-MES > '201403' THEN COMPUTE BONIFIC = 571.09.               
           IF FECHA-MES > '201501'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.07.       
           IF FECHA-MES > '201503'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.1037.       
           IF FECHA-MES > '201508'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.0822.       
           IF FECHA-MES > '201601'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.051.        
           IF FECHA-MES > '201603'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.093923.     
           IF FECHA-MES > '201607'                                              
                            THEN COMPUTE BONIFIC = BONIFIC * 1.1385.     
           COMPUTE BONI-ANT = BONIFIC * 0.0035 * ANTIGT. 
      **        DISPLAY ' BONIF ' BONIFIC  
      **        DISPLAY ' BANT  ' BONI-ANT 
      **        DISPLAY '  ANTI ' ANTIGT   
           MOVE '*BON.REG.2012' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BONIFICA2014. 
           IF POS-15 = 'R' AND FECHA-MES > '201403' THEN NEXT SENTENCE
                                               ELSE GO TO BON-ECONOM-X.
     ******************************************  ELSE GO TO BON-ECONOM-X. el punto estaba dp de la pos.72 y pago a rh <> 30 y 40!
     ********* cargo los valores para 30 h.
           IF POS-1 = '3'      
                      THEN
               IF POS-4-5 = '30' OR '40' THEN NEXT SENTENCE
                                     ELSE GO TO BON-ECONOM-X.
     **********                      ELSE GO TO BONIF-REG14-X.
           IF FECHA-MES > '201403'                         
             THEN
           IF POS-9-10 < '03' THEN MOVE 750  TO BONIFIC
                              ELSE
           IF POS-9-10 < '05' THEN MOVE 900  TO BONIFIC
                              ELSE
           IF POS-9-10 < '10' THEN MOVE 900  TO BONIFIC
                              ELSE
           IF POS-9-10 < '15' THEN MOVE 975  TO BONIFIC
                              ELSE
           IF POS-9-10 < '20' THEN MOVE 1088 TO BONIFIC
                              ELSE
           IF POS-9-10 < '25' THEN MOVE 1125 TO BONIFIC
                              ELSE
           IF POS-9-10 < '30' THEN MOVE 1275 TO BONIFIC
                              ELSE MOVE 1313 TO BONIFIC.
           IF FECHA-MES > '201408'                         
             THEN
           IF POS-9-10 < '03' THEN MOVE 750  TO BONIFIC
                              ELSE
           IF POS-9-10 < '05' THEN MOVE 975  TO BONIFIC
                              ELSE
           IF POS-9-10 < '10' THEN MOVE 1240 TO BONIFIC
                              ELSE
           IF POS-9-10 < '15' THEN MOVE 1315 TO BONIFIC
                              ELSE
           IF POS-9-10 < '20' THEN MOVE 1425 TO BONIFIC
                              ELSE
           IF POS-9-10 < '25' THEN MOVE 1500 TO BONIFIC
                              ELSE
           IF POS-9-10 < '30' THEN MOVE 1575 TO BONIFIC
                              ELSE MOVE 1725 TO BONIFIC.
           IF POS-4-5 = '40' THEN COMPUTE BONIFIC = BONIFIC * 40 / 30.
           IF FECHA-MES > '201501'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.07.    
           IF FECHA-MES > '201503'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.1037.    
           IF FECHA-MES > '201508'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.0822.    
           IF FECHA-MES > '201601'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.051.     
           IF FECHA-MES > '201603'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.09473923.  
           IF FECHA-MES > '201607'                         
                             THEN COMPUTE BONIFIC = BONIFIC * 1.1385.      
     ***************** en mov abr15 agr antig a esta bonif tb, reaj a mar14**
           COMPUTE BONI-ANT = BONIFIC * 0.0035 * ANTIGT.
     ***   DISPLAY ' BONIF ' BONIFIC. 
     ***   DISPLAY ' BANT  ' BONI-ANT.
     ***   DISPLAY '  ANTI ' ANTIGT.  
           MOVE '*BON.REG.2014' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-ECONOM-X.         EXIT.

       BON-TUTELA.                      
  ********                       es el haber de su propia categ
           IF FECHA-MES > '201508'                                  
                                 THEN
             IF POS-7 = '3' THEN COMPUTE BONIFIC  = H-HABER * 0.85            
                            ELSE                    
             IF POS-7 = '4' THEN COMPUTE BONIFIC  = H-HABER * 0.616                   
                            ELSE                    
             IF POS-7 = '5' THEN COMPUTE BONIFIC  = H-HABER * 0.123                   
                            ELSE GO TO BON-TUTELA-X    
                                 ELSE
           IF FECHA-MES > '201503'                                  
                                 THEN
             IF POS-7 = '3' THEN COMPUTE BONIFIC  = H-HABER * 0.8             
                            ELSE                    
             IF POS-7 = '4' THEN COMPUTE BONIFIC  = H-HABER * 0.58                    
                            ELSE                    
             IF POS-7 = '5' THEN COMPUTE BONIFIC  = H-HABER * 0.116                   
                            ELSE GO TO BON-TUTELA-X    
                                 ELSE
           IF FECHA-MES > '201103'                                  
                                 THEN
           IF POS-7 = '3' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.759           
                         ELSE                    
           IF POS-7 = '4' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.55                    
                         ELSE                    
           IF POS-7 = '5' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.11                    
                         ELSE GO TO BON-TUTELA-X    
                                 ELSE
           IF POS-7 = '3' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.69            
                         ELSE                    
           IF POS-7 = '4' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.50                    
                         ELSE                    
           IF POS-7 = '5' 
                         THEN COMPUTE BONIFIC  = H-HABER * 0.10                    
                         ELSE GO TO BON-TUTELA-X.   
           COMPUTE BONIFIC = BONIFIC  * PORC / 100. 
           MOVE '*DTO 3321/08 ' TO LEYENDA.            
           MOVE 'SI' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-TUTELA-X.         EXIT.
       BON-IPS.                         
           IF POS-6 = 'L' THEN GO TO BON-IPS-X.   
           IF POS-2-3-DE-HISCAR = '05' 
                           THEN COMPUTE A-HABER (I) = H-HABER  * 1.2            
   *********                       * POS-N-4-5 / 30
                           ELSE                    
  ********                       es el haber de su propia categ
           IF POS-2-3-DE-HISCAR < '21' 
                           THEN COMPUTE A-HABER (I) = H-HABER * 1.2            
  ********                       es el haber de la categ 20
                           ELSE COMPUTE A-HABER (I) = H-HABER * 1.2.           
 ***********                       * POS-N-4-5 / 30.
           COMPUTE A-HABER (I) = A-HABER (I) * PORC / 100. 
           MOVE '*DBON IPS ' TO A-CODIGO (I).
           MOVE '02' TO A-CONC (I).
       BON-IPS-X.         EXIT.

       BON-IPS-MAY10.                  
           IF FECHA-MES > '201005' THEN NEXT SENTENCE               
                                   ELSE GO TO BON-IPS10-X.
           IF POS-6 = 'L' THEN GO TO BON-IPS10-X.   
           IF POS-2-3 > '21'            
             THEN COMPUTE BONIFIC = 400.25
             ELSE
             IF POS-2-3 > '19'                    
               THEN COMPUTE BONIFIC = 330.05
               ELSE
                IF POS-2-3 > '16'                    
                 THEN COMPUTE BONIFIC = 304.19
                 ELSE
                  IF POS-2-3 > '06'                    
                   THEN COMPUTE BONIFIC = 298.03
                   ELSE
                    IF POS-2-3 > '00'                    
                     THEN COMPUTE BONIFIC = 295.57.
           IF BONIFIC > ZEROS 
            THEN MOVE '*BON.IPS MAY/10' TO LEYENDA 
                 PERFORM CARGAR THRU CARGAR-X.
       BON-IPS10-X.   EXIT.        

       BON-TRA.                         
   *****   DISPLAY ' entro a TRABA'
           IF FECHA-MES > '200904'
                   THEN COMPUTE A-HABER (I) = H-HABER  * 0.74           
                   ELSE COMPUTE A-HABER (I) = H-HABER  * 0.72.           
           COMPUTE A-HABER (I) = A-HABER (I) * PORC / 100. 
           MOVE '*B.DTO 390/09' TO A-CODIGO (I).
           MOVE '02' TO A-CONC (I).
       BON-TRA-X.         EXIT.

       BON-ELEC.                         
            COMPUTE A-HABER (I) = H-HABER  * 0.60.            
            COMPUTE A-HABER (I) = A-HABER (I)                   
                                      * PORC / 100.  
            MOVE '*B.JUNTA ELEC' TO A-CODIGO (I).                          
            MOVE '02' TO A-CONC (I).
       BON-ELEC-X.         EXIT.
       BONELEC-131.             
  *******   DISPLAY 'hhaber  ' H-HABER.
            IF POS-6 = 'S'
                     THEN
                 COMPUTE A-HABER (I) = H-HABER  * 0.90            
                     ELSE
            IF POS-6 = 'A'
                     THEN
                 COMPUTE A-HABER (I) = H-HABER  * 1.15            
                     ELSE
            IF POS-6 = 'T' OR 'P'
                     THEN
                 COMPUTE A-HABER (I) = H-HABER  * 1.25            
                     ELSE
            IF POS-6 = 'J' OR 'N'
                     THEN
                 COMPUTE A-HABER (I) = H-HABER  * 1.50.            
            COMPUTE A-HABER (I) = A-HABER (I)                   
                                        * PORC / 100.  
            MOVE '*B.DTO 131/07' TO A-CODIGO (I).                          
            MOVE '02' TO A-CONC (I).
       BONELEC-131-X.       EXIT.
       BON-MAR09G.                  
           PERFORM BUSBONI THRU BUSBONI-X.
           IF BONIFIC = ZEROS THEN GO TO BON-MAR09G-X.
           IF FECHA-MES  > '201303' THEN
              COMPUTE BONIFIC = BONIFIC * 1.3.
           IF FECHA-MES  > '201309' THEN
              COMPUTE BONIFIC = BONIFIC * 1.23074.
           IF FECHA-MES  > '201312' THEN
              COMPUTE BONIFIC = BONIFIC / 1.23074 * 1.423.
           IF FECHA-MES  > '201403' THEN
              COMPUTE BONIFIC = BONIFIC * 5.3.
           IF FECHA-MES  > '201408' THEN
              COMPUTE BONIFIC = BONIFIC * 1.382071.
           IF FECHA-MES  > '201503' THEN
              COMPUTE BONIFIC = BONIFIC * 1.18.
           IF FECHA-MES  > '201508' THEN
              COMPUTE BONIFIC = BONIFIC * 1.1695.
           IF FECHA-MES  > '201601' THEN
              COMPUTE BONIFIC = BONIFIC * 1.065.
           IF FECHA-MES  > '201603' THEN
              COMPUTE BONIFIC = BONIFIC * 1.07981.
           IF FECHA-MES  > '201607' THEN
              COMPUTE BONIFIC = BONIFIC * 1.156521.
           MOVE '*DMAR/09 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR09G-X.    EXIT.
       BUSBONI.
           IF FECHA-MES  > '201203' THEN
           IF POS-2-3 = '05' THEN MOVE 199.62 TO BONIFIC
                             ELSE
           IF POS-2-3 = '06' THEN MOVE 201.02 TO BONIFIC
                             ELSE
           IF POS-2-3 = '07' THEN MOVE 203.37 TO BONIFIC
                             ELSE
           IF POS-2-3 = '08' THEN MOVE 205.02 TO BONIFIC
                             ELSE
           IF POS-2-3 = '10' THEN MOVE 208.31 TO BONIFIC
                             ELSE
           IF POS-2-3 = '12' THEN MOVE 211.38 TO BONIFIC
                             ELSE
           IF POS-2-3 = '14' THEN MOVE 215.60 TO BONIFIC
                             ELSE
           IF POS-2-3 = '16' THEN MOVE 235.82 TO BONIFIC
                             ELSE
           IF POS-2-3 = '18' THEN MOVE 260.86 TO BONIFIC
                             ELSE
           IF POS-2-3 = '21' THEN MOVE 397.34 TO BONIFIC
                             ELSE MOVE ZEROS TO BONIFIC
                                    ELSE
           IF FECHA-MES  > '201103' THEN
           IF POS-2-3 = '05' THEN MOVE 124.76 TO BONIFIC
                             ELSE
           IF POS-2-3 = '06' THEN MOVE 125.64 TO BONIFIC
                             ELSE
           IF POS-2-3 = '07' THEN MOVE 127.11 TO BONIFIC
                             ELSE
           IF POS-2-3 = '08' THEN MOVE 128.14 TO BONIFIC
                             ELSE
           IF POS-2-3 = '10' THEN MOVE 130.20 TO BONIFIC
                             ELSE
           IF POS-2-3 = '12' THEN MOVE 132.11 TO BONIFIC
                             ELSE
           IF POS-2-3 = '14' THEN MOVE 134.75 TO BONIFIC
                             ELSE
           IF POS-2-3 = '16' THEN MOVE 147.39 TO BONIFIC
                             ELSE
           IF POS-2-3 = '18' THEN MOVE 163.04 TO BONIFIC
                             ELSE
           IF POS-2-3 = '21' THEN MOVE 248.34 TO BONIFIC
                             ELSE MOVE ZEROS TO BONIFIC
                                    ELSE
           IF FECHA-MES  > '201007' THEN
           IF POS-2-3 = '05' THEN MOVE 249.52 TO BONIFIC
                             ELSE
           IF POS-2-3 = '06' THEN MOVE 251.28 TO BONIFIC
                             ELSE
           IF POS-2-3 = '07' THEN MOVE 254.21 TO BONIFIC
                             ELSE
           IF POS-2-3 = '08' THEN MOVE 256.28 TO BONIFIC
                             ELSE
           IF POS-2-3 = '10' THEN MOVE 260.39 TO BONIFIC
                             ELSE
           IF POS-2-3 = '12' THEN MOVE 264.22 TO BONIFIC
                             ELSE
           IF POS-2-3 = '14' THEN MOVE 269.50 TO BONIFIC
                             ELSE
           IF POS-2-3 = '16' THEN MOVE 294.77 TO BONIFIC
                             ELSE
           IF POS-2-3 = '18' THEN MOVE 326.08 TO BONIFIC
                             ELSE
           IF POS-2-3 = '21' THEN MOVE 496.68 TO BONIFIC
                             ELSE MOVE ZEROS TO BONIFIC
                                    ELSE
           IF FECHA-MES  > '201003' THEN
           IF POS-2-3 = '05' THEN MOVE 205.31 TO BONIFIC
                             ELSE
           IF POS-2-3 = '06' THEN MOVE 206.76 TO BONIFIC
                             ELSE
           IF POS-2-3 = '07' THEN MOVE 209.17 TO BONIFIC
                             ELSE
           IF POS-2-3 = '08' THEN MOVE 210.88 TO BONIFIC
                             ELSE
           IF POS-2-3 = '10' THEN MOVE 214.26 TO BONIFIC
                             ELSE
           IF POS-2-3 = '12' THEN MOVE 217.41 TO BONIFIC
                             ELSE
           IF POS-2-3 = '14' THEN MOVE 221.75 TO BONIFIC
                             ELSE
           IF POS-2-3 = '16' THEN MOVE 242.55 TO BONIFIC
                             ELSE
           IF POS-2-3 = '18' THEN MOVE 268.31 TO BONIFIC
                             ELSE
           IF POS-2-3 = '21' THEN MOVE 408.68 TO BONIFIC
                             ELSE MOVE ZEROS TO BONIFIC
                                    ELSE
           IF POS-2-3 = '05' THEN MOVE 119.16 TO BONIFIC
                             ELSE
           IF POS-2-3 = '06' THEN MOVE 120    TO BONIFIC
                             ELSE
           IF POS-2-3 = '07' THEN MOVE 121.4  TO BONIFIC
                             ELSE
           IF POS-2-3 = '08' THEN MOVE 122.39 TO BONIFIC
                             ELSE
           IF POS-2-3 = '10' THEN MOVE 124.35 TO BONIFIC
                             ELSE
           IF POS-2-3 = '12' THEN MOVE 126.18 TO BONIFIC
                             ELSE
           IF POS-2-3 = '14' THEN MOVE 128.70 TO BONIFIC
                             ELSE
           IF POS-2-3 = '16' THEN MOVE 140.77 TO BONIFIC
                             ELSE
           IF POS-2-3 = '18' THEN MOVE 155.72 TO BONIFIC
                             ELSE
           IF POS-2-3 = '21' THEN MOVE 237.19 TO BONIFIC
                             ELSE MOVE ZEROS TO BONIFIC.
       BUSBONI-X.     EXIT.

       BON-MAR09.                  
           IF FECHA-MES < '200904'
                     THEN GO TO BON-MAR09-X.      
           IF POS-1-A-6 = '30130L' OR '32430L'
                     THEN GO TO BON-MAR09-X.      
           MOVE 0 TO BONIFIC.
   *******************  agrego 25-03-10   saco GMAEGRAFA

           IF (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y') AND                                           
                                             FECHA-MES > '200903'                      
                 THEN PERFORM BON-MAR09G THRU BON-MAR09G-X 
                      GO TO BON-MAR09-X.       

   *******************  agrego 25-03-10   saco GMAEGRAFA
           IF FECHA-MES > '201103'
                     THEN GO TO BON-MAR09-X.      
           PERFORM BUSCOBONMAR09 THRU BUSCOBONMAR09-X                        
             VARYING M FROM 1 BY 1 UNTIL M > 20.
           IF BONIFIC = 0 THEN GO TO BON-MAR09-X.
           IF POS-1 = 'X'                 
                THEN NEXT SENTENCE                                         
                ELSE                 
               COMPUTE BONIFIC = BONIFIC * POS-N-4-5 / 30.   
           IF FECHA-MES > '201003'
               THEN COMPUTE BONIFIC = BONIFIC * 1.5.              
           IF FECHA-MES > '201007'
               THEN COMPUTE BONIFIC = BONIFIC * 1.25.             
           MOVE '*DMAR/09' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR09-X.       EXIT.

       BUSCOBONMAR09.                                            
           IF POS-2-3 = CAT-MAR09(M)
              THEN MOVE MO-MAR09(M) TO BONIFIC 
                   MOVE 21 TO M.
       BUSCOBONMAR09-X.      EXIT.                      
       BON-ARBAJ.                   
           COMPUTE A-HABER (I) = H-HABER * A-POS-9-12R (I)
                                                     / 100.
           MOVE '02' TO A-CONC (I).
           MOVE '*ADIC.ARBAJ ' TO A-CODIGO (I).
       BON-ARBAJ-X.       EXIT.
       BON-ARMA. 
           IF POS-15 = 'E' AND A-POS-9-12R (I) = ZEROS
                      THEN                                 
           COMPUTE A-HABER (I) = 229.10 * PORC / 100            
                      ELSE
           COMPUTE A-HABER (I) = H-HABER * A-POS-9-12R (I)       
                                                     / 100.       
           MOVE '02' TO A-CONC (I).
           IF POS-15 = 'Q'
                    THEN
           MOVE '*BONI.ARBA ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'Y'
                    THEN
           MOVE '*BONI.DSOCIAL ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '3'
                    THEN
                  MOVE '*BONI.PLIBERADOS' TO A-CODIGO (I) 
                  IF POS-7 = '3' OR '4' OR '5'
                               THEN
                  COMPUTE A-HABER (I) = A-HABER (I) * 0.80              
                               ELSE NEXT SENTENCE
                    ELSE
           IF POS-15 = 'Z'
                    THEN
           MOVE '*BONI.SALUD' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'U'
                    THEN
           MOVE '*BONI.AGRARIOS' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '1'
                    THEN
           MOVE '*BONI.SEG-JUST' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '0' OR ' '
                    THEN
           MOVE '*B.SUST.URPE' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '2'                  
                    THEN
           MOVE '*BONI.PRODUC.' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'E'               
                    THEN
           MOVE '*BONI.EDUCAC.' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '4'            
                    THEN
           MOVE '*BONI.TURISMO' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '5'            
                    THEN
           MOVE '*BONI.COR.FO ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '6'            
                    THEN
           MOVE '*BONI.DEPORTES' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'L'            
                    THEN
           MOVE '*BONI.LOTERIA ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = '8'            
                    THEN
           MOVE '*BONI.OPDS    ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'W'            
                    THEN
           MOVE '*BONI.TRABAJO ' TO A-CODIGO (I) 
                    ELSE
           IF POS-15 = 'X'            
                    THEN
   ********MOVE '*BONI.ECONOM15' TO A-CODIGO (I). cambio leyenda a pedido del 3ro.mov.feb16.
           MOVE '*BONI.D1785/15' TO A-CODIGO (I).
       BON-ARMA-X.       EXIT.
       BON-URPE.                   
           IF POS-2-3-DE-HISCAR = '20' 
                    THEN
                    IF POS-15 = 'M' OR 'P' OR 'O'
                              THEN
                    COMPUTE A-HABER (I) = H-HABER * 0.76                    
                                          * PORC / 100
                              ELSE
                    COMPUTE A-HABER (I) = H-HABER * 0.75                    
                                          * PORC / 100
                    ELSE
           IF POS-2-3-DE-HISCAR = '21' OR '24'
                      THEN
                      COMPUTE A-HABER (I) = H-HABER * 0.60                    
                                            * PORC / 100.
           MOVE '02' TO A-CONC (I).
           IF POS-15 = 'I' 
             THEN MOVE '*B.DTO1840/09' TO A-CODIGO (I) 
             ELSE IF POS-15 = 'G' 
               THEN MOVE '*B.DTO2494/09' TO A-CODIGO (I) 
               ELSE IF POS-15 = 'M' OR 'P' OR 'N' 
                THEN MOVE '*B.DTO2495/09' TO A-CODIGO (I) 
                ELSE IF POS-15 = 'O'           
                  THEN MOVE '*B.DTO3315/09' TO A-CODIGO (I) 
                  ELSE IF POS-15 = 'J'           
                  THEN MOVE '*B.JEF.GAB.  ' TO A-CODIGO (I) 
                  ELSE IF POS-15 = '7'           
                   THEN MOVE '*B.JUNTA ELEC' TO A-CODIGO (I) 
                   ELSE MOVE '*BONI.URPE   ' TO A-CODIGO (I).
       BON-URPE-X.       EXIT.
       BON-JUNTA1.
           IF POS-6 = 'S'
                    THEN
                    COMPUTE A-HABER (I) = H-HABER * 0.90
                                          * PORC / 100
                    ELSE
           IF POS-6 = 'A'
                    THEN
                    COMPUTE A-HABER (I) = H-HABER * 1.15
                                          * PORC / 100
                    ELSE
           IF POS-6 = 'T' OR 'P'
                    THEN
                    COMPUTE A-HABER (I) = H-HABER * 1.25
                                          * PORC / 100
                    ELSE
           IF POS-6 = 'J' OR 'N'
                    THEN
                    COMPUTE A-HABER (I) = H-HABER * 1.50
                                          * PORC / 100.
           MOVE '02' TO A-CONC (I).
           MOVE '*B.1448/13   ' TO A-CODIGO (I).
       BON-JUNTA1-X.       EXIT.
********

       BON-INFRAEST.                    
 **************    esto es URPE solo paga al 01 blan, pero dde oct11 habilito los '02', NO es para chofer.
      *    IF CONC = '01' AND EME = ' '
      *           THEN NEXT SENTENCE
      *           ELSE GO TO BON-INFRAE-X.
           IF POS-1 = '3' AND ((POS-6 = 'L' OR 'G' OR 'X' OR 'Z' OR 'Y')       
                          OR (POS-6 = 'S' AND POS-7 = '1' OR '2'))
                      THEN GO TO BON-INFRAE-X.
           IF FECHA-MES > '201405'
              THEN COMPUTE BONIFIC =  H-HABER            
              ELSE COMPUTE BONIFIC = 0.80 * H-HABER.           
           MOVE '*B.DTO1844/09 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-INFRAE-X.     EXIT.                    
       MILVIV.                                    
           IF POS-1 = '3' AND POS-6 = 'L' THEN GO TO MILV-X.
           IF FECHA-MES = ANIOMES (M)
                THEN 
              IF POS-2-3R < 8
                  THEN MOVE VALMEN8 (M) TO BONIFIC   
                  ELSE MOVE VALMAY7 (M) TO BONIFIC     
                  MOVE 90   TO M
                ELSE
           IF FECHA-MES > ANIOMES (M)
                   THEN
              IF POS-2-3R < 8
                  THEN MOVE VALMEN8 (M) TO BONIFIC   
                  ELSE MOVE VALMAY7 (M) TO BONIFIC.    
           MOVE '*BON.RES.5602V' TO LEYENDA.
       MILV-X.     EXIT.                    
       SUMA-DELEGADO.                    
           IF POS-15 = 'I'                         
                      THEN
                      IF K = 1 OR (A-CODIGO (K) = '*D207/04 '                 
                                            OR '*BONI.DISP.PTE.')
                               THEN              
                         COMPUTE DELEGADO = DELEGADO + A-HABER (K)                
                               ELSE  NEXT SENTENCE
                      ELSE
                      IF K = 1 OR                 
                    (A-CODIGO (K) = '*D207/04 '  OR '*D1511/04' OR   
                                                    '*ENERO/05')                     
                               THEN              
                       COMPUTE DELEGADO = DELEGADO + A-HABER (K).               
       SUMA-DELEGADO-X. EXIT.                    
       BON-DELEGADO.                    
     **    IF POS-15 = 'G' THEN GO TO RRPP  pidieron que se reemplace por J
           IF POS-15 = 'J' THEN GO TO RRPP         
                           ELSE
           IF FECHA-MES > '201607'
             THEN COMPUTE TOPEDEL = 7664.40 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201508'
             THEN COMPUTE TOPEDEL = 7205.27 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201503'
             THEN COMPUTE TOPEDEL = 6658.40 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201309'
             THEN COMPUTE TOPEDEL = 5637.93 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201203'
             THEN COMPUTE TOPEDEL = 4738.92 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201007'
             THEN COMPUTE TOPEDEL = 3140.40 * PORC / 100                          
             ELSE
           IF FECHA-MES > '201003'
             THEN COMPUTE TOPEDEL = 2944.60 * PORC / 100                          
             ELSE
           IF FECHA-MES > '200903'
             THEN COMPUTE TOPEDEL = 2748.00 * PORC / 100                          
             ELSE COMPUTE TOPEDEL = 2544.00 * PORC / 100.                         
           COMPUTE TOPEDEL = TOPEDEL - DELEGADO.                           
           IF TOPEDEL > ZEROS                 
                     THEN
                    MOVE TOPEDEL TO  BONIFIC                             
                    MOVE '*DTO.3536/06  ' TO LEYENDA 
                    MOVE 'SI' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    GO TO BON-DELEGADO-X                               
                     ELSE
                    GO TO BON-DELEGADO-X.                              
       RRPP.                                        
           IF FECHA-MES > '201607'
             THEN COMPUTE TOPEDEL = 7664.40 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201603'
             THEN COMPUTE TOPEDEL = 6731.80 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201601'
             THEN COMPUTE TOPEDEL = 6149.07 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201508'
             THEN COMPUTE TOPEDEL = 5850.68 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201503'
             THEN COMPUTE TOPEDEL = 5406.62 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201309'
             THEN COMPUTE TOPEDEL = 4578.00 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201203'
             THEN COMPUTE TOPEDEL = 3848.00 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201007'
             THEN COMPUTE TOPEDEL = 2550.00 * PORC / 100                           
             ELSE
           IF FECHA-MES > '201003'
             THEN COMPUTE TOPEDEL = 2291.00 * PORC / 100                           
             ELSE
           IF FECHA-MES > '200803'
             THEN COMPUTE TOPEDEL = 1045.00 * PORC / 100.                          
           COMPUTE TOPEDEL = TOPEDEL - DELEGADO.                           
           IF TOPEDEL > ZEROS
                             THEN NEXT SENTENCE
                             ELSE MOVE ZEROS TO TOPEDEL.
           IF POS-8 = 'R' THEN 
            COMPUTE TOPEDEL = TOPEDEL + (300 * PORC / 100)                                         
                          ELSE
           IF POS-8 = 'B' THEN 
            COMPUTE TOPEDEL = TOPEDEL + (200 * PORC / 100)                                         
                          ELSE
           IF POS-8 = 'C' THEN 
            COMPUTE TOPEDEL = TOPEDEL + (100 * PORC / 100).                                        
           IF TOPEDEL > ZEROS                 
                     THEN
                    MOVE TOPEDEL TO  BONIFIC                             
                    MOVE '*DTO.2888/05  ' TO LEYENDA 
                    MOVE 'SI' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X.
       BON-DELEGADO-X.     EXIT.                    
    ***BON-HTAL. DTO.933/13 art.7.    
       BON-HTAL.                      
           IF POS-7 = 'B' THEN GO TO BON-HTAL-X.
           IF POS-4-5 = '48' AND POS-7 = 'D' THEN GO TO BON-HTAL-X.
     ***   IF (POS-6 = 'S' OR 'O') AND (POS-7 = '1' OR '2')  lo dejo pq es anterior a las urpes.         
     ***                  THEN GO TO BON-HTAL-X.
           IF FECHA-MES > '201607'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * (0.59375 + 0.30)                        
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.59375                             
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.59375                              
     ***************** nueva bonif para los q trabajan en el mrio de salud y no cobran 'tarea critica'.
               ELSE COMPUTE BONIFIC = H-HABER * 0.09375
           ELSE
           IF FECHA-MES > '201603'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * (0.5625 + 0.30)                        
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.5625                              
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.5625                               
     ***************** nueva bonif para los q trabajan en el mrio de salud y no cobran 'tarea critica'.
               ELSE COMPUTE BONIFIC = H-HABER * 0.0625
           ELSE
           IF FECHA-MES > '201312'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * (0.50 + 0.30)                        
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.50                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.50                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
           IF FECHA-MES > '201309'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * (0.47 + 0.27)                        
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.47                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.47                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
           IF FECHA-MES > '201303'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * (0.44 + 0.26)                        
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.44                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.44                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
           IF FECHA-MES > '201203'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.64                                
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.40                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.40                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
           IF FECHA-MES > '201103'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.45                                
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.29                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.29                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
           IF FECHA-MES > '201003'
           THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.27                                
               ELSE
             IF POS-4-5 = '48'
               THEN COMPUTE BONIFIC = H-HABER * 0.19                                
               ELSE
             IF POS-6 = 'T' AND (POS-4-5 = '40' OR '36')
                   AND (POS-7 = '0' OR 'P' OR 'R' OR 'V')
               THEN COMPUTE BONIFIC = H-HABER * 0.19                                 
               ELSE GO TO BON-HTAL-X 
           ELSE
             IF FECHA-MES > '200908'
              THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
                THEN COMPUTE BONIFIC = H-HABER * 0.19                                
                ELSE
             IF POS-4-5 = '48' OR (POS-6 = 'T' AND (POS-4-5 = '40' OR       
                    '36') AND (POS-7 = '0' OR 'P' OR 'R' OR 'V'))
                  THEN COMPUTE BONIFIC = H-HABER * 0.17                                
                  ELSE GO TO BON-HTAL-X 
              ELSE
             IF FECHA-MES > '200903'
              THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
                THEN COMPUTE BONIFIC = H-HABER * 0.15                                
                ELSE
             IF POS-4-5 = '48' OR (POS-6 = 'T' AND (POS-4-5 = '40' OR          
                   '36') AND (POS-7 = '0' OR 'P' OR 'R' OR 'V'))
                  THEN COMPUTE BONIFIC = H-HABER * 0.13                                
                  ELSE GO TO BON-HTAL-X 
              ELSE              
             IF FECHA-MES > '200808'
              THEN
             IF POS-6 = 'T' AND POS-4-5 = '48'
                THEN COMPUTE BONIFIC = H-HABER * 0.14                                
                ELSE
             IF POS-4-5 = '48' OR (POS-6 = 'T' AND (POS-4-5 = '40' OR          
                   '36') AND (POS-7 = '0' OR 'P' OR 'R' OR 'V'))
                  THEN COMPUTE BONIFIC = H-HABER * 0.12                                
                  ELSE GO TO BON-HTAL-X.

           MOVE '*BON.HOSPITAL ' TO LEYENDA. 
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-HTAL-X.   EXIT.

       BON-ENF-PSIC-DESF.
     **    IF FECHA-MES > '201205' THEN NEXT SENTENCE  cambio vigencia a pedido del 3ro, pedido 5303.junio 2013.
           IF FECHA-MES > '200703' THEN NEXT SENTENCE
                                    ELSE GO TO BON-ENF-X.
    ********************** bonif enfermeria
           IF POS-6 = 'T' AND POS-7 = 'C'
                THEN COMPUTE BONIFIC = H-HABER * 0.15                                
                     MOVE '*BON.ENFERMERO' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X.
    ********************** bonif enfermeria nocturna
           IF POS-6 = 'T' AND POS-7 = 'G'
                THEN COMPUTE BONIFIC = H-HABER * 0.3                                 
                     MOVE '*BON.ENF.NOCT.' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X.
    ********************** bonif peligrosidad
           IF POS-8 = 'S' OR 'R'                
                THEN COMPUTE BONIFIC = H-HABER * 0.55                                
                     MOVE '*BON.PELIGROS.' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X 
                ELSE
    ********************** bonif distancia    
           IF POS-8 = 'L' OR 'T'                
                THEN COMPUTE BONIFIC = H-HABER * 0.1                                 
                     MOVE '*BON.DISTANCIA' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X 
                ELSE
    ********************** bonif desfavorabilidad
           IF POS-8 = 'D' OR 'V'                
                THEN COMPUTE BONIFIC = H-HABER * 0.15                                
                     MOVE '*BON.DESFAVOR.' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X 
                ELSE
    ********************** bonif distancia y peligrosidad
           IF POS-8 = 'Q' OR 'W'                
                THEN COMPUTE BONIFIC = H-HABER * (0.55 + 0.1)                               
                     MOVE '*BON.DIS.Y.PEL' TO LEYENDA  
                     MOVE 'NO' TO BON-20 
                     PERFORM CARGAR THRU CARGAR-X.
               
       BON-ENF-X.        EXIT.
*****************
       BON-RURAL.              
            IF POS-7 = '3' OR '4' OR '5' OR '6' OR '7' OR
                   'J' OR 'K' OR 'L' OR 'Z' OR 'W' OR 'T'  
     *******    OR 'M' OR 'N' OR 'O'  mov jul12 los cambio por z w t
              THEN NEXT SENTENCE
              ELSE GO TO BON-RURAL-X.  
            IF POS-7 = '3' THEN                           
                    COMPUTE BONIFIC = H-HABER * 0.3                                 
                    MOVE '*BON.EDUC.RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = '4' THEN                           
                     COMPUTE BONIFIC = H-HABER * 0.6                                 
                    MOVE '*BON.EDUC.RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = '5' THEN                           
                     COMPUTE BONIFIC = H-HABER * 0.9                                 
                    MOVE '*BON.EDUC.RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = '6' THEN                           
                     COMPUTE BONIFIC = H-HABER                                       
                    MOVE '*BON.EDUC.RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = '7' THEN                           
                     COMPUTE BONIFIC = H-HABER * 1.2                                 
                    MOVE '*BON.EDUC.RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = 'J' THEN                           
                     COMPUTE BONIFIC = H-HABER                                       
                    MOVE '*BON.JORNADA.C ' TO LEYENDA  
                           ELSE
            IF POS-7 = 'K' THEN                           
                     COMPUTE BONIFIC = H-HABER * 1.3                                      
                    MOVE '*BON.JORN-RURAL ' TO LEYENDA  
                           ELSE
            IF POS-7 = 'L' THEN                           
                     COMPUTE BONIFIC = H-HABER * 1.6                                      
                    MOVE '*BON.JORN-RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = 'Z' THEN                           
                     COMPUTE BONIFIC = H-HABER * 1.9                                      
                    MOVE '*BON.JORN-RURAL' TO LEYENDA  
                           ELSE
            IF POS-7 = 'W' THEN                           
                     COMPUTE BONIFIC = H-HABER * 2                                        
                    MOVE '*BON.JORNADA.C ' TO LEYENDA  
                           ELSE
            IF POS-7 = 'T' THEN                           
                     COMPUTE BONIFIC = H-HABER * 2.2                                      
                    MOVE '*BON.JORNADA.C ' TO LEYENDA. 
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-RURAL-X.   EXIT.

       BON-AUXD.             
           IF (POS-2-3 > '00' AND POS-2-3 < '09')
                           AND POS-6 = 'S'
              THEN NEXT SENTENCE
              ELSE GO TO BON-AUXD-X.
           IF FECHA-MES > '201607'
                THEN COMPUTE BONIFIC = H-HABER * 102 / 100                                 
                ELSE
           IF FECHA-MES > '201603'
                THEN COMPUTE BONIFIC = H-HABER * 90 / 100                                 
                ELSE
           IF FECHA-MES > '201508'
                THEN COMPUTE BONIFIC = H-HABER * 86 / 100                                 
                ELSE
           IF FECHA-MES > '201503'
                THEN COMPUTE BONIFIC = H-HABER * 83 / 100                                 
                ELSE
           IF FECHA-MES > '201312'
                THEN COMPUTE BONIFIC = H-HABER * 79 / 100                                 
                ELSE
           IF FECHA-MES > '201309'
                THEN COMPUTE BONIFIC = H-HABER * 76 / 100                                 
                ELSE
           IF FECHA-MES > '201303'
                THEN COMPUTE BONIFIC = H-HABER * 71 / 100                                 
                ELSE COMPUTE BONIFIC = H-HABER * 64 / 100.                                
           MOVE '*BON.AUX.DOC.  ' TO LEYENDA. 
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-AUXD-X.   EXIT.

       BON-AUXD15.           
           IF FECHA-MES < '201504'
              THEN GO TO BON-AUXD15-X.
           IF (POS-2-3 > '00' AND POS-2-3 < '09')
                           AND POS-6 = 'S'
              THEN NEXT SENTENCE
              ELSE GO TO BON-AUXD15-X.
           COMPUTE BONIFIC = 383.99.                                            
           MOVE '*BON.AUX.DOC/15' TO LEYENDA. 
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-AUXD15-X.   EXIT.
                              
     **** seg anios de servicios. estimulo personal no jerarquico.
       BON-ESCRIB.             
           IF POS-1 = '3' AND (POS-6 = 'L' OR 'G' OR 'X' OR 'Z' OR 'J'        
                                           OR 'Y')                            
                      THEN GO TO BON-ESCRIB-X.
           IF FECHA-MES < '201104'
             THEN COMPUTE BONIFIC = 3118.52                   
             ELSE IF FECHA-MES > '201103'
               THEN IF POS-9-10  = '00'
                      THEN MOVE 2880.59 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '05'
                      THEN MOVE 3179    TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '10'
                      THEN MOVE 3327.38 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '15'
                      THEN MOVE 3475.75 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '20'
                      THEN MOVE 3773.41 TO BONIFIC
                      ELSE 
                           MOVE 3920.40 TO BONIFIC.
           IF FECHA-MES > '201111'
             THEN COMPUTE BONIFIC = BONIFIC + 500.
           IF FECHA-MES > '201203'
             THEN COMPUTE BONIFIC = BONIFIC * 1.2.
           IF FECHA-MES > '201302'
               THEN IF (POS-2-3 = '21' OR '22' OR '23')
                        AND POS-6 = 'J'
                      THEN MOVE 5900    TO BONIFIC
                      ELSE 
                    IF POS-9-10  = '00'
                      THEN MOVE 4100    TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '05'
                      THEN MOVE 4600    TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '10'
                      THEN MOVE 4825    TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '15'
                      THEN MOVE  5050   TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '20'
                      THEN MOVE  5475   TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '25'
                      THEN MOVE  5700   TO BONIFIC
                      ELSE 
                           MOVE 5900    TO BONIFIC.
           IF FECHA-MES > '201303'
             THEN COMPUTE BONIFIC = BONIFIC * 1.099.
           IF FECHA-MES > '201308'
               THEN IF (POS-2-3 = '21' OR '22' OR '23')
                        AND POS-6 = 'J'
                      THEN MOVE 8200    TO BONIFIC
                      ELSE 
                    IF POS-9-10  = '00'
                      THEN MOVE 4505.9  TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '05'
                      THEN MOVE 5069.48 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '10'
                      THEN MOVE 5570.53 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '15'
                      THEN MOVE 6070.85 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '20'
                      THEN MOVE  6770.11 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '25'
                      THEN MOVE  7268.78 TO BONIFIC
                      ELSE 
                           MOVE 7588.58 TO BONIFIC.
           IF FECHA-MES > '201309'
                    IF POS-9-10  = '00'
                      THEN MOVE 4878.08 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '05'
                      THEN MOVE 5488.22 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '10'
                      THEN MOVE 6030.66 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '15'
                      THEN MOVE 6572.30 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '20'
                      THEN MOVE  7329.32 TO BONIFIC
                      ELSE 
                    IF POS-9-10 < '25'
                      THEN MOVE  7869.18 TO BONIFIC
                      ELSE 
                           MOVE 8215.40 TO BONIFIC.
           IF FECHA-MES > '201403'
             THEN COMPUTE BONIFIC = BONIFIC * 1.25. 
           IF FECHA-MES > '201501'
             THEN COMPUTE BONIFIC = BONIFIC * 1.07. 
           IF FECHA-MES > '201503'
             THEN COMPUTE BONIFIC = BONIFIC * 1.1037 * 1.1686.
           IF FECHA-MES > '201601'
             THEN COMPUTE BONIFIC = BONIFIC * 1.07.
           IF FECHA-MES > '201603'
             THEN COMPUTE BONIFIC = BONIFIC * 1.1.
           IF FECHA-MES > '201607'
             THEN COMPUTE BONIFIC = BONIFIC * 1.17.
           MOVE '*B.L.10295    ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-ESCRIB-X.      EXIT.

      *** bonif para escrib gral de gobierno para cargos especificos, otras en gehi con cod 3SE... mov mar15.
       BON-ESCRIBBON.                
           IF POS-1 = '3' AND (POS-6 = 'L' OR 'G' OR 'X' OR 'Z' OR 'J'        
                                           OR 'Y')                            
                      THEN GO TO BON-ESCRIBB-X.
           IF FECHA-MES > '201403'
             THEN NEXT SENTENCE
             ELSE GO TO BON-ESCRIBB-X.
           IF POS-1-5 = 'J1901' 
              THEN COMPUTE BONIFIC = 13191.39         
              ELSE IF POS-1-5 = 'J2502' 
                 THEN COMPUTE BONIFIC = 8927.88          
                 ELSE IF POS-1 = '3' AND POS-6 = 'J' AND                            
                         POS-2-3 = '21' OR '22' OR '23'
                      THEN COMPUTE BONIFIC = 11096.65.        
           IF FECHA-MES > '201501'
             THEN 
              IF POS-1-5 = 'J1501' 
               THEN COMPUTE BONIFIC = 22185.95         
               ELSE COMPUTE BONIFIC = BONIFIC * 1.07. 
           IF FECHA-MES > '201503'
               THEN COMPUTE BONIFIC = BONIFIC * 1.1037 * 1.1686.
           IF FECHA-MES > '201601'
               THEN COMPUTE BONIFIC = BONIFIC * 1.07.             
           IF FECHA-MES > '201603'
               THEN COMPUTE BONIFIC = BONIFIC * 1.10.             
           IF FECHA-MES > '201607'
               THEN COMPUTE BONIFIC = BONIFIC * 1.17.             
           MOVE '*B.L.10295B   ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-ESCRIBB-X.      EXIT.
                              
       BON-VIV-1661.           
           IF FECHA-MES > '201511'
                  THEN COMPUTE NOMINAL1 = NOMINAL * 1.25
                       IF POS-2-3-ORI < '08'
                           THEN MOVE 41.00 TO PORNOM
                           ELSE
                          IF POS-2-3-ORI < '25'
                           THEN MOVE 49.0  TO PORNOM
                           ELSE NEXT SENTENCE 
                  ELSE
            MOVE NOMINAL TO NOMINAL1 
           IF FECHA-MES > '201106'
                  THEN IF POS-2-3-ORI < '08'
                           THEN MOVE 32.8 TO PORNOM
                           ELSE
                          IF POS-2-3-ORI < '25'
                           THEN MOVE 39.2 TO PORNOM
                           ELSE NEXT SENTENCE
                   ELSE IF POS-2-3-ORI < '08'
                           THEN MOVE 16.4 TO PORNOM
                           ELSE
                          IF POS-2-3-ORI < '25'
                           THEN MOVE 19.6 TO PORNOM
                           ELSE GO TO BON-VIV-1661-X.
           COMPUTE BONIFIC = NOMINAL1 * PORNOM / 100.                 
           MOVE '*RES.1661/11  ' TO LEYENDA.
           MOVE 'SI' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-VIV-1661-X.      EXIT.

       BON-CIC-TEC.            
           IF FECHA-MES < '201204'
             THEN GO TO BON-CIC-TEC-X.
           IF POS-2-3 = 'A7' OR 'A6' OR 'A5' OR 'A4'
                 THEN NEXT SENTENCE
                 ELSE GO TO BON-CIC-TEC-X.
           MOVE '*BON.CIC.TEC.' TO LEYENDA.
           IF POS-2-3 = 'A7'                        
             THEN COMPUTE BONIFIC = H-HABER * 70 / 100
             ELSE
           IF POS-2-3 = 'A6'                        
             THEN COMPUTE BONIFIC = H-HABER * 55 / 100
             ELSE
           IF POS-2-3 = 'A5'                        
             THEN COMPUTE BONIFIC = H-HABER * 50 / 100
             ELSE
           IF POS-2-3 = 'A4'                        
             THEN COMPUTE BONIFIC = H-HABER * 25 / 100.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-CIC-TEC-X.     EXIT.

     ********* D8080/86 es solo para investigadores, mov jun15, retro 2 anios aunq el dto es dde octubre de 1986.
       BON-INV-D808086.           
           IF POS-1-2 = 'JA' THEN GO TO BON-INV-X.
           IF FECHA-MES < '201307' THEN GO TO BON-INV-X.
           IF POS-6 = 'A' THEN GO TO BON-INV-X.
           IF POS-15 = 'B' THEN GO TO BON-INV-X.
           COMPUTE BONIFIC = H-HABER * 25 / 100.
           MOVE '*BON.DTO8080/86' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-INV-X.     EXIT.       
                              
     ***** corresponde al dto 1076/12.es la parte remuner de la bonif. aumenta cada anio en agosto.
       BON-IPSAGO12.           
           IF FECHA-MES > '201608'
             THEN COMPUTE BONIFIC = 3182                      
             ELSE
           IF FECHA-MES > '201508'
             THEN COMPUTE BONIFIC = 2235                      
             ELSE
           IF FECHA-MES > '201408'
             THEN COMPUTE BONIFIC = 1534                      
             ELSE
           IF FECHA-MES > '201308'
             THEN COMPUTE BONIFIC = 1177                      
             ELSE COMPUTE BONIFIC = 900.00.                   
           MOVE '*BON.DTO1076/12' TO LEYENDA.
     ***** MOVE '*BON.IPSAGO12 ' TO LEYENDA. mov oct14 cambio leyenda a pedido del 3ro.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-IPSAGO12-X.    EXIT.
                              
     ***** es la parte remuner de la bonif; aumentara dp del valor inicial, cada anio en agosto.
       BON-IPSJUL15.              
           IF FECHA-MES > '201608'
             THEN COMPUTE BONIFIC = 1380                     
             ELSE                           
              IF FECHA-MES > '201507'
               THEN COMPUTE BONIFIC = 1200                     
               ELSE GO TO BON-IPSJUL15-X.
           MOVE '*BON.IPS.JUL15' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-IPSJUL15-X.    EXIT.

     **** DTO.382/13.          
       BON-IOMASET12.          
           IF FECHA-MES > '201609'
             THEN COMPUTE BONIFIC = 3540                      
             ELSE
           IF FECHA-MES > '201509'
             THEN COMPUTE BONIFIC = 2900                      
             ELSE
           IF FECHA-MES > '201409'
             THEN COMPUTE BONIFIC = 2200                      
             ELSE
           IF FECHA-MES > '201309'
             THEN COMPUTE BONIFIC = 1855                      
             ELSE COMPUTE BONIFIC = 1531.                  
           MOVE '*BON.IOMASET12' TO LEYENDA.
           MOVE 'NO' TO BON-20.
           PERFORM CARGAR THRU CARGAR-X.
       BON-IOMA12-X.    EXIT.
                              
       BON-MAR2014.                         
           IF FECHA-MES < '201404'
                     THEN GO TO BON-MAR2014-X.      
           IF POS-1-A-6 = '30130L' OR '32430L'
                                   OR '3C001K'
                     THEN GO TO BON-MAR2014-X.      
           IF POS-1 = '3' AND
              (POS-6 = 'G' OR 'Z' OR 'X' OR 'Y')                                               
                     THEN GO TO BON-MAR2014-X.      

           IF FECHA-MES < '201409'        
                                    THEN
           IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 1170 / 30      
                  ELSE
            IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 1170 TO BONIFIC 
                     ELSE GO TO BON-MAR2014-X 
                                    ELSE
*****************  agrego 8-1-15
           IF FECHA-MES < '201502'      
                                    THEN
           IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 1550 / 30      
                  ELSE
            IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 1550 TO BONIFIC 
                     ELSE GO TO BON-MAR2014-X 
                                    ELSE
           IF FECHA-MES < '201604'      
                                    THEN
           IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 1659 / 30      
                  ELSE
            IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 1659 TO BONIFIC 
                     ELSE GO TO BON-MAR2014-X 
                                    ELSE
           IF FECHA-MES < '201608'      
                                    THEN
           IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 1914 / 30      
                  ELSE
            IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 1914 TO BONIFIC 
                     ELSE GO TO BON-MAR2014-X 
                                    ELSE
           IF POS-1 = '3'
                  THEN COMPUTE BONIFIC = POS-N-4-5 * 2114 / 30      
                  ELSE
            IF POS-1 = 'X' OR (POS-1 = 'J' AND POS-4-5 = '02')                                            
                     THEN MOVE 2114 TO BONIFIC 
                     ELSE GO TO BON-MAR2014-X.

           IF FECHA-MES > '201511'      
                           THEN
                 COMPUTE NOMINAL = NOMINAL +                            
                                  (BONIFIC * PORC / 100).
           MOVE '*DMAR2014 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-MAR2014-X.       EXIT.           

       BON-RESPESTR.                        
           IF FECHA-MES > '201408'
              THEN COMPUTE BONIFIC = H-HABER * 0.50           
              ELSE COMPUTE BONIFIC = H-HABER * 0.28.          
           MOVE '*BON.RESP.EST.' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-RESP-X.       EXIT.              

       BON-LEYES.                           
           IF FECHA-MES > '201408'
              THEN COMPUTE BONIFIC = 1550                     
              ELSE COMPUTE BONIFIC = 1170.                    
           MOVE '*BON ADI2014L.' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-LEYES-X.      EXIT.              

       BON-PERM15.                          
     ****  GO TO BON-PERM15-X.
     ****  si pos-8=P son los que tienen la anterior permanencia q fue solo para los q la cobraron en activ., en 2015 es p/todos.
           IF POS-8 = 'P' THEN
             IF POS-2-3 = '24' OR '15' THEN 
                COMPUTE HAB-TOPE = H-HABER / 1.03
                                       ELSE
             IF POS-2-3 = '18' OR '21' THEN 
                COMPUTE HAB-TOPE = H-HABER / 1.0111
                                       ELSE
             IF POS-2-3 = '16' THEN 
                COMPUTE HAB-TOPE = H-HABER / 1.05822
                               ELSE
             IF POS-2-3 = '17' THEN 
                COMPUTE HAB-TOPE = H-HABER / 1.0249067
                               ELSE GO TO BON-PERM15-X 
                          ELSE COMPUTE HAB-TOPE = H-HABER.                 
           COMPUTE BONIFIC = HAB-TOPE * 0.20.          
           MOVE '*BON.PERM2015 ' TO LEYENDA.
           PERFORM CARGAR THRU CARGAR-X.
       BON-PERM15-X.      EXIT.              

       BON-NINEZ.                           
           MOVE ZEROS TO BONIFIC.
           IF FECHA-MES > '201508'
            THEN
             IF POS-8 = 'N' OR 'Q' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.123                                                 
                    MOVE '*DTO.1177/11-A ' TO LEYENDA 
                    GO TO ALNINEZ 
                                   ELSE
             IF POS-8 = 'M' OR 'L' THEN 
     ************   COMPUTE BONIFIC = H-HABER * 0.55 en mov marzo 15 el 3ro pide cambios. solo queda D=55%.
                    COMPUTE BONIFIC = H-HABER * 0.38                                                   
                    MOVE '*DTO.10104/87 ' TO LEYENDA 
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.236                                                 
                    MOVE '*DTO.1177/09' TO LEYENDA 
     ************** MOVE '*DTO.2492/08-15' TO LEYENDA 
                    GO TO ALNINEZ
                                   ELSE
             IF POS-8 = 'D' OR 'P' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.55                                              
                    MOVE '*DTO.6166/89  ' TO LEYENDA 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.3                                                   
                    MOVE '*DTO.6166/89-C ' TO LEYENDA 
                    GO TO ALNINEZ.

           IF FECHA-MES > '201503'
            THEN
             IF POS-8 = 'N' OR 'Q' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.116                                                 
                    MOVE '*DTO.1177/11-A ' TO LEYENDA 
                    GO TO ALNINEZ 
                                   ELSE
             IF POS-8 = 'M' OR 'L' THEN 
     ************   COMPUTE BONIFIC = H-HABER * 0.55 en mov marzo 15 el 3ro pide cambios. solo queda D=55%.
                    COMPUTE BONIFIC = H-HABER * 0.38                                                   
                    MOVE '*DTO.10104/87 ' TO LEYENDA 
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.2                                                   
                    MOVE '*DTO.1177/09' TO LEYENDA 
     ************** MOVE '*DTO.2492/08-15' TO LEYENDA 
                    GO TO ALNINEZ
                                   ELSE
             IF POS-8 = 'D' OR 'P' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.55                                              
                    MOVE '*DTO.6166/89  ' TO LEYENDA 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.25                                                  
                    MOVE '*DTO.6166/89-C ' TO LEYENDA 
                    GO TO ALNINEZ.

           IF FECHA-MES > '201103'
            THEN
             IF POS-8 = 'N' OR 'Q' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.11                                                  
                    MOVE '*DTO.1177/11-A ' TO LEYENDA 
                    GO TO ALNINEZ 
                                   ELSE
             IF POS-8 = 'M' OR 'L' THEN 
     ************   COMPUTE BONIFIC = H-HABER * 0.55 en mov marzo 15 el 3ro pide cambios.              
                    COMPUTE BONIFIC = H-HABER * 0.38                                                   
                    MOVE '*DTO.10104/87 ' TO LEYENDA 
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.17                                                  
                    MOVE '*DTO.1177/09' TO LEYENDA 
     ************** MOVE '*DTO.2492/08-15' TO LEYENDA 
                    GO TO ALNINEZ
                                   ELSE
             IF POS-8 = 'D' OR 'P' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.55                                              
                    MOVE '*DTO.6166/89  ' TO LEYENDA 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.209                                                 
                    MOVE '*DTO.6166/89-C ' TO LEYENDA 
                    GO TO ALNINEZ.

           IF FECHA-MES > '200808'
            THEN
     ***** IF POS-8 = 'L' THEN 
           IF POS-8 = 'N' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.10                                                       
                    MOVE '*DTO.2492/08-14' TO LEYENDA 
                    GO TO ALNINEZ
                          ELSE
           IF POS-8 = 'M' THEN 
     **********     COMPUTE BONIFIC = H-HABER * 0.55                                                  
                    COMPUTE BONIFIC = H-HABER * 0.38                                                  
                    MOVE '*DTO.10104/87 ' TO LEYENDA 
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.12                                                  
                    MOVE '*DTO.2492/08-15' TO LEYENDA 
                    GO TO ALNINEZ
                          ELSE
           IF POS-8 = 'D' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.55                                                
                    MOVE '*DTO.6166/89  ' TO LEYENDA 
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X 
                    COMPUTE BONIFIC = H-HABER *  0.14                                                  
                    MOVE '*DTO.6166/89-15  ' TO LEYENDA 
                    GO TO ALNINEZ.

    *****  IF POS-8 = 'P' THEN 
    *****           COMPUTE BONIFIC = H-HABER * 0.38                                                       
    *****           MOVE '*DTO.5828/86  ' TO LEYENDA 
    *****                 ELSE
           IF POS-8 = 'M' THEN 
    ***********     COMPUTE BONIFIC = H-HABER * 0.55                                                       
                    COMPUTE BONIFIC = H-HABER * 0.38                                                       
                    MOVE '*DTO.10104/87 ' TO LEYENDA 
                          ELSE
           IF POS-8 = 'D' THEN 
                    COMPUTE BONIFIC = H-HABER * 0.55                                                       
                    MOVE '*DTO.6166/89  ' TO LEYENDA.

       ALNINEZ.
           IF BONIFIC > 0 THEN
                    MOVE 'NO' TO BON-20 
                    PERFORM CARGAR THRU CARGAR-X.
       BON-NINEZ-X.     EXIT.                    

       VER-TOPE.   
             IF FECHA-MES > '201603'
                            THEN IF A-HABER (I) > 56057.93
                                 THEN MOVE 56057.93 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201509'
                            THEN IF A-HABER (I) > 48598.08
                                 THEN MOVE 48598.08 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201503'
                            THEN IF A-HABER (I) > 43202.17
                                 THEN MOVE 43202.17 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201409'
                            THEN IF A-HABER (I) > 36531.48
                                 THEN MOVE 36531.48 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201403'
                            THEN IF A-HABER (I) > 31167.56
                                 THEN MOVE 31167.56 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201309'
                            THEN IF A-HABER (I) > 28000.65
                                 THEN MOVE 28000.65 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201303'
                            THEN IF A-HABER (I) > 24473.92
                                 THEN MOVE 24473.92 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201209'
                            THEN IF A-HABER (I) > 21248.45
                                 THEN MOVE 21248.45 TO A-HABER (I)
                                 ELSE NEXT SENTENCE
                            ELSE
             IF FECHA-MES > '201203'
                            THEN IF A-HABER (I) > 19070.55
                                 THEN MOVE 19070.55 TO A-HABER (I). 

       VER-TOPE-X.   

       BON-TRAB16.
            COMPUTE BONIFIC = 989.1 * PORC / 100.
            MOVE 'SI' TO BON-20.
            MOVE '*BON.TRAB 678/16' TO LEYENDA.
            PERFORM CARGAR THRU CARGAR-X.
       BON-TRAB16-X.   EXIT.

       CARGAR.
           IF BONIFIC NOT > ZEROS THEN GO TO CARGAR-X.
           ADD 1 TO I.
           MOVE REG-MAE TO REGISTRO (I).
           MOVE ZEROS TO  A-IOMA (I).               
           IF BONI-ANT > ZEROS                      
              THEN MOVE BONI-ANT TO A-ANTIG (I) 
              ELSE MOVE ZEROS    TO A-ANTIG (I).
           IF BON-20 = 'SI'                                        
              THEN COMPUTE A-HABER (I) = BONIFIC
              ELSE COMPUTE A-HABER (I) = BONIFIC * PORC / 100 
                   COMPUTE A-ANTIG (I) = A-ANTIG (I) * PORC / 100.
           MOVE '02' TO A-CONC (I).
           MOVE LEYENDA TO A-CODIGO (I).
           MOVE ZEROS TO BONIFIC BONI-ANT.           
           MOVE 'NO' TO BON-20.
       CARGAR-X.     EXIT.
                    
