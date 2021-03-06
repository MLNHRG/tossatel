/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP 1000 [Id]
      ,[Codigo]
      ,[Reparticion]
  FROM [IPS].[dbo].[ReparticionesYMunicipios]

  SELECT * FROM  aux_antADMI_

  SELECT A.TODO TODO1, B.TODO TODO2 
  into #D
  FROM 
(SELECT     id,  (id/2.00) a_ ,(id/2.00)-0.001 b_, FLOOR((id/2.00)-0.001  ) FL  , TODO
FROM          aux_antADMI_
WHERE   (id/2.00)-0.001 - FLOOR((id/2.00)-0.001  ) = 0.999000  ) A
inner 
join 
(SELECT     id,  (id/2.00) a_ ,(id/2.00)-0.001 b_, FLOOR((id/2.00)-0.001  ) FL , TODO
FROM          aux_antADMI_
WHERE   (id/2.00)-0.001 - FLOOR((id/2.00)-0.001  ) <> 0.999000  ) B
 on A.FL = B.FL


SELECT  substring(TODO1,15,30)  Observacion ,
convert(int,  substring(TODO1,45,30)    ) Año   ,
substring(TODO2,41,2)  Linea ,
dbo.trim(substring(TODO2,43,2) ) Letra,
convert(int, substring(TODO2,48,1))  Porcentaje
into dbo.Antiguedades_ADMI
FROM  #D


SELECT * FROM dbo.Antiguedades_ADMI





       --  02  F PIC X(36) VALUE '324XXJ052500 0163316 0539786 0600332'.
SELECT
substring(TODO,35,6) 'CARGO-F' ,
convert(decimal(10,2), substring(TODO,41,6)  ) /100 'MONTO-F',
convert(decimal(10,2), substring(TODO,48,7)  ) /100 'MONTO-F2012',
convert(decimal(10,2), substring(TODO,56,7)  ) /100 'MONTO-F2014',
convert(decimal(10,2), substring(TODO,64,7)  ) /100 'MONTO-F22014',
[TODO]
into ADMI_FISC_CONT_TESOR
FROM [IPS].[dbo].[_GMAEADMI_FISC_CONT_TESOR]



SELECT 
substring(TODO,42,6) 'CARGO-FI'  ,
substring(TODO,48,6) 'F'
, substring(TODO,55,6) 'MONTO-FI'
--,convert(decimal(10,2), substring(TODO,55,6)  ) /100 'MONTO-F2012'
,TODO
FROM [dbo].[_GMAEADMI_TABLA_SOLO_FISCALIA]

           02  FILLER   PIC X(19) VALUE '321XXP064000 076000'.


		   CARGO-F	MONTO-F	TODO
301XXX	180.000000	           02  FILLER   PIC X(19) VALUE '301XXX018000 030000'.
J2702X	2300.000000	           02  FILLER   PIC X(19) VALUE 'J2702X230000 230000'.



----------------

SELECT 
substring(TODO,42,2) 'CAT-MAR09'  
--, substring(TODO,44,7) 'CAT-MAR09'
,convert(decimal(10,2), substring(TODO,44,7)  ) /100 'MO-MAR09'
,TODO
--SELECT *
INTO ADMI_TABLA_MAR09
FROM [dbo].[_GMAEADMI_TABLA_MAR09]


           02  FILLER   PIC X(09) VALUE '05 008937'.