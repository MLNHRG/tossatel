SELECT  --substring([TODO],1,4),
substring([TODO],5,3) Codigo,
substring([TODO],8,12) Ley,
substring([TODO],20,50) Beneficio
--,[TODO]
into dbo.T003
FROM [IPS].[dbo].[_T003]


--------------------------------


SELECT --substring([TODO],1,4),
substring([TODO],5,3) Codigo,
substring([TODO],8,55) Entidad
--into [IPS].[dbo].[T005]
FROM [IPS].[dbo].[_T005]


 dbo.TRIM
 

 SELECT * FROM [dbo].[T005]
 WHERE Entidad LIKE '%#%'

 UPDATE [dbo].[T005] SET Entidad =  dbo.TRIM(Entidad)