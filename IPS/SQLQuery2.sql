SELECT      ID, TODO ,  replace(TODO,'*','')
FROM          aux_codANT
WHERE ID/3.00 - round(ID/3.00,2) = 0


SELECT   ID,(ID+2)  , convert(int, replace(TODO,'*','')  )
FROM          aux_codANT
WHERE (ID-1)/3.00 - round((ID-1)/3.00,2) = 0

SELECT   (ID/3.00)-0.01,floor( (ID/3.00)-0.01)   ,*
FROM          aux_codANT A


-------------------------------
SELECT   *
update A set IDMAX = IDposta
FROM          aux_codANT A
inner join (
		SELECT   ID IDini , (ID+2) IDfin  , convert(int, replace(TODO,'*','')  ) IDposta
		FROM          aux_codANT
		WHERE (ID-1)/3.00 - round((ID-1)/3.00,2) = 0
		) VV
on A.ID between IDini and IDfin

-------------------------------
SELECT  *
FROM          aux_codANT A 


--UPDATE a set omitir = 'False'
--FROM          aux_codANT a
--WHERE omitir  is null


declare @e varchar(100) =  '''' + '.'

SELECT @e
SELECT R1.IDMAX , R2.TODO,
SUBSTRING(  rtrim(LTRIM(replace(R2.TODO ,      '02  FILLER  PIC X(78)   VALUE','') )) ,2,100)              ,
rtrim(LTRIM(substring(   replace(R3.TODO,@e ,'' ),36,100) ))
		FROM 		
				(SELECT IDMAX , TODO FROM (
				SELECT  *, ROW_NUMBER() OVER(PARTITION BY IDMAX ORDER BY ID) AS RowNumber
				FROM          aux_codANT A 
				WHERE Omitir = 0
				) X1
		WHERE RowNumber = 1 )  R1
		inner join 
				(SELECT  IDMAX , TODO  FROM (
				SELECT  *, ROW_NUMBER() OVER(PARTITION BY IDMAX ORDER BY ID) AS RowNumber
				FROM          aux_codANT A
				WHERE Omitir = 0
				) X2
		WHERE RowNumber = 2  ) R2
		on R1.IDMAX = R2.IDMAX
		inner join 
				(SELECT  IDMAX , TODO  FROM (
				SELECT  *, ROW_NUMBER() OVER(PARTITION BY IDMAX ORDER BY ID) AS RowNumber
				FROM          aux_codANT A
				WHERE Omitir = 0
				) X3
		WHERE RowNumber = 3  ) R3
		on R2.IDMAX = R3.IDMAX


   ---        02  FILLER  PIC X(78)   VALUE '001001001001001001001001001001

