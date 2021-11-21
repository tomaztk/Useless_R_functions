USE QL;


DECLARE @dim INT = 4

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CREATE TABLE for @dim dimension
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DECLARE @i INT = 1
DECLARE @j INT = 1

DECLARE @TableCreate NVARCHAR(2000) = 
'DROP TABLE IF EXISTS dbo.T_2048; 
CREATE TABLE dbo.T_2048 ('

WHILE (@dim >= @i)

BEGIN
	SET @TableCreate = @TableCreate + 'V' + CAST(@i AS VARCHAR(10)) + ' SMALLINT ,'
	SET @i = @i + 1
END
SET @TableCreate = STUFF(@TableCreate, LEN(@TableCreate), 1, ');')

WHILE (@dim >= @j)
BEGIN
	SET @TableCreate = @TableCreate + ' 
	INSERT INTO dbo.T_2048 VALUES ('
	+ STUFF(REPLICATE('0,',@dim), LEN(REPLICATE('0,',@dim)), 1, ');') 
	SET @j = @j+1
END

EXEC sp_executesql @tableCreate

SELECT * FROM T_2048
