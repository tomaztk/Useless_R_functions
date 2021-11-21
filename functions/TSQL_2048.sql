USE QL;


DECLARE @dim INT = 4
DECLARE @i INT  = 1

DECLARE @TableCreate VARCHAR(2000) = 
'DROP TABLE IF EXISTS dbo.T_2048
CREATE TABLE dbo.T_2048 ('

WHILE (@dim >= @i)

BEGIN
	SET @TableCreate = @TableCreate + 'V' + CAST(@i AS VARCHAR(10)) + ' SMALLINT ,'
	SET @i = @i + 1
END
SET @TableCreate = STUFF(@TableCreate, LEN(@TableCreate), 1, ')')

SET @TableCreate = '
INSERT INTO dbo.T_2048 VALUES ('

GO 4


EXEC sp_sqlexec @tableCreate



SELECT * FROM T_2048
