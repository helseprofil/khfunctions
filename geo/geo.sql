-- SQL syntax for Access
SELECT tblFylke.fylkeCode, tblFylke.fylkeName, tblKommune.kommuneCode, tblKommune.kommuneName
 FROM tblKommune
 LEFT JOIN tblFylke ON tblKommune.[fylkeCode] = tblFylke.[fylkeCode]
 WHERE (((tblFylke.fylkeCode) Is Not NULL))
 ORDER BY tblFylke.fylkeCode;

-- Geo basic with Fylke, Kommune and Grunnkrets
SELECT tblFylke.fylkeCode, tblKommune.kommuneCode, tblGrunnkrets.grunnkretsCode
FROM (tblFylke
 LEFT JOIN tblKommune ON tblFylke.fylkeCode = tblKommune.fylkeCode)
 LEFT JOIN tblGrunnkrets ON tblKommune.kommuneCode = tblGrunnkrets.kommuneCode
ORDER BY tblFylke.fylkeCode;

-- Geo extended with bydel etc
SELECT tblFylke.fylkeCode, tblKommune.kommuneCode, tblBydel.bydelCode
FROM (tblFylke
 LEFT JOIN tblKommune ON tblFylke.fylkeCode = tblKommune.fylkeCode)
 LEFT JOIN tblBydel ON tblKommune.kommuneCode = tblBydel.kommuneCode
ORDER BY tblFylke.fylkeCode;


-- Access has no FULL OUTER JOIN so have to do LEFT and RIGHT JOIN then UNION
SELECT
*
FROM Table_1
LEFT JOIN Table_2
ON Table_2.ID = Table_1.ID

UNION

SELECT
*
FROM Table_1
RIGHT JOIN Table_2
ON Table_2.ID = Table_1.ID
