-- selectA.test
-- 
-- execsql {
--     SELECT a,b,c FROM t1 UNION ALL SELECT x,y,z FROM t2
--     ORDER BY b COLLATE NOCASE DESC,a,c
-- }
SELECT a,b,c FROM t1 UNION ALL SELECT x,y,z FROM t2
ORDER BY b COLLATE NOCASE DESC,a,c