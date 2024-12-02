-- where2.test
-- 
-- execsql {
--       SELECT w FROM tx
--        WHERE x IN (SELECT x FROM t1 WHERE w BETWEEN 10 AND 20)
--          AND y IN (SELECT y FROM t1 WHERE w BETWEEN 200 AND 300)
--          AND z IN (SELECT z FROM t1 WHERE w BETWEEN 10 AND 20)
-- }
SELECT w FROM tx
WHERE x IN (SELECT x FROM t1 WHERE w BETWEEN 10 AND 20)
AND y IN (SELECT y FROM t1 WHERE w BETWEEN 200 AND 300)
AND z IN (SELECT z FROM t1 WHERE w BETWEEN 10 AND 20)