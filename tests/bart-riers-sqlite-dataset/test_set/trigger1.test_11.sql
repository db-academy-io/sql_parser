-- trigger1.test
-- 
-- execsql {
--       INSERT INTO t1 VALUES(5,6);
--       SELECT * FROM t1 UNION ALL SELECT * FROM t2;
-- }
INSERT INTO t1 VALUES(5,6);
SELECT * FROM t1 UNION ALL SELECT * FROM t2;