-- func.test
-- 
-- execsql {
--      CREATE TABLE t2(a);
--      INSERT INTO t2 VALUES(1);
--      INSERT INTO t2 VALUES(NULL);
--      INSERT INTO t2 VALUES(345);
--      INSERT INTO t2 VALUES(NULL);
--      INSERT INTO t2 VALUES(67890);
--      SELECT * FROM t2;
-- }
CREATE TABLE t2(a);
INSERT INTO t2 VALUES(1);
INSERT INTO t2 VALUES(NULL);
INSERT INTO t2 VALUES(345);
INSERT INTO t2 VALUES(NULL);
INSERT INTO t2 VALUES(67890);
SELECT * FROM t2;