-- fkey3.test
-- 
-- execsql {
--     PRAGMA foreign_keys=ON;
--     CREATE TABLE t1(x INTEGER PRIMARY KEY);
--     INSERT INTO t1 VALUES(100);
--     INSERT INTO t1 VALUES(101);
--     CREATE TABLE t2(y INTEGER PRIMARY KEY REFERENCES t1 (x) ON UPDATE SET NULL);
-- }
PRAGMA foreign_keys=ON;
CREATE TABLE t1(x INTEGER PRIMARY KEY);
INSERT INTO t1 VALUES(100);
INSERT INTO t1 VALUES(101);
CREATE TABLE t2(y INTEGER PRIMARY KEY REFERENCES t1 (x) ON UPDATE SET NULL);