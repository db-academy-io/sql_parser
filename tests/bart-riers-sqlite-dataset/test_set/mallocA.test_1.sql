-- mallocA.test
-- 
-- db eval {
--   CREATE TABLE t1(a COLLATE NOCASE,b,c);
--   INSERT INTO t1 VALUES(1,2,3);
--   INSERT INTO t1 VALUES(1,2,4);
--   INSERT INTO t1 VALUES(2,3,4);
--   CREATE INDEX t1i1 ON t1(a);
--   CREATE INDEX t1i2 ON t1(b,c);
--   CREATE TABLE t2(x,y,z);
-- }
CREATE TABLE t1(a COLLATE NOCASE,b,c);
INSERT INTO t1 VALUES(1,2,3);
INSERT INTO t1 VALUES(1,2,4);
INSERT INTO t1 VALUES(2,3,4);
CREATE INDEX t1i1 ON t1(a);
CREATE INDEX t1i2 ON t1(b,c);
CREATE TABLE t2(x,y,z);