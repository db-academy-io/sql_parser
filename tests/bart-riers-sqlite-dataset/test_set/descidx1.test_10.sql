-- descidx1.test
-- 
-- execsql {
--       CREATE TABLE t2(a INT, b TEXT, c BLOB, d REAL);
--       CREATE INDEX i3 ON t2(a ASC, b DESC, c ASC);
--       CREATE INDEX i4 ON t2(b DESC, a ASC, d DESC);
--       INSERT INTO t2 VALUES(1,'one',x'31',1.0);
--       INSERT INTO t2 VALUES(2,'two',x'3232',2.0);
--       INSERT INTO t2 VALUES(3,'three',x'333333',3.0);
--       INSERT INTO t2 VALUES(4,'four',x'34343434',4.0);
--       INSERT INTO t2 VALUES(5,'five',x'3535353535',5.0);
--       INSERT INTO t2 VALUES(6,'six',x'363636363636',6.0);
--       INSERT INTO t2 VALUES(2,'two',x'323232',2.1);
--       INSERT INTO t2 VALUES(2,'zwei',x'3232',2.2);
--       INSERT INTO t2 VALUES(2,NULL,NULL,2.3);
--       SELECT count(*) FROM t2;
-- }
CREATE TABLE t2(a INT, b TEXT, c BLOB, d REAL);
CREATE INDEX i3 ON t2(a ASC, b DESC, c ASC);
CREATE INDEX i4 ON t2(b DESC, a ASC, d DESC);
INSERT INTO t2 VALUES(1,'one',x'31',1.0);
INSERT INTO t2 VALUES(2,'two',x'3232',2.0);
INSERT INTO t2 VALUES(3,'three',x'333333',3.0);
INSERT INTO t2 VALUES(4,'four',x'34343434',4.0);
INSERT INTO t2 VALUES(5,'five',x'3535353535',5.0);
INSERT INTO t2 VALUES(6,'six',x'363636363636',6.0);
INSERT INTO t2 VALUES(2,'two',x'323232',2.1);
INSERT INTO t2 VALUES(2,'zwei',x'3232',2.2);
INSERT INTO t2 VALUES(2,NULL,NULL,2.3);
SELECT count(*) FROM t2;