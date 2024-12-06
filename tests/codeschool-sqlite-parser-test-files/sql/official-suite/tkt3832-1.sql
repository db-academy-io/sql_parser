-- original: tkt3832.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a INT, b INTEGER PRIMARY KEY);
    CREATE TABLE log(x);
    CREATE TRIGGER t1r1 BEFORE INSERT ON t1 BEGIN
      INSERT INTO log VALUES(new.b);
    END;
    INSERT INTO t1 VALUES(NULL,5);
    INSERT INTO t1 SELECT b, a FROM t1 ORDER BY b;
    SELECT rowid, * FROM t1;
    SELECT rowid, * FROM log;