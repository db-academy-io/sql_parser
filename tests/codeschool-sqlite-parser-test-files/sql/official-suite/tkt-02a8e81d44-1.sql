-- original: tkt-02a8e81d44.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a);
    INSERT INTO t1 VALUES(1);
    INSERT INTO t1 VALUES(2);
    INSERT INTO t1 VALUES(4);
    INSERT INTO t1 VALUES(5);
    SELECT * FROM (SELECT a FROM t1 LIMIT 1) UNION ALL SELECT 3;