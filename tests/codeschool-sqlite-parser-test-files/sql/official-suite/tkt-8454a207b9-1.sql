-- original: tkt-8454a207b9.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a);
    INSERT INTO t1 VALUES(1);
    ALTER TABLE t1 ADD COLUMN b TEXT DEFAULT -123.0;
    SELECT b, typeof(b) FROM t1
;ALTER TABLE t1 ADD COLUMN c TEXT DEFAULT -123.5;
    SELECT c, typeof(c) FROM t1
;ALTER TABLE t1 ADD COLUMN d TEXT DEFAULT -'hello';
    SELECT d, typeof(d) FROM t1
;ALTER TABLE t1 ADD COLUMN e DEFAULT -123.0;
    SELECT e, typeof(e) FROM t1
;ALTER TABLE t1 ADD COLUMN f DEFAULT -123.5;
    SELECT f, typeof(f) FROM t1
;ALTER TABLE t1 ADD COLUMN g DEFAULT -9223372036854775808;
    SELECT g, typeof(g) FROM t1
;ALTER TABLE t1 ADD COLUMN h DEFAULT 9223372036854775807;
    SELECT h, typeof(h) FROM t1;