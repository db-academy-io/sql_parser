-- original: whereH.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a,b,c,d);
  CREATE INDEX t1abc ON t1(a,b,c);
  CREATE INDEX t1bc ON t1(b,c);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c>=? ORDER BY c
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c>=? ORDER BY c
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d);
  CREATE INDEX t1bc ON t1(b,c);
  CREATE INDEX t1abc ON t1(a,b,c);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c>=? ORDER BY c
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c>=? ORDER BY c
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1cd ON t1(c,d);
  CREATE INDEX t1bcd ON t1(b,c,d);
  CREATE INDEX t1abcd ON t1(a,b,c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1cd ON t1(c,d);
  CREATE INDEX t1abcd ON t1(a,b,c,d);
  CREATE INDEX t1bcd ON t1(b,c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1bcd ON t1(b,c,d);
  CREATE INDEX t1cd ON t1(c,d);
  CREATE INDEX t1abcd ON t1(a,b,c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1bcd ON t1(b,c,d);
  CREATE INDEX t1abcd ON t1(a,b,c,d);
  CREATE INDEX t1cd ON t1(c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1abcd ON t1(a,b,c,d);
  CREATE INDEX t1bcd ON t1(b,c,d);
  CREATE INDEX t1cd ON t1(c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;DROP TABLE t1;
  CREATE TABLE t1(a,b,c,d,e);
  CREATE INDEX t1abcd ON t1(a,b,c,d);
  CREATE INDEX t1cd ON t1(c,d);
  CREATE INDEX t1bcd ON t1(b,c,d);

  EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d
;EXPLAIN QUERY PLAN
  SELECT d FROM t1 WHERE a=? AND b=? AND c=? AND d>=? ORDER BY d;