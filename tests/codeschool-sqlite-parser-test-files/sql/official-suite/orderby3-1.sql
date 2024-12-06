-- original: orderby3.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a INTEGER PRIMARY KEY);
  CREATE TABLE t2(b INTEGER PRIMARY KEY, c INTEGER);
  CREATE TABLE t3(d INTEGER);
    
  INSERT INTO t1 VALUES(1),(2),(3);
    
  INSERT INTO t2 VALUES(3, 1);
  INSERT INTO t2 VALUES(4, 2);
  INSERT INTO t2 VALUES(5, 3);
    
  INSERT INTO t3 VALUES(4),(3),(5)
;SELECT t1.a
    FROM t1, t2, t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t1, t2, t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t1 CROSS JOIN t2 CROSS JOIN t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t1 CROSS JOIN t2 CROSS JOIN t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t1 CROSS JOIN t3 CROSS JOIN t2
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t1 CROSS JOIN t3 CROSS JOIN t2
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t2 CROSS JOIN t1 CROSS JOIN t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t2 CROSS JOIN t1 CROSS JOIN t3
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t2 CROSS JOIN t3 CROSS JOIN t1
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t2 CROSS JOIN t3 CROSS JOIN t1
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t3 CROSS JOIN t1 CROSS JOIN t2
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t3 CROSS JOIN t1 CROSS JOIN t2
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC
;SELECT t1.a
    FROM t3 CROSS JOIN t2 CROSS JOIN t1
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a
;SELECT t1.a
    FROM t3 CROSS JOIN t2 CROSS JOIN t1
   WHERE t1.a=t2.c AND t2.b=t3.d
   ORDER BY t1.a DESC;