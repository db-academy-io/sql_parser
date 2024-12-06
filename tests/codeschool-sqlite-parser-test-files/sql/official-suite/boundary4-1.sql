-- original: boundary4.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(a,x);
    INSERT INTO t1(oid,a,x) VALUES(549755813887,1,'0000007fffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-8388608,2,'ffffffffff800000');
    INSERT INTO t1(oid,a,x) VALUES(0,3,'0000000000000000');
    INSERT INTO t1(oid,a,x) VALUES(-129,4,'ffffffffffffff7f');
    INSERT INTO t1(oid,a,x) VALUES(8388608,5,'0000000000800000');
    INSERT INTO t1(oid,a,x) VALUES(65535,6,'000000000000ffff');
    INSERT INTO t1(oid,a,x) VALUES(8388607,7,'00000000007fffff');
    INSERT INTO t1(oid,a,x) VALUES(1099511627776,8,'0000010000000000');
    INSERT INTO t1(oid,a,x) VALUES(16777215,9,'0000000000ffffff');
    INSERT INTO t1(oid,a,x) VALUES(32767,10,'0000000000007fff');
    INSERT INTO t1(oid,a,x) VALUES(4294967296,11,'0000000100000000');
    INSERT INTO t1(oid,a,x) VALUES(-549755813888,12,'ffffff8000000000');
    INSERT INTO t1(oid,a,x) VALUES(-140737488355328,13,'ffff800000000000');
    INSERT INTO t1(oid,a,x) VALUES(256,14,'0000000000000100');
    INSERT INTO t1(oid,a,x) VALUES(16777216,15,'0000000001000000');
    INSERT INTO t1(oid,a,x) VALUES(72057594037927936,16,'0100000000000000');
    INSERT INTO t1(oid,a,x) VALUES(-1,17,'ffffffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(9223372036854775807,18,'7fffffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(281474976710655,19,'0000ffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(1099511627775,20,'000000ffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-8388609,21,'ffffffffff7fffff');
    INSERT INTO t1(oid,a,x) VALUES(32768,22,'0000000000008000');
    INSERT INTO t1(oid,a,x) VALUES(36028797018963968,23,'0080000000000000');
    INSERT INTO t1(oid,a,x) VALUES(-32769,24,'ffffffffffff7fff');
    INSERT INTO t1(oid,a,x) VALUES(127,25,'000000000000007f');
    INSERT INTO t1(oid,a,x) VALUES(-9223372036854775808,26,'8000000000000000');
    INSERT INTO t1(oid,a,x) VALUES(72057594037927935,27,'00ffffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-549755813889,28,'ffffff7fffffffff');
    INSERT INTO t1(oid,a,x) VALUES(255,29,'00000000000000ff');
    INSERT INTO t1(oid,a,x) VALUES(-36028797018963969,30,'ff7fffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-2147483648,31,'ffffffff80000000');
    INSERT INTO t1(oid,a,x) VALUES(281474976710656,32,'0001000000000000');
    INSERT INTO t1(oid,a,x) VALUES(65536,33,'0000000000010000');
    INSERT INTO t1(oid,a,x) VALUES(140737488355328,34,'0000800000000000');
    INSERT INTO t1(oid,a,x) VALUES(549755813888,35,'0000008000000000');
    INSERT INTO t1(oid,a,x) VALUES(2147483648,36,'0000000080000000');
    INSERT INTO t1(oid,a,x) VALUES(4294967295,37,'00000000ffffffff');
    INSERT INTO t1(oid,a,x) VALUES(140737488355327,38,'00007fffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-2147483649,39,'ffffffff7fffffff');
    INSERT INTO t1(oid,a,x) VALUES(36028797018963967,40,'007fffffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(128,41,'0000000000000080');
    INSERT INTO t1(oid,a,x) VALUES(-32768,42,'ffffffffffff8000');
    INSERT INTO t1(oid,a,x) VALUES(-36028797018963968,43,'ff80000000000000');
    INSERT INTO t1(oid,a,x) VALUES(-140737488355329,44,'ffff7fffffffffff');
    INSERT INTO t1(oid,a,x) VALUES(-128,45,'ffffffffffffff80');
    INSERT INTO t1(oid,a,x) VALUES(2147483647,46,'000000007fffffff');
    CREATE INDEX t1i1 ON t1(a);
    CREATE INDEX t1i2 ON t1(x)
;SELECT count(*) FROM t1
;SELECT rowid, a, x FROM t1 ORDER BY +rowid
;SELECT rowid, a, x FROM t1 ORDER BY rowid
;SELECT rowid, a, x FROM t1 ORDER BY +rowid DESC
;SELECT rowid, a, x FROM t1 ORDER BY rowid DESC
;SELECT rowid, a, x FROM t1 ORDER BY +a
;SELECT rowid, a, x FROM t1 ORDER BY a
;SELECT rowid, a, x FROM t1 ORDER BY +a DESC
;SELECT rowid, a, x FROM t1 ORDER BY a DESC
;SELECT rowid, a, x FROM t1 ORDER BY +x
;SELECT rowid, a, x FROM t1 ORDER BY x
;SELECT rowid, a, x FROM t1 ORDER BY +x DESC
;SELECT rowid, a, x FROM t1 ORDER BY x DESC
;UPDATE t1 SET rowid=a, a=rowid
;SELECT a, rowid, x FROM t1 ORDER BY +a
;SELECT a, rowid, x FROM t1 ORDER BY a
;SELECT a, rowid, x FROM t1 ORDER BY +a DESC
;SELECT a, rowid, x FROM t1 ORDER BY a DESC
;SELECT a, rowid, x FROM t1 ORDER BY +rowid
;SELECT a, rowid, x FROM t1 ORDER BY rowid
;SELECT a, rowid, x FROM t1 ORDER BY +rowid DESC
;SELECT a, rowid, x FROM t1 ORDER BY rowid DESC
;SELECT a, rowid, x FROM t1 ORDER BY +x
;SELECT a, rowid, x FROM t1 ORDER BY x
;SELECT a, rowid, x FROM t1 ORDER BY +x DESC
;SELECT a, rowid, x FROM t1 ORDER BY x DESC
;UPDATE t1 SET rowid=a, a=rowid
;ALTER TABLE t1 ADD COLUMN z; UPDATE t1 SET z=zeroblob(600)
;SELECT rowid, a, x FROM t1 ORDER BY +rowid
;SELECT rowid, a, x FROM t1 ORDER BY rowid
;SELECT rowid, a, x FROM t1 ORDER BY +rowid DESC
;SELECT rowid, a, x FROM t1 ORDER BY rowid DESC
;SELECT rowid, a, x FROM t1 ORDER BY +a
;SELECT rowid, a, x FROM t1 ORDER BY a
;SELECT rowid, a, x FROM t1 ORDER BY +a DESC
;SELECT rowid, a, x FROM t1 ORDER BY a DESC
;SELECT rowid, a, x FROM t1 ORDER BY +x
;SELECT rowid, a, x FROM t1 ORDER BY x
;SELECT rowid, a, x FROM t1 ORDER BY +x DESC
;SELECT rowid, a, x FROM t1 ORDER BY x DESC
;UPDATE t1 SET rowid=a, a=rowid, x=z, z=x
;SELECT a, rowid, z FROM t1 ORDER BY +a
;SELECT a, rowid, z FROM t1 ORDER BY a
;SELECT a, rowid, z FROM t1 ORDER BY +a DESC
;SELECT a, rowid, z FROM t1 ORDER BY a DESC
;SELECT a, rowid, z FROM t1 ORDER BY +rowid
;SELECT a, rowid, z FROM t1 ORDER BY rowid
;SELECT a, rowid, z FROM t1 ORDER BY +rowid DESC
;SELECT a, rowid, z FROM t1 ORDER BY rowid DESC
;SELECT a, rowid, z FROM t1 ORDER BY +z
;SELECT a, rowid, z FROM t1 ORDER BY z
;SELECT a, rowid, z FROM t1 ORDER BY +z DESC
;SELECT a, rowid, z FROM t1 ORDER BY z DESC;