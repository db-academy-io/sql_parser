-- original: boundary2.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

SELECT a FROM t1 WHERE r > 2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 2147483648 ORDER BY r
;SELECT a FROM t1 WHERE r > 2147483648 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 2147483648 ORDER BY x
;SELECT a FROM t1 WHERE r >= 2147483648 ORDER BY a
;SELECT a FROM t1 WHERE r >= 2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 2147483648 ORDER BY r
;SELECT a FROM t1 WHERE r >= 2147483648 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 2147483648 ORDER BY x
;SELECT a FROM t1 WHERE r < 2147483648 ORDER BY a
;SELECT a FROM t1 WHERE r < 2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 2147483648 ORDER BY r
;SELECT a FROM t1 WHERE r < 2147483648 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 2147483648 ORDER BY x
;SELECT a FROM t1 WHERE r <= 2147483648 ORDER BY a
;SELECT a FROM t1 WHERE r <= 2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 2147483648 ORDER BY r
;SELECT a FROM t1 WHERE r <= 2147483648 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 2147483648 ORDER BY x
;SELECT * FROM t1 WHERE r=549755813887
;SELECT r, a FROM t1 WHERE x='0000007fffffffff'
;SELECT r, x FROM t1 WHERE a=46
;SELECT a FROM t1 WHERE r > 549755813887 ORDER BY a
;SELECT a FROM t1 WHERE r > 549755813887 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 549755813887 ORDER BY r
;SELECT a FROM t1 WHERE r > 549755813887 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 549755813887 ORDER BY x
;SELECT a FROM t1 WHERE r >= 549755813887 ORDER BY a
;SELECT a FROM t1 WHERE r >= 549755813887 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 549755813887 ORDER BY r
;SELECT a FROM t1 WHERE r >= 549755813887 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 549755813887 ORDER BY x
;SELECT a FROM t1 WHERE r < 549755813887 ORDER BY a
;SELECT a FROM t1 WHERE r < 549755813887 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 549755813887 ORDER BY r
;SELECT a FROM t1 WHERE r < 549755813887 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 549755813887 ORDER BY x
;SELECT a FROM t1 WHERE r <= 549755813887 ORDER BY a
;SELECT a FROM t1 WHERE r <= 549755813887 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 549755813887 ORDER BY r
;SELECT a FROM t1 WHERE r <= 549755813887 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 549755813887 ORDER BY x
;SELECT * FROM t1 WHERE r=-549755813888
;SELECT r, a FROM t1 WHERE x='ffffff8000000000'
;SELECT r, x FROM t1 WHERE a=63
;SELECT a FROM t1 WHERE r > -549755813888 ORDER BY a
;SELECT a FROM t1 WHERE r > -549755813888 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > -549755813888 ORDER BY r
;SELECT a FROM t1 WHERE r > -549755813888 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > -549755813888 ORDER BY x
;SELECT a FROM t1 WHERE r >= -549755813888 ORDER BY a
;SELECT a FROM t1 WHERE r >= -549755813888 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= -549755813888 ORDER BY r
;SELECT a FROM t1 WHERE r >= -549755813888 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= -549755813888 ORDER BY x
;SELECT a FROM t1 WHERE r < -549755813888 ORDER BY a
;SELECT a FROM t1 WHERE r < -549755813888 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < -549755813888 ORDER BY r
;SELECT a FROM t1 WHERE r < -549755813888 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < -549755813888 ORDER BY x
;SELECT a FROM t1 WHERE r <= -549755813888 ORDER BY a
;SELECT a FROM t1 WHERE r <= -549755813888 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= -549755813888 ORDER BY r
;SELECT a FROM t1 WHERE r <= -549755813888 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= -549755813888 ORDER BY x
;SELECT * FROM t1 WHERE r=281474976710655
;SELECT r, a FROM t1 WHERE x='0000ffffffffffff'
;SELECT r, x FROM t1 WHERE a=10
;SELECT a FROM t1 WHERE r > 281474976710655 ORDER BY a
;SELECT a FROM t1 WHERE r > 281474976710655 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 281474976710655 ORDER BY r
;SELECT a FROM t1 WHERE r > 281474976710655 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 281474976710655 ORDER BY x
;SELECT a FROM t1 WHERE r >= 281474976710655 ORDER BY a
;SELECT a FROM t1 WHERE r >= 281474976710655 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 281474976710655 ORDER BY r
;SELECT a FROM t1 WHERE r >= 281474976710655 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 281474976710655 ORDER BY x
;SELECT a FROM t1 WHERE r < 281474976710655 ORDER BY a
;SELECT a FROM t1 WHERE r < 281474976710655 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 281474976710655 ORDER BY r
;SELECT a FROM t1 WHERE r < 281474976710655 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 281474976710655 ORDER BY x
;SELECT a FROM t1 WHERE r <= 281474976710655 ORDER BY a
;SELECT a FROM t1 WHERE r <= 281474976710655 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 281474976710655 ORDER BY r
;SELECT a FROM t1 WHERE r <= 281474976710655 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 281474976710655 ORDER BY x
;SELECT * FROM t1 WHERE r=4398046511103
;SELECT r, a FROM t1 WHERE x='000003ffffffffff'
;SELECT r, x FROM t1 WHERE a=7
;SELECT a FROM t1 WHERE r > 4398046511103 ORDER BY a
;SELECT a FROM t1 WHERE r > 4398046511103 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 4398046511103 ORDER BY r
;SELECT a FROM t1 WHERE r > 4398046511103 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 4398046511103 ORDER BY x
;SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY a
;SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY r
;SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY r DESC;