-- original: boundary1.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

SELECT a FROM t1 WHERE rowid >= 255 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid >= 255 ORDER BY x
;SELECT a FROM t1 WHERE rowid < 255 ORDER BY a
;SELECT a FROM t1 WHERE rowid < 255 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid < 255 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid < 255 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid < 255 ORDER BY x
;SELECT a FROM t1 WHERE rowid <= 255 ORDER BY a
;SELECT a FROM t1 WHERE rowid <= 255 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid <= 255 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid <= 255 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid <= 255 ORDER BY x
;SELECT * FROM t1 WHERE rowid=-2147483648
;SELECT rowid, a FROM t1 WHERE x='ffffffff80000000'
;SELECT rowid, x FROM t1 WHERE a=11
;SELECT a FROM t1 WHERE rowid > -2147483648 ORDER BY a
;SELECT a FROM t1 WHERE rowid > -2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid > -2147483648 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid > -2147483648 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid > -2147483648 ORDER BY x
;SELECT a FROM t1 WHERE rowid >= -2147483648 ORDER BY a
;SELECT a FROM t1 WHERE rowid >= -2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid >= -2147483648 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid >= -2147483648 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid >= -2147483648 ORDER BY x
;SELECT a FROM t1 WHERE rowid < -2147483648 ORDER BY a
;SELECT a FROM t1 WHERE rowid < -2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid < -2147483648 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid < -2147483648 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid < -2147483648 ORDER BY x
;SELECT a FROM t1 WHERE rowid <= -2147483648 ORDER BY a
;SELECT a FROM t1 WHERE rowid <= -2147483648 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid <= -2147483648 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid <= -2147483648 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid <= -2147483648 ORDER BY x
;SELECT * FROM t1 WHERE rowid=34359738367
;SELECT rowid, a FROM t1 WHERE x='00000007ffffffff'
;SELECT rowid, x FROM t1 WHERE a=39
;SELECT a FROM t1 WHERE rowid > 34359738367 ORDER BY a
;SELECT a FROM t1 WHERE rowid > 34359738367 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid > 34359738367 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid > 34359738367 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid > 34359738367 ORDER BY x
;SELECT a FROM t1 WHERE rowid >= 34359738367 ORDER BY a
;SELECT a FROM t1 WHERE rowid >= 34359738367 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid >= 34359738367 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid >= 34359738367 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid >= 34359738367 ORDER BY x
;SELECT a FROM t1 WHERE rowid < 34359738367 ORDER BY a
;SELECT a FROM t1 WHERE rowid < 34359738367 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid < 34359738367 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid < 34359738367 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid < 34359738367 ORDER BY x
;SELECT a FROM t1 WHERE rowid <= 34359738367 ORDER BY a
;SELECT a FROM t1 WHERE rowid <= 34359738367 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid <= 34359738367 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid <= 34359738367 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid <= 34359738367 ORDER BY x
;SELECT * FROM t1 WHERE rowid=-549755813889
;SELECT rowid, a FROM t1 WHERE x='ffffff7fffffffff'
;SELECT rowid, x FROM t1 WHERE a=58
;SELECT a FROM t1 WHERE rowid > -549755813889 ORDER BY a
;SELECT a FROM t1 WHERE rowid > -549755813889 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid > -549755813889 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid > -549755813889 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid > -549755813889 ORDER BY x
;SELECT a FROM t1 WHERE rowid >= -549755813889 ORDER BY a
;SELECT a FROM t1 WHERE rowid >= -549755813889 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid >= -549755813889 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid >= -549755813889 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid >= -549755813889 ORDER BY x
;SELECT a FROM t1 WHERE rowid < -549755813889 ORDER BY a
;SELECT a FROM t1 WHERE rowid < -549755813889 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid < -549755813889 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid < -549755813889 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid < -549755813889 ORDER BY x
;SELECT a FROM t1 WHERE rowid <= -549755813889 ORDER BY a
;SELECT a FROM t1 WHERE rowid <= -549755813889 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid <= -549755813889 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid <= -549755813889 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid <= -549755813889 ORDER BY x
;SELECT * FROM t1 WHERE rowid=-32768
;SELECT rowid, a FROM t1 WHERE x='ffffffffffff8000'
;SELECT rowid, x FROM t1 WHERE a=32
;SELECT a FROM t1 WHERE rowid > -32768 ORDER BY a
;SELECT a FROM t1 WHERE rowid > -32768 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid > -32768 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid > -32768 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid > -32768 ORDER BY x
;SELECT a FROM t1 WHERE rowid >= -32768 ORDER BY a
;SELECT a FROM t1 WHERE rowid >= -32768 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid >= -32768 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid >= -32768 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid >= -32768 ORDER BY x
;SELECT a FROM t1 WHERE rowid < -32768 ORDER BY a
;SELECT a FROM t1 WHERE rowid < -32768 ORDER BY a DESC
;SELECT a FROM t1 WHERE rowid < -32768 ORDER BY rowid
;SELECT a FROM t1 WHERE rowid < -32768 ORDER BY rowid DESC
;SELECT a FROM t1 WHERE rowid < -32768 ORDER BY x
;SELECT a FROM t1 WHERE rowid <= -32768 ORDER BY a;