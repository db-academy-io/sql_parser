-- original: boundary2.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

SELECT a FROM t1 WHERE r > 2097151 ORDER BY r
;SELECT a FROM t1 WHERE r > 2097151 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 2097151 ORDER BY x
;SELECT a FROM t1 WHERE r >= 2097151 ORDER BY a
;SELECT a FROM t1 WHERE r >= 2097151 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 2097151 ORDER BY r
;SELECT a FROM t1 WHERE r >= 2097151 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 2097151 ORDER BY x
;SELECT a FROM t1 WHERE r < 2097151 ORDER BY a
;SELECT a FROM t1 WHERE r < 2097151 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 2097151 ORDER BY r
;SELECT a FROM t1 WHERE r < 2097151 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 2097151 ORDER BY x
;SELECT a FROM t1 WHERE r <= 2097151 ORDER BY a
;SELECT a FROM t1 WHERE r <= 2097151 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 2097151 ORDER BY r
;SELECT a FROM t1 WHERE r <= 2097151 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 2097151 ORDER BY x
;SELECT * FROM t1 WHERE r=140737488355327
;SELECT r, a FROM t1 WHERE x='00007fffffffffff'
;SELECT r, x FROM t1 WHERE a=25
;SELECT a FROM t1 WHERE r > 140737488355327 ORDER BY a
;SELECT a FROM t1 WHERE r > 140737488355327 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 140737488355327 ORDER BY r
;SELECT a FROM t1 WHERE r > 140737488355327 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 140737488355327 ORDER BY x
;SELECT a FROM t1 WHERE r >= 140737488355327 ORDER BY a
;SELECT a FROM t1 WHERE r >= 140737488355327 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 140737488355327 ORDER BY r
;SELECT a FROM t1 WHERE r >= 140737488355327 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 140737488355327 ORDER BY x
;SELECT a FROM t1 WHERE r < 140737488355327 ORDER BY a
;SELECT a FROM t1 WHERE r < 140737488355327 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 140737488355327 ORDER BY r
;SELECT a FROM t1 WHERE r < 140737488355327 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 140737488355327 ORDER BY x
;SELECT a FROM t1 WHERE r <= 140737488355327 ORDER BY a
;SELECT a FROM t1 WHERE r <= 140737488355327 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 140737488355327 ORDER BY r
;SELECT a FROM t1 WHERE r <= 140737488355327 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 140737488355327 ORDER BY x
;SELECT * FROM t1 WHERE r=281474976710656
;SELECT r, a FROM t1 WHERE x='0001000000000000'
;SELECT r, x FROM t1 WHERE a=26
;SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY a
;SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY r
;SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY x
;SELECT a FROM t1 WHERE r >= 281474976710656 ORDER BY a
;SELECT a FROM t1 WHERE r >= 281474976710656 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 281474976710656 ORDER BY r
;SELECT a FROM t1 WHERE r >= 281474976710656 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 281474976710656 ORDER BY x
;SELECT a FROM t1 WHERE r < 281474976710656 ORDER BY a
;SELECT a FROM t1 WHERE r < 281474976710656 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 281474976710656 ORDER BY r
;SELECT a FROM t1 WHERE r < 281474976710656 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 281474976710656 ORDER BY x
;SELECT a FROM t1 WHERE r <= 281474976710656 ORDER BY a
;SELECT a FROM t1 WHERE r <= 281474976710656 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 281474976710656 ORDER BY r
;SELECT a FROM t1 WHERE r <= 281474976710656 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 281474976710656 ORDER BY x
;SELECT * FROM t1 WHERE r=32767
;SELECT r, a FROM t1 WHERE x='0000000000007fff'
;SELECT r, x FROM t1 WHERE a=23
;SELECT a FROM t1 WHERE r > 32767 ORDER BY a
;SELECT a FROM t1 WHERE r > 32767 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 32767 ORDER BY r
;SELECT a FROM t1 WHERE r > 32767 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 32767 ORDER BY x
;SELECT a FROM t1 WHERE r >= 32767 ORDER BY a
;SELECT a FROM t1 WHERE r >= 32767 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 32767 ORDER BY r
;SELECT a FROM t1 WHERE r >= 32767 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 32767 ORDER BY x
;SELECT a FROM t1 WHERE r < 32767 ORDER BY a
;SELECT a FROM t1 WHERE r < 32767 ORDER BY a DESC
;SELECT a FROM t1 WHERE r < 32767 ORDER BY r
;SELECT a FROM t1 WHERE r < 32767 ORDER BY r DESC
;SELECT a FROM t1 WHERE r < 32767 ORDER BY x
;SELECT a FROM t1 WHERE r <= 32767 ORDER BY a
;SELECT a FROM t1 WHERE r <= 32767 ORDER BY a DESC
;SELECT a FROM t1 WHERE r <= 32767 ORDER BY r
;SELECT a FROM t1 WHERE r <= 32767 ORDER BY r DESC
;SELECT a FROM t1 WHERE r <= 32767 ORDER BY x
;SELECT * FROM t1 WHERE r=127
;SELECT r, a FROM t1 WHERE x='000000000000007f'
;SELECT r, x FROM t1 WHERE a=4
;SELECT a FROM t1 WHERE r > 127 ORDER BY a
;SELECT a FROM t1 WHERE r > 127 ORDER BY a DESC
;SELECT a FROM t1 WHERE r > 127 ORDER BY r
;SELECT a FROM t1 WHERE r > 127 ORDER BY r DESC
;SELECT a FROM t1 WHERE r > 127 ORDER BY x
;SELECT a FROM t1 WHERE r >= 127 ORDER BY a
;SELECT a FROM t1 WHERE r >= 127 ORDER BY a DESC
;SELECT a FROM t1 WHERE r >= 127 ORDER BY r
;SELECT a FROM t1 WHERE r >= 127 ORDER BY r DESC
;SELECT a FROM t1 WHERE r >= 127 ORDER BY x;