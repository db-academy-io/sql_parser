-- boundary2.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY a
-- }
SELECT a FROM t1 WHERE r > 281474976710656 ORDER BY a