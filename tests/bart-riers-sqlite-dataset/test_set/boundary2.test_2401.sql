-- boundary2.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE r < 72057594037927936 ORDER BY x
-- }
SELECT a FROM t1 WHERE r < 72057594037927936 ORDER BY x