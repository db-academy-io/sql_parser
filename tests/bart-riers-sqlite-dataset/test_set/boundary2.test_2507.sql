-- boundary2.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY a
-- }
SELECT a FROM t1 WHERE r >= 4398046511103 ORDER BY a