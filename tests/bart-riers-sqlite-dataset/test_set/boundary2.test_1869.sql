-- boundary2.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE r < 9223372036854775807 ORDER BY a DESC
-- }
SELECT a FROM t1 WHERE r < 9223372036854775807 ORDER BY a DESC