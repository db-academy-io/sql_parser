-- boundary1.test
-- 
-- db eval {
--     SELECT a FROM t1 WHERE rowid > 562949953421312 ORDER BY a DESC
-- }
SELECT a FROM t1 WHERE rowid > 562949953421312 ORDER BY a DESC