-- boundary3.test
-- 
-- db eval {
--     SELECT t2.* FROM t1 JOIN t2 USING(a) WHERE x='00000000001fffff'
-- }
SELECT t2.* FROM t1 JOIN t2 USING(a) WHERE x='00000000001fffff'