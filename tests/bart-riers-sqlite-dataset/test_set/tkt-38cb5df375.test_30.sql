-- tkt-38cb5df375.test
-- 
-- execsql {
--       SELECT a FROM (SELECT * FROM t1 ORDER BY a)
--       EXCEPT SELECT a FROM (SELECT a FROM t1 ORDER BY a LIMIT ii)
--       ORDER BY a DESC
--       LIMIT jj;
-- }
SELECT a FROM (SELECT * FROM t1 ORDER BY a)
EXCEPT SELECT a FROM (SELECT a FROM t1 ORDER BY a LIMIT ii)
ORDER BY a DESC
LIMIT jj;
