-- fts1e.test
-- 
-- execsql {SELECT rowid FROM t1 WHERE content MATCH 'five'}
SELECT rowid FROM t1 WHERE content MATCH 'five'