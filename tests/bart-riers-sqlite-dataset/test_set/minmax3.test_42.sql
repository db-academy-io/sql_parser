-- minmax3.test
-- 
-- execsql {
--     SELECT min(x), min(x COLLATE nocase) FROM t4;
-- }
SELECT min(x), min(x COLLATE nocase) FROM t4;