-- fts2a.test
-- 
-- db eval {
--   CREATE VIRTUAL TABLE t1 USING fts2(content);
--   INSERT INTO t1(content) VALUES('one');
--   INSERT INTO t1(content) VALUES('two');
--   INSERT INTO t1(content) VALUES('one two');
--   INSERT INTO t1(content) VALUES('three');
--   INSERT INTO t1(content) VALUES('one three');
--   INSERT INTO t1(content) VALUES('two three');
--   INSERT INTO t1(content) VALUES('one two three');
--   INSERT INTO t1(content) VALUES('four');
--   INSERT INTO t1(content) VALUES('one four');
--   INSERT INTO t1(content) VALUES('two four');
--   INSERT INTO t1(content) VALUES('one two four');
--   INSERT INTO t1(content) VALUES('three four');
--   INSERT INTO t1(content) VALUES('one three four');
--   INSERT INTO t1(content) VALUES('two three four');
--   INSERT INTO t1(content) VALUES('one two three four');
--   INSERT INTO t1(content) VALUES('five');
--   INSERT INTO t1(content) VALUES('one five');
--   INSERT INTO t1(content) VALUES('two five');
--   INSERT INTO t1(content) VALUES('one two five');
--   INSERT INTO t1(content) VALUES('three five');
--   INSERT INTO t1(content) VALUES('one three five');
--   INSERT INTO t1(content) VALUES('two three five');
--   INSERT INTO t1(content) VALUES('one two three five');
--   INSERT INTO t1(content) VALUES('four five');
--   INSERT INTO t1(content) VALUES('one four five');
--   INSERT INTO t1(content) VALUES('two four five');
--   INSERT INTO t1(content) VALUES('one two four five');
--   INSERT INTO t1(content) VALUES('three four five');
--   INSERT INTO t1(content) VALUES('one three four five');
--   INSERT INTO t1(content) VALUES('two three four five');
--   INSERT INTO t1(content) VALUES('one two three four five');
-- }
CREATE VIRTUAL TABLE t1 USING fts2(content);
INSERT INTO t1(content) VALUES('one');
INSERT INTO t1(content) VALUES('two');
INSERT INTO t1(content) VALUES('one two');
INSERT INTO t1(content) VALUES('three');
INSERT INTO t1(content) VALUES('one three');
INSERT INTO t1(content) VALUES('two three');
INSERT INTO t1(content) VALUES('one two three');
INSERT INTO t1(content) VALUES('four');
INSERT INTO t1(content) VALUES('one four');
INSERT INTO t1(content) VALUES('two four');
INSERT INTO t1(content) VALUES('one two four');
INSERT INTO t1(content) VALUES('three four');
INSERT INTO t1(content) VALUES('one three four');
INSERT INTO t1(content) VALUES('two three four');
INSERT INTO t1(content) VALUES('one two three four');
INSERT INTO t1(content) VALUES('five');
INSERT INTO t1(content) VALUES('one five');
INSERT INTO t1(content) VALUES('two five');
INSERT INTO t1(content) VALUES('one two five');
INSERT INTO t1(content) VALUES('three five');
INSERT INTO t1(content) VALUES('one three five');
INSERT INTO t1(content) VALUES('two three five');
INSERT INTO t1(content) VALUES('one two three five');
INSERT INTO t1(content) VALUES('four five');
INSERT INTO t1(content) VALUES('one four five');
INSERT INTO t1(content) VALUES('two four five');
INSERT INTO t1(content) VALUES('one two four five');
INSERT INTO t1(content) VALUES('three four five');
INSERT INTO t1(content) VALUES('one three four five');
INSERT INTO t1(content) VALUES('two three four five');
INSERT INTO t1(content) VALUES('one two three four five');