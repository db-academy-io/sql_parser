-- savepoint.test
-- 
-- execsql {
--     SAVEPOINT sp1;
--     SAVEPOINT sp2;
--     SAVEPOINT sp3;
--     ROLLBACK TO SAVEPOINT sp3;
--     ROLLBACK TRANSACTION TO sp2;
--     ROLLBACK TRANSACTION TO SAVEPOINT sp1;
-- }
SAVEPOINT sp1;
SAVEPOINT sp2;
SAVEPOINT sp3;
ROLLBACK TO SAVEPOINT sp3;
ROLLBACK TRANSACTION TO sp2;
ROLLBACK TRANSACTION TO SAVEPOINT sp1;