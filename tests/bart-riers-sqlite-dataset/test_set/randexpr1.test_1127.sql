-- randexpr1.test
-- 
-- db eval {SELECT case t1.e-d+b when coalesce((select (select max(t1.a | a |  -(select (count(*))+( - - -count(*)) from t1)*19+ -f+t1.e) from t1) from t1 where not exists(select 1 from t1 where (d not between a and 11) or not d>=11 and exists(select 1 from t1 where not t1.a<>13)) or 19 not in (17,t1.c,t1.c)),19)*t1.c+d then b else c end-b FROM t1 WHERE c<17}
SELECT case t1.e-d+b when coalesce((select (select max(t1.a | a |  -(select (count(*))+( - - -count(*)) from t1)*19+ -f+t1.e) from t1) from t1 where not exists(select 1 from t1 where (d not between a and 11) or not d>=11 and exists(select 1 from t1 where not t1.a<>13)) or 19 not in (17,t1.c,t1.c)),19)*t1.c+d then b else c end-b FROM t1 WHERE c<17