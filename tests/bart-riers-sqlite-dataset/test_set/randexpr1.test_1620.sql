-- randexpr1.test
-- 
-- db eval {SELECT (abs(a)/abs(coalesce((select case when coalesce((select d from t1 where d+case (coalesce((select max(e) from t1 where 17+f*13-(case 19 when t1.f then 17 else t1.e end)<>b),17)) when t1.e then t1.e else t1.a end- -13+ -t1.f between b and f),11)+t1.d between 13 and t1.d then b else (19) end-t1.c from t1 where c in (select t1.e from t1 union select f from t1)),b))) FROM t1 WHERE d>t1.a}
SELECT (abs(a)/abs(coalesce((select case when coalesce((select d from t1 where d+case (coalesce((select max(e) from t1 where 17+f*13-(case 19 when t1.f then 17 else t1.e end)<>b),17)) when t1.e then t1.e else t1.a end- -13+ -t1.f between b and f),11)+t1.d between 13 and t1.d then b else (19) end-t1.c from t1 where c in (select t1.e from t1 union select f from t1)),b))) FROM t1 WHERE d>t1.a