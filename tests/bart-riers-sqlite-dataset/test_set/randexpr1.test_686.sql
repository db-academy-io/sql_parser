-- randexpr1.test
-- 
-- db eval {SELECT ~case when exists(select 1 from t1 where t1.c not between case case ~t1.b*19 when b then t1.c*t1.e else t1.d end when c then case ++13 when case when a+d+~~11 not between e and  -19 then 19 else b end then t1.d else 19 end else t1.a end and d) then 19 when c in (select abs(count(distinct 17)) from t1 union select max(t1.b) from t1) then (d) else t1.e end & 19 FROM t1 WHERE NOT (t1.d between 11 and ~19-coalesce((select d from t1 where (d>=c)),t1.d)*+t1.e+t1.e)}
SELECT ~case when exists(select 1 from t1 where t1.c not between case case ~t1.b*19 when b then t1.c*t1.e else t1.d end when c then case ++13 when case when a+d+~~11 not between e and  -19 then 19 else b end then t1.d else 19 end else t1.a end and d) then 19 when c in (select abs(count(distinct 17)) from t1 union select max(t1.b) from t1) then (d) else t1.e end & 19 FROM t1 WHERE NOT (t1.d between 11 and ~19-coalesce((select d from t1 where (d>=c)),t1.d)*+t1.e+t1.e)