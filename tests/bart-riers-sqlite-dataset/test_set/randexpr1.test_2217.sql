-- randexpr1.test
-- 
-- db eval {SELECT t1.b-c*a*t1.b+ -f & (case when 13<f then d else t1.a+case (select +min(a) from t1) when case when a<=d*a & e then b when not f not between e and 17 or not 19=coalesce((select 13 from t1 where t1.f in (t1.b,t1.d,t1.a)),d) then 11 else (13) end then 11 else (a) end end-b) FROM t1 WHERE exists(select 1 from t1 where t1.a>17)}
SELECT t1.b-c*a*t1.b+ -f & (case when 13<f then d else t1.a+case (select +min(a) from t1) when case when a<=d*a & e then b when not f not between e and 17 or not 19=coalesce((select 13 from t1 where t1.f in (t1.b,t1.d,t1.a)),d) then 11 else (13) end then 11 else (a) end end-b) FROM t1 WHERE exists(select 1 from t1 where t1.a>17)