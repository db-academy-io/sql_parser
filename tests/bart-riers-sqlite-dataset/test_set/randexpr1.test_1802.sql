-- randexpr1.test
-- 
-- db eval {SELECT (t1.d)+(abs(coalesce((select ((abs(case t1.e when a+(abs(11)/abs(b)) then t1.f else case coalesce((select max(d+c) from t1 where +19+11<b),t1.d)-t1.d-t1.d*d when t1.c then  -e else t1.d end end-c)/abs(17))) from t1 where 17<>t1.c),d))/abs(t1.d))-11 | (17) | b FROM t1 WHERE NOT ((select ~min(b)+count(distinct 17) from t1)>(select max((abs((13))/abs(coalesce((select case when b*13 in (case when e=t1.b then c when 11<>e then f else t1.b end,t1.f,t1.a) then d when not exists(select 1 from t1 where t1.f<>e) then t1.a else c end from t1 where t1.e=13),t1.f) | b)))+(~~cast(avg(f) AS integer)-min((13))) from t1))}
SELECT (t1.d)+(abs(coalesce((select ((abs(case t1.e when a+(abs(11)/abs(b)) then t1.f else case coalesce((select max(d+c) from t1 where +19+11<b),t1.d)-t1.d-t1.d*d when t1.c then  -e else t1.d end end-c)/abs(17))) from t1 where 17<>t1.c),d))/abs(t1.d))-11 | (17) | b FROM t1 WHERE NOT ((select ~min(b)+count(distinct 17) from t1)>(select max((abs((13))/abs(coalesce((select case when b*13 in (case when e=t1.b then c when 11<>e then f else t1.b end,t1.f,t1.a) then d when not exists(select 1 from t1 where t1.f<>e) then t1.a else c end from t1 where t1.e=13),t1.f) | b)))+(~~cast(avg(f) AS integer)-min((13))) from t1))