-- randexpr1.test
-- 
-- db eval {SELECT +11+~coalesce((select  -19 from t1 where exists(select 1 from t1 where f*t1.e not in ( -f*~t1.a-f*case when t1.f in ((e),coalesce((select b from t1 where b>=t1.f | t1.d),19),t1.a) then (t1.d) when  -a<=( -t1.a) then a else f end*a,13,19)) or e=17),a)*17 FROM t1 WHERE NOT ((select case min(+19) when abs((max(17)* -min(a*(abs(case e when t1.b then t1.f else  -t1.c end*f)/abs(t1.d))*e)-count(*) | max(19))) then  - -case count(distinct 11) when min(11) then max(19) else min(13) end-( -count(distinct 13)) else  -(count(distinct b)) end*cast(avg(17) AS integer)-max(e) from t1) between t1.d and coalesce((select max(13) from t1 where not 17>a-f-e),13))}
SELECT +11+~coalesce((select  -19 from t1 where exists(select 1 from t1 where f*t1.e not in ( -f*~t1.a-f*case when t1.f in ((e),coalesce((select b from t1 where b>=t1.f | t1.d),19),t1.a) then (t1.d) when  -a<=( -t1.a) then a else f end*a,13,19)) or e=17),a)*17 FROM t1 WHERE NOT ((select case min(+19) when abs((max(17)* -min(a*(abs(case e when t1.b then t1.f else  -t1.c end*f)/abs(t1.d))*e)-count(*) | max(19))) then  - -case count(distinct 11) when min(11) then max(19) else min(13) end-( -count(distinct 13)) else  -(count(distinct b)) end*cast(avg(17) AS integer)-max(e) from t1) between t1.d and coalesce((select max(13) from t1 where not 17>a-f-e),13))