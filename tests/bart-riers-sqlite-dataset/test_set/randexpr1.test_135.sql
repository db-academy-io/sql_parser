-- randexpr1.test
-- 
-- db eval {SELECT case ~13 when t1.e then e else (select case case (abs(count(*))-~count(*)+count(*) & count(*)-count(*)+max(t1.b)* -(cast(avg(13) AS integer)) &  -(count(distinct t1.c))) when min(e) then (min(13)) else count(*) end*count(*) when max(e) then count(distinct  -d) else count(*) end from t1) end-t1.d*case (abs(e & t1.b)/abs((select cast(avg(t1.c) AS integer) from t1))) when (select cast(avg(a) AS integer) from t1) then t1.c else f+13 end FROM t1 WHERE not exists(select 1 from t1 where coalesce((select case when t1.a>t1.f-~case when ~t1.d+c+t1.f>11 then t1.a when (f between t1.f and t1.c) then t1.f else 13 end*t1.a then t1.d when c<t1.e or e>19 then c else t1.e end from t1 where e<>a and 19<>t1.f),t1.b)+t1.f-t1.e<d)}
SELECT case ~13 when t1.e then e else (select case case (abs(count(*))-~count(*)+count(*) & count(*)-count(*)+max(t1.b)* -(cast(avg(13) AS integer)) &  -(count(distinct t1.c))) when min(e) then (min(13)) else count(*) end*count(*) when max(e) then count(distinct  -d) else count(*) end from t1) end-t1.d*case (abs(e & t1.b)/abs((select cast(avg(t1.c) AS integer) from t1))) when (select cast(avg(a) AS integer) from t1) then t1.c else f+13 end FROM t1 WHERE not exists(select 1 from t1 where coalesce((select case when t1.a>t1.f-~case when ~t1.d+c+t1.f>11 then t1.a when (f between t1.f and t1.c) then t1.f else 13 end*t1.a then t1.d when c<t1.e or e>19 then c else t1.e end from t1 where e<>a and 19<>t1.f),t1.b)+t1.f-t1.e<d)