-- randexpr1.test
-- 
-- db eval {SELECT t1.b+coalesce((select max(~e- -t1.d+case +b+f when a-case when t1.a-d+t1.c between t1.e and a*e then (abs(e)/abs(t1.a)) else f |  -f*t1.b-t1.c end*t1.e+f then 19 else t1.b end*19) from t1 where 19 in (select e from t1 union select 17 from t1)),19) FROM t1 WHERE d in (11,t1.c+c*t1.d+case when t1.c-case when ~(19+13)*c=11 and d<>17 or t1.e>=c then case t1.a when c then f else 11 end+t1.c when t1.f in (select max(13) from t1 union select cast(avg(c) AS integer) from t1) or t1.c<t1.b then 13 else b end=13 then 11 when d<>t1.b then t1.e else 19 end,a)}
SELECT t1.b+coalesce((select max(~e- -t1.d+case +b+f when a-case when t1.a-d+t1.c between t1.e and a*e then (abs(e)/abs(t1.a)) else f |  -f*t1.b-t1.c end*t1.e+f then 19 else t1.b end*19) from t1 where 19 in (select e from t1 union select 17 from t1)),19) FROM t1 WHERE d in (11,t1.c+c*t1.d+case when t1.c-case when ~(19+13)*c=11 and d<>17 or t1.e>=c then case t1.a when c then f else 11 end+t1.c when t1.f in (select max(13) from t1 union select cast(avg(c) AS integer) from t1) or t1.c<t1.b then 13 else b end=13 then 11 when d<>t1.b then t1.e else 19 end,a)