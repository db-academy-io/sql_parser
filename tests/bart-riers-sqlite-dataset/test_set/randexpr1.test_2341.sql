-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select max(19) from t1 where t1.f*17+case t1.f when 11 then coalesce((select max(t1.a) from t1 where 13<>t1.d),(abs(13+b-case when not c+(abs(case when t1.c not between a and t1.f then  -(e) else t1.d end)/abs( -17))<a then 11 else t1.c end)/abs(17)))*d*t1.a else c end-((f))-e*t1.a<17),t1.f)+t1.a FROM t1 WHERE NOT (e>=13)}
SELECT coalesce((select max(19) from t1 where t1.f*17+case t1.f when 11 then coalesce((select max(t1.a) from t1 where 13<>t1.d),(abs(13+b-case when not c+(abs(case when t1.c not between a and t1.f then  -(e) else t1.d end)/abs( -17))<a then 11 else t1.c end)/abs(17)))*d*t1.a else c end-((f))-e*t1.a<17),t1.f)+t1.a FROM t1 WHERE NOT (e>=13)