-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select (t1.d) from t1 where not not case when c<coalesce((select f from t1 where 13 | t1.d in (select 11*t1.f*case when exists(select 1 from t1 where d>=case when f<>( -t1.c) then c when 19<>t1.d then t1.f else d end) then e-f when c in (select d from t1 union select f from t1) then a else t1.b end | t1.a-f from t1 union select  -19 from t1)),19) then 19 when 11 between f and t1.f then t1.c else 19 end=a),t1.f) FROM t1 WHERE NOT (coalesce((select t1.d from t1 where (case when exists(select 1 from t1 where not  - -19 in (17,f,t1.b)) and 19>c or t1.c in (select t1.e from t1 union select 17 from t1) then a when exists(select 1 from t1 where (t1.c<>d)) and  -t1.c not between b and e then 11 else (select cast(avg((abs(b)/abs(d))) AS integer) from t1) end+t1.d not in (t1.f,11,b) and f not between 17 and t1.d)),t1.e)=t1.a)}
SELECT coalesce((select (t1.d) from t1 where not not case when c<coalesce((select f from t1 where 13 | t1.d in (select 11*t1.f*case when exists(select 1 from t1 where d>=case when f<>( -t1.c) then c when 19<>t1.d then t1.f else d end) then e-f when c in (select d from t1 union select f from t1) then a else t1.b end | t1.a-f from t1 union select  -19 from t1)),19) then 19 when 11 between f and t1.f then t1.c else 19 end=a),t1.f) FROM t1 WHERE NOT (coalesce((select t1.d from t1 where (case when exists(select 1 from t1 where not  - -19 in (17,f,t1.b)) and 19>c or t1.c in (select t1.e from t1 union select 17 from t1) then a when exists(select 1 from t1 where (t1.c<>d)) and  -t1.c not between b and e then 11 else (select cast(avg((abs(b)/abs(d))) AS integer) from t1) end+t1.d not in (t1.f,11,b) and f not between 17 and t1.d)),t1.e)=t1.a)