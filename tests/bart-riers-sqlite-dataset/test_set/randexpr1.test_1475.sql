-- randexpr1.test
-- 
-- db eval {SELECT case when (t1.b+case when a in (select coalesce((select max(t1.a) from t1 where not exists(select 1 from t1 where 11=case e when (e) then t1.b else t1.c end or c in (t1.f,f,11))),t1.f) from t1 union select t1.d from t1) and t1.e<>e then t1.f when t1.a in (t1.d,a,13) then 17 else t1.d end not in (t1.f,t1.d,17)) or c<> -19 then b when f<t1.c and t1.b in (e,11,t1.f) then t1.b-t1.c else  - -d end FROM t1 WHERE NOT (t1.f- -t1.d in (select case a when 13+coalesce((select coalesce((select coalesce((select case when b>a-e then b else 19 end from t1 where 11<>t1.b),a) from t1 where not 19 in (select max(19)*count(distinct a) from t1 union select cast(avg(11) AS integer) from t1)),11) from t1 where t1.e<>a),17) then d else 19 end from t1 union select f from t1) and c in (select count(distinct c) from t1 union select  -+max(d) | abs(count(*)) from t1))}
SELECT case when (t1.b+case when a in (select coalesce((select max(t1.a) from t1 where not exists(select 1 from t1 where 11=case e when (e) then t1.b else t1.c end or c in (t1.f,f,11))),t1.f) from t1 union select t1.d from t1) and t1.e<>e then t1.f when t1.a in (t1.d,a,13) then 17 else t1.d end not in (t1.f,t1.d,17)) or c<> -19 then b when f<t1.c and t1.b in (e,11,t1.f) then t1.b-t1.c else  - -d end FROM t1 WHERE NOT (t1.f- -t1.d in (select case a when 13+coalesce((select coalesce((select coalesce((select case when b>a-e then b else 19 end from t1 where 11<>t1.b),a) from t1 where not 19 in (select max(19)*count(distinct a) from t1 union select cast(avg(11) AS integer) from t1)),11) from t1 where t1.e<>a),17) then d else 19 end from t1 union select f from t1) and c in (select count(distinct c) from t1 union select  -+max(d) | abs(count(*)) from t1))