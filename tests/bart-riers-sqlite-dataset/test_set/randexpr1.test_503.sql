-- randexpr1.test
-- 
-- db eval {SELECT case when case when exists(select 1 from t1 where 17 in (select b from t1 union select 13 from t1)) and 19+a in (select count(distinct  -(select min(t1.a-f) from t1)) from t1 union select min(case when 11 between coalesce((select max(t1.b) from t1 where case when t1.d<>c or t1.b>=t1.a then ( -11) else t1.d end>=t1.a),t1.c) and  -17 then  -t1.f else e end) from t1) then a else 19 end+t1.e in (select e from t1 union select  -t1.b from t1) then t1.b when not (f) between f and t1.c then t1.e else e end FROM t1 WHERE 17>coalesce((select t1.b from t1 where t1.a+case when b in (19,t1.c,case t1.e when d-(b) then t1.f else 13 end) then 13 when d in (select count(distinct t1.a) from t1 union select case ~max(f) when cast(avg(b) AS integer) then max(d) else min(t1.f) end*min(e) from t1) or (t1.c<a) then t1.f else  -t1.c end<f or 13<=f),c) or 19>=t1.e}
SELECT case when case when exists(select 1 from t1 where 17 in (select b from t1 union select 13 from t1)) and 19+a in (select count(distinct  -(select min(t1.a-f) from t1)) from t1 union select min(case when 11 between coalesce((select max(t1.b) from t1 where case when t1.d<>c or t1.b>=t1.a then ( -11) else t1.d end>=t1.a),t1.c) and  -17 then  -t1.f else e end) from t1) then a else 19 end+t1.e in (select e from t1 union select  -t1.b from t1) then t1.b when not (f) between f and t1.c then t1.e else e end FROM t1 WHERE 17>coalesce((select t1.b from t1 where t1.a+case when b in (19,t1.c,case t1.e when d-(b) then t1.f else 13 end) then 13 when d in (select count(distinct t1.a) from t1 union select case ~max(f) when cast(avg(b) AS integer) then max(d) else min(t1.f) end*min(e) from t1) or (t1.c<a) then t1.f else  -t1.c end<f or 13<=f),c) or 19>=t1.e