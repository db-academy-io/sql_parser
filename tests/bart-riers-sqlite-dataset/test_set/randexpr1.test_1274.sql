-- randexpr1.test
-- 
-- db eval {SELECT case when coalesce((select b from t1 where t1.d not in (t1.c,case when t1.d not in (d,coalesce((select (select count(distinct d) from t1)*e from t1 where not exists(select 1 from t1 where t1.d>=t1.f) or 11+t1.a between (select  -cast(avg( -t1.b & 19) AS integer)+case min(t1.d) when  -max(t1.e) then  -count(distinct 11) else  - -max((t1.c)) end from t1) and f),(e)),13) then t1.a else d end* -t1.f,13)),d)>=e then 17 else  -11 end FROM t1 WHERE NOT (t1.d*b<=+~t1.c-t1.d+d*~b | e)}
SELECT case when coalesce((select b from t1 where t1.d not in (t1.c,case when t1.d not in (d,coalesce((select (select count(distinct d) from t1)*e from t1 where not exists(select 1 from t1 where t1.d>=t1.f) or 11+t1.a between (select  -cast(avg( -t1.b & 19) AS integer)+case min(t1.d) when  -max(t1.e) then  -count(distinct 11) else  - -max((t1.c)) end from t1) and f),(e)),13) then t1.a else d end* -t1.f,13)),d)>=e then 17 else  -11 end FROM t1 WHERE NOT (t1.d*b<=+~t1.c-t1.d+d*~b | e)