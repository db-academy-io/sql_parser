-- randexpr1.test
-- 
-- db eval {SELECT (((d)+t1.f))-case +t1.d when coalesce((select max(13) from t1 where (select  -max(17*case when d>=coalesce((select 19 from t1 where a<>t1.e),t1.b) then e when  -c<>d then e else t1.a end*t1.f)+ -case count(*) when (+cast(avg(c) AS integer)) then count(*) else  -count(distinct t1.f) end+count(*) from t1)<=(abs(t1.c)/abs(t1.f))),17) then 17 else t1.d end-t1.b FROM t1 WHERE NOT (~d-c in (select cast(avg(19) AS integer) from t1 union select  - -abs((abs((cast(avg(f) AS integer)) | abs(count(distinct 19))))) from t1))}
SELECT (((d)+t1.f))-case +t1.d when coalesce((select max(13) from t1 where (select  -max(17*case when d>=coalesce((select 19 from t1 where a<>t1.e),t1.b) then e when  -c<>d then e else t1.a end*t1.f)+ -case count(*) when (+cast(avg(c) AS integer)) then count(*) else  -count(distinct t1.f) end+count(*) from t1)<=(abs(t1.c)/abs(t1.f))),17) then 17 else t1.d end-t1.b FROM t1 WHERE NOT (~d-c in (select cast(avg(19) AS integer) from t1 union select  - -abs((abs((cast(avg(f) AS integer)) | abs(count(distinct 19))))) from t1))