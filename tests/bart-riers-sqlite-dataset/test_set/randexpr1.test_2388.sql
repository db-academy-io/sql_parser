-- randexpr1.test
-- 
-- db eval {SELECT (abs((select (case (abs(max( -t1.a))* -case count(*) when case min(c) when max(d) then count(*) else max(17) end-cast(avg(17) AS integer) then max(11) else cast(avg(t1.d) AS integer) end | max(13)+max(f)) when cast(avg(f) AS integer) then cast(avg(19) AS integer) else (max(11)) end) from t1) | coalesce((select (select max( -t1.d) from t1) from t1 where t1.b-~e in (select d from t1 union select e from t1) and t1.e=t1.d), -t1.e)-17)/abs(t1.e)) |  -t1.e FROM t1 WHERE exists(select 1 from t1 where coalesce((select +19 from t1 where (coalesce((select max(e) from t1 where ((coalesce((select max(19) from t1 where coalesce((select t1.d*19+(t1.c) from t1 where e in (select  -11 from t1 union select t1.f from t1) and (t1.d) in (select ~max(t1.e) from t1 union select (( -count(*))) from t1) or 19>=(a)),f) in ((e),e,t1.a)),t1.a)<=t1.c))),t1.f+t1.c)>13)),11) in (11,17,t1.d) and e>=t1.b)}
SELECT (abs((select (case (abs(max( -t1.a))* -case count(*) when case min(c) when max(d) then count(*) else max(17) end-cast(avg(17) AS integer) then max(11) else cast(avg(t1.d) AS integer) end | max(13)+max(f)) when cast(avg(f) AS integer) then cast(avg(19) AS integer) else (max(11)) end) from t1) | coalesce((select (select max( -t1.d) from t1) from t1 where t1.b-~e in (select d from t1 union select e from t1) and t1.e=t1.d), -t1.e)-17)/abs(t1.e)) |  -t1.e FROM t1 WHERE exists(select 1 from t1 where coalesce((select +19 from t1 where (coalesce((select max(e) from t1 where ((coalesce((select max(19) from t1 where coalesce((select t1.d*19+(t1.c) from t1 where e in (select  -11 from t1 union select t1.f from t1) and (t1.d) in (select ~max(t1.e) from t1 union select (( -count(*))) from t1) or 19>=(a)),f) in ((e),e,t1.a)),t1.a)<=t1.c))),t1.f+t1.c)>13)),11) in (11,17,t1.d) and e>=t1.b)