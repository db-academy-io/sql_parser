-- randexpr1.test
-- 
-- db eval {SELECT (abs((select abs(case  -max(e)+max(t1.f)+(count(*)* -count(distinct case e when 19 then c else 13 end))*~min(t1.a) when cast(avg(t1.a) AS integer) then  -min(e) else  -count(*) end) from t1)*coalesce((select (t1.a) from t1 where case when c+t1.b-b | d in (select max(d) from t1 union select  -max(19) from t1) then t1.b else f end=t1.d),t1.b)*(t1.d) | e+t1.a)/abs(t1.d)) FROM t1 WHERE NOT (t1.f in (19,d*t1.d,(select min((select count(*)*min((17)+(abs(11)/abs(+~coalesce((select max(f) from t1 where 13 not between t1.c and t1.a),t1.e)-11))) from t1)) from t1)+17))}
SELECT (abs((select abs(case  -max(e)+max(t1.f)+(count(*)* -count(distinct case e when 19 then c else 13 end))*~min(t1.a) when cast(avg(t1.a) AS integer) then  -min(e) else  -count(*) end) from t1)*coalesce((select (t1.a) from t1 where case when c+t1.b-b | d in (select max(d) from t1 union select  -max(19) from t1) then t1.b else f end=t1.d),t1.b)*(t1.d) | e+t1.a)/abs(t1.d)) FROM t1 WHERE NOT (t1.f in (19,d*t1.d,(select min((select count(*)*min((17)+(abs(11)/abs(+~coalesce((select max(f) from t1 where 13 not between t1.c and t1.a),t1.e)-11))) from t1)) from t1)+17))