-- randexpr1.test
-- 
-- db eval {SELECT case 17 when  -b then e*c else (abs(coalesce((select max(coalesce((select coalesce((select max(case when t1.d<=19 and t1.d>e then b*t1.e else a end) from t1 where t1.e=t1.e),t1.f) from t1 where (d not between 13 and (f))),17)-d) from t1 where (11<>19)),t1.e)+t1.c)/abs(t1.e))*11*c-t1.d*t1.d+a end FROM t1 WHERE NOT (13-e*t1.e<case when (coalesce((select max(c*13) from t1 where not exists(select 1 from t1 where (19+(coalesce((select coalesce((select max(a) from t1 where 17>=19),(abs(e)/abs((abs( -d+e)/abs(a))))+t1.a) from t1 where t1.b in (t1.e,(t1.e),t1.f)),f))=t1.e)) or b<=19),t1.b)<=13) then 13 else t1.f end-f)}
SELECT case 17 when  -b then e*c else (abs(coalesce((select max(coalesce((select coalesce((select max(case when t1.d<=19 and t1.d>e then b*t1.e else a end) from t1 where t1.e=t1.e),t1.f) from t1 where (d not between 13 and (f))),17)-d) from t1 where (11<>19)),t1.e)+t1.c)/abs(t1.e))*11*c-t1.d*t1.d+a end FROM t1 WHERE NOT (13-e*t1.e<case when (coalesce((select max(c*13) from t1 where not exists(select 1 from t1 where (19+(coalesce((select coalesce((select max(a) from t1 where 17>=19),(abs(e)/abs((abs( -d+e)/abs(a))))+t1.a) from t1 where t1.b in (t1.e,(t1.e),t1.f)),f))=t1.e)) or b<=19),t1.b)<=13) then 13 else t1.f end-f)