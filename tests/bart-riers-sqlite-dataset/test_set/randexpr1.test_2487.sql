-- randexpr1.test
-- 
-- db eval {SELECT case (abs(case when 13 not in (t1.f,a,t1.b) then d*(abs((coalesce((select coalesce((select coalesce((select max(t1.d-t1.a) from t1 where (t1.c between 19 and (c) and c<t1.c)),17) from t1 where (11>=17)),t1.f) from t1 where t1.e between e and 17),17))-b)/abs(d)) | b+t1.b when e between t1.f and d then 19 else 17 end*f)/abs(a)) when t1.c then 11 else t1.d end FROM t1 WHERE NOT (d>=e)}
SELECT case (abs(case when 13 not in (t1.f,a,t1.b) then d*(abs((coalesce((select coalesce((select coalesce((select max(t1.d-t1.a) from t1 where (t1.c between 19 and (c) and c<t1.c)),17) from t1 where (11>=17)),t1.f) from t1 where t1.e between e and 17),17))-b)/abs(d)) | b+t1.b when e between t1.f and d then 19 else 17 end*f)/abs(a)) when t1.c then 11 else t1.d end FROM t1 WHERE NOT (d>=e)