-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select case d when e then t1.f else d++(abs((select count(*) from t1)-case when t1.f*case when t1.b-c in (select  -cast(avg(e) AS integer) from t1 union select max(t1.a) from t1) or b between t1.a and  -17 and  -t1.f not between 17 and a then t1.e when 17<>11 then (abs(19)/abs(f)) else 17 end-17 not in (13,11,c) then t1.f else t1.c end)/abs(c))*t1.e end from t1 where f between t1.c and 17),t1.f) FROM t1 WHERE NOT (t1.b<>19)}
SELECT coalesce((select case d when e then t1.f else d++(abs((select count(*) from t1)-case when t1.f*case when t1.b-c in (select  -cast(avg(e) AS integer) from t1 union select max(t1.a) from t1) or b between t1.a and  -17 and  -t1.f not between 17 and a then t1.e when 17<>11 then (abs(19)/abs(f)) else 17 end-17 not in (13,11,c) then t1.f else t1.c end)/abs(c))*t1.e end from t1 where f between t1.c and 17),t1.f) FROM t1 WHERE NOT (t1.b<>19)