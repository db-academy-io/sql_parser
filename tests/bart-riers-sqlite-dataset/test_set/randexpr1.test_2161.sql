-- randexpr1.test
-- 
-- db eval {SELECT coalesce((select ( -t1.e*(abs((case when not 13 not in (case coalesce((select max(t1.f) from t1 where not exists(select 1 from t1 where d between  -b and f)),11) when 11 then 19 else a end,t1.a,t1.a) then f-t1.f when t1.c in (select  -abs(count(*)+(max((t1.a)))) from t1 union select  -max((19)) from t1) then d else 13 end-19))/abs(t1.b))-t1.d*e-19) from t1 where t1.e not between 13 and c),19)*13 FROM t1 WHERE 19<>(t1.c)}
SELECT coalesce((select ( -t1.e*(abs((case when not 13 not in (case coalesce((select max(t1.f) from t1 where not exists(select 1 from t1 where d between  -b and f)),11) when 11 then 19 else a end,t1.a,t1.a) then f-t1.f when t1.c in (select  -abs(count(*)+(max((t1.a)))) from t1 union select  -max((19)) from t1) then d else 13 end-19))/abs(t1.b))-t1.d*e-19) from t1 where t1.e not between 13 and c),19)*13 FROM t1 WHERE 19<>(t1.c)