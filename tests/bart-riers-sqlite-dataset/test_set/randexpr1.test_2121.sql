-- randexpr1.test
-- 
-- db eval {SELECT 19* -coalesce((select coalesce((select t1.d*17 from t1 where t1.b<=t1.b+17), -(a & 17)) from t1 where t1.d in (select case abs(count(*))*count(distinct (17)) when max(11) then abs(+max(13)-case  -cast(avg(t1.d) AS integer)- -(max(19))-count(distinct d) when min(t1.c) then cast(avg(c) AS integer) else count(distinct t1.f) end & max(c)) else max(t1.d) end from t1 union select count(*) from t1)),t1.f)-t1.e+c FROM t1 WHERE NOT (~(abs(case 17 when d then coalesce((select t1.d from t1 where 13 in (select t1.d-f | case when (17 in (t1.a,19,t1.f)) then case when a*f in (select e from t1 union select t1.a from t1) or 11=t1.c or t1.a>=(19) and ( -b)<f or e<>13 then b when t1.e in (t1.c, -t1.b,a) then 19 else 19 end else a end from t1 union select t1.b from t1)),d) else 13 end)/abs(11))+19*t1.e in (e, -t1.e,a))}
SELECT 19* -coalesce((select coalesce((select t1.d*17 from t1 where t1.b<=t1.b+17), -(a & 17)) from t1 where t1.d in (select case abs(count(*))*count(distinct (17)) when max(11) then abs(+max(13)-case  -cast(avg(t1.d) AS integer)- -(max(19))-count(distinct d) when min(t1.c) then cast(avg(c) AS integer) else count(distinct t1.f) end & max(c)) else max(t1.d) end from t1 union select count(*) from t1)),t1.f)-t1.e+c FROM t1 WHERE NOT (~(abs(case 17 when d then coalesce((select t1.d from t1 where 13 in (select t1.d-f | case when (17 in (t1.a,19,t1.f)) then case when a*f in (select e from t1 union select t1.a from t1) or 11=t1.c or t1.a>=(19) and ( -b)<f or e<>13 then b when t1.e in (t1.c, -t1.b,a) then 19 else 19 end else a end from t1 union select t1.b from t1)),d) else 13 end)/abs(11))+19*t1.e in (e, -t1.e,a))