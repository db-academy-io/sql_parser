-- randexpr1.test
-- 
-- db eval {SELECT +c+case when (~t1.e in (+(select case cast(avg(17) AS integer)+min(19)+count(*) when  -max(c) then count(distinct t1.c) else  - - -(max(13)) end*count(distinct ( -17)) from t1)-case when d+case when f<>d or t1.d<=t1.e then t1.a else  -11 end in (select cast(avg(t1.e) AS integer) from t1 union select cast(avg(b) AS integer) from t1) then 19 when ((t1.c) between e and a and  -13 not in (d,f,e)) then t1.e else d end,a,t1.b)) then case when t1.b<19 then 13 else 19 end else t1.c end FROM t1 WHERE not exists(select 1 from t1 where 19>t1.f+coalesce((select 13 from t1 where t1.d in (coalesce((select max(e) from t1 where c<t1.c),case when exists(select 1 from t1 where not exists(select 1 from t1 where not (coalesce((select 19 from t1 where case when not exists(select 1 from t1 where 17<17) then t1.f else a end>=t1.c),t1.d) between t1.b and c))) then  -f+(t1.b)-t1.e-t1.c+t1.c when c in (select  -( -f) from t1 union select 13 from t1) then t1.e else t1.a end),t1.b,a)),11))}
SELECT +c+case when (~t1.e in (+(select case cast(avg(17) AS integer)+min(19)+count(*) when  -max(c) then count(distinct t1.c) else  - - -(max(13)) end*count(distinct ( -17)) from t1)-case when d+case when f<>d or t1.d<=t1.e then t1.a else  -11 end in (select cast(avg(t1.e) AS integer) from t1 union select cast(avg(b) AS integer) from t1) then 19 when ((t1.c) between e and a and  -13 not in (d,f,e)) then t1.e else d end,a,t1.b)) then case when t1.b<19 then 13 else 19 end else t1.c end FROM t1 WHERE not exists(select 1 from t1 where 19>t1.f+coalesce((select 13 from t1 where t1.d in (coalesce((select max(e) from t1 where c<t1.c),case when exists(select 1 from t1 where not exists(select 1 from t1 where not (coalesce((select 19 from t1 where case when not exists(select 1 from t1 where 17<17) then t1.f else a end>=t1.c),t1.d) between t1.b and c))) then  -f+(t1.b)-t1.e-t1.c+t1.c when c in (select  -( -f) from t1 union select 13 from t1) then t1.e else t1.a end),t1.b,a)),11))