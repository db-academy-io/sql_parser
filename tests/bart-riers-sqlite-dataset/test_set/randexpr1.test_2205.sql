-- randexpr1.test
-- 
-- db eval {SELECT c-coalesce((select  -coalesce((select max(case when (abs(~~case when case when t1.e<=e and t1.f>t1.d then b when t1.a<>t1.a then 17 else 17 end+t1.c>=t1.a then (c) when c not in (f,e,t1.c) then t1.d else 13 end)/abs(t1.a))<t1.b then t1.e when c<=t1.a then b else d end) from t1 where (19) not in (13,t1.e,f)),17) from t1 where 19>c),t1.a)*19 FROM t1 WHERE NOT ((17) | case when 19 not in ((case when (e*19=f) then case t1.f when a then 13 else t1.d end when b<t1.f then f else a end)*t1.d,c, -t1.f) then t1.f when (e=(d) and 19<>t1.b) then f else a end in (select c from t1 union select  -13 from t1) or 17 in (select 19 from t1 union select d from t1) and t1.b in (c,b,19) and t1.b>=( -b))}
SELECT c-coalesce((select  -coalesce((select max(case when (abs(~~case when case when t1.e<=e and t1.f>t1.d then b when t1.a<>t1.a then 17 else 17 end+t1.c>=t1.a then (c) when c not in (f,e,t1.c) then t1.d else 13 end)/abs(t1.a))<t1.b then t1.e when c<=t1.a then b else d end) from t1 where (19) not in (13,t1.e,f)),17) from t1 where 19>c),t1.a)*19 FROM t1 WHERE NOT ((17) | case when 19 not in ((case when (e*19=f) then case t1.f when a then 13 else t1.d end when b<t1.f then f else a end)*t1.d,c, -t1.f) then t1.f when (e=(d) and 19<>t1.b) then f else a end in (select c from t1 union select  -13 from t1) or 17 in (select 19 from t1 union select d from t1) and t1.b in (c,b,19) and t1.b>=( -b))