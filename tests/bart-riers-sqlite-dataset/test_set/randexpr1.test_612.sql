-- randexpr1.test
-- 
-- db eval {SELECT f-~+17*(abs(t1.f)/abs(a*case when case when (case when d>=t1.b or t1.b>=11 then t1.c when t1.b>t1.d then t1.d else t1.a end+17) not in (11,t1.e,17) then c when (t1.b>=a and b>b) then t1.d else d end in (select +abs( -max(t1.e)) from t1 union select count(*) & max(17) from t1) then (t1.a) else a end+(t1.e)))*(11)-f & t1.a-f FROM t1 WHERE ((t1.d>=t1.c))}
SELECT f-~+17*(abs(t1.f)/abs(a*case when case when (case when d>=t1.b or t1.b>=11 then t1.c when t1.b>t1.d then t1.d else t1.a end+17) not in (11,t1.e,17) then c when (t1.b>=a and b>b) then t1.d else d end in (select +abs( -max(t1.e)) from t1 union select count(*) & max(17) from t1) then (t1.a) else a end+(t1.e)))*(11)-f & t1.a-f FROM t1 WHERE ((t1.d>=t1.c))