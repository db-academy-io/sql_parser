-- randexpr1.test
-- 
-- db eval {SELECT case when (select  -min(11) from t1)<>(abs(17)/abs(t1.d | e)) then case 11+case 11 when b | e-e then 19+e else b end*(abs(t1.a)/abs(b | a)) | coalesce((select case when a in (select cast(avg(d) AS integer) from t1 union select  - - -max(c) from t1) or e not in (c,d,a) then +13 when t1.a>a then 19 else f end from t1 where t1.c=13 or f<>t1.e),a) when f then f else c end else t1.e end FROM t1 WHERE ~17 in (11,t1.c,19)}
SELECT case when (select  -min(11) from t1)<>(abs(17)/abs(t1.d | e)) then case 11+case 11 when b | e-e then 19+e else b end*(abs(t1.a)/abs(b | a)) | coalesce((select case when a in (select cast(avg(d) AS integer) from t1 union select  - - -max(c) from t1) or e not in (c,d,a) then +13 when t1.a>a then 19 else f end from t1 where t1.c=13 or f<>t1.e),a) when f then f else c end else t1.e end FROM t1 WHERE ~17 in (11,t1.c,19)