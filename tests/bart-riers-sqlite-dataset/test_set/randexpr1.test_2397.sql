-- randexpr1.test
-- 
-- db eval {SELECT case t1.d when (abs(17)/abs(coalesce((select max(e) from t1 where a not in (t1.e,case when 11+t1.d not between t1.e and  -t1.c & t1.a*(abs(b)/abs(c)) then  -d when a in (select +11 from t1 union select c from t1) then  -t1.c else 13 end & t1.c,t1.b)),d)-f)) & t1.c then case when 13<c then t1.b when d<>t1.d then 13 else f end else b end FROM t1 WHERE 17>d*~t1.e+case when not t1.f<>(select count(distinct 19)+min(e) from t1) then case when (13-a<= -t1.b) and t1.f<=t1.f then 19*d else f end-t1.f*d+t1.d when b not in (t1.b,19,19) or t1.d not between t1.b and a then t1.f else b end}
SELECT case t1.d when (abs(17)/abs(coalesce((select max(e) from t1 where a not in (t1.e,case when 11+t1.d not between t1.e and  -t1.c & t1.a*(abs(b)/abs(c)) then  -d when a in (select +11 from t1 union select c from t1) then  -t1.c else 13 end & t1.c,t1.b)),d)-f)) & t1.c then case when 13<c then t1.b when d<>t1.d then 13 else f end else b end FROM t1 WHERE 17>d*~t1.e+case when not t1.f<>(select count(distinct 19)+min(e) from t1) then case when (13-a<= -t1.b) and t1.f<=t1.f then 19*d else f end-t1.f*d+t1.d when b not in (t1.b,19,19) or t1.d not between t1.b and a then t1.f else b end