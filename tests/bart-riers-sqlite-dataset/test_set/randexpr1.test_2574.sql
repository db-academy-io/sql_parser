-- randexpr1.test
-- 
-- db eval {SELECT case when d-t1.c<>e then c when (11 not between 13 and 13) then 13 else  -case when c*t1.d>=b*b then  -t1.f else b-t1.c | d*(case t1.d when  -case t1.b when t1.b then t1.b else 19 end*19*(t1.d) then 13 else 19 end) end+19*11 end+b FROM t1 WHERE ~~case when t1.d+(abs(t1.b+coalesce((select coalesce((select max(b-a-a) from t1 where 17 not in (e,case when (t1.d)<=t1.f or  -a not between c and t1.b then c when  -19 in ( -d,(t1.a),19) then b else f end,t1.e)), -13) from t1 where not b in (t1.b, -t1.b,c)),e))/abs(11)) between 13 and d then (t1.b) when t1.e<13 then 19 else a end-(19)-d*t1.d not in (d,t1.c,13)}
SELECT case when d-t1.c<>e then c when (11 not between 13 and 13) then 13 else  -case when c*t1.d>=b*b then  -t1.f else b-t1.c | d*(case t1.d when  -case t1.b when t1.b then t1.b else 19 end*19*(t1.d) then 13 else 19 end) end+19*11 end+b FROM t1 WHERE ~~case when t1.d+(abs(t1.b+coalesce((select coalesce((select max(b-a-a) from t1 where 17 not in (e,case when (t1.d)<=t1.f or  -a not between c and t1.b then c when  -19 in ( -d,(t1.a),19) then b else f end,t1.e)), -13) from t1 where not b in (t1.b, -t1.b,c)),e))/abs(11)) between 13 and d then (t1.b) when t1.e<13 then 19 else a end-(19)-d*t1.d not in (d,t1.c,13)