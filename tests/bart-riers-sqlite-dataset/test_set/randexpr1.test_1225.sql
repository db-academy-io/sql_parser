-- randexpr1.test
-- 
-- db eval {SELECT f+case when (~t1.c between case when +d between (select  -abs(count(distinct t1.a+17+t1.f))+case min(19)*count(distinct  -t1.a) when (count(*)) then ((count(*))) else ((max(t1.e))) end from t1) and case when  -t1.b>=13 then 11 else a end then  -17 else a end and (t1.c) and (b=17)) then (+(t1.c)) when t1.a<=t1.f then t1.d else 11 end FROM t1 WHERE NOT (c<>~13)}
SELECT f+case when (~t1.c between case when +d between (select  -abs(count(distinct t1.a+17+t1.f))+case min(19)*count(distinct  -t1.a) when (count(*)) then ((count(*))) else ((max(t1.e))) end from t1) and case when  -t1.b>=13 then 11 else a end then  -17 else a end and (t1.c) and (b=17)) then (+(t1.c)) when t1.a<=t1.f then t1.d else 11 end FROM t1 WHERE NOT (c<>~13)