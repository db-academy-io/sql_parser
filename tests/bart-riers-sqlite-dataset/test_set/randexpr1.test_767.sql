-- randexpr1.test
-- 
-- db eval {SELECT (select cast(avg( -coalesce((select max(+(abs(t1.d)/abs(~d))*+c*t1.c) from t1 where e in (select coalesce((select f from t1 where t1.f<> -11),17)+c*19 from t1 union select f from t1)),t1.c)) AS integer)-~case ~count(distinct 17)*count(distinct 19) when count(distinct t1.c)*count(*)-count(distinct 19) then +(max(a)+case  - - - -min(t1.f) when max(c) then count(distinct (f)) else min(13) end) else ( -cast(avg( -t1.b) AS integer)) end from t1) FROM t1 WHERE exists(select 1 from t1 where (abs(e*coalesce((select (abs(b*(t1.b)+f)/abs(d)) from t1 where t1.c in (select min(a | t1.b) from t1 union select abs(case  -case ~ -~cast(avg(e) AS integer) when ++min(17)+( -cast(avg(a) AS integer))-max(t1.c) then cast(avg(t1.b) AS integer) else cast(avg(f) AS integer) end when  - -max((t1.d)) then  -min(t1.b) else cast(avg(t1.c) AS integer) end) from t1)),b))/abs(t1.d)) in (select min(t1.c)-cast(avg( -e) AS integer)- -cast(avg(t1.f) AS integer) from t1 union select (max((19))) from t1))}
SELECT (select cast(avg( -coalesce((select max(+(abs(t1.d)/abs(~d))*+c*t1.c) from t1 where e in (select coalesce((select f from t1 where t1.f<> -11),17)+c*19 from t1 union select f from t1)),t1.c)) AS integer)-~case ~count(distinct 17)*count(distinct 19) when count(distinct t1.c)*count(*)-count(distinct 19) then +(max(a)+case  - - - -min(t1.f) when max(c) then count(distinct (f)) else min(13) end) else ( -cast(avg( -t1.b) AS integer)) end from t1) FROM t1 WHERE exists(select 1 from t1 where (abs(e*coalesce((select (abs(b*(t1.b)+f)/abs(d)) from t1 where t1.c in (select min(a | t1.b) from t1 union select abs(case  -case ~ -~cast(avg(e) AS integer) when ++min(17)+( -cast(avg(a) AS integer))-max(t1.c) then cast(avg(t1.b) AS integer) else cast(avg(f) AS integer) end when  - -max((t1.d)) then  -min(t1.b) else cast(avg(t1.c) AS integer) end) from t1)),b))/abs(t1.d)) in (select min(t1.c)-cast(avg( -e) AS integer)- -cast(avg(t1.f) AS integer) from t1 union select (max((19))) from t1))