-- randexpr1.test
-- 
-- db eval {SELECT ((select abs(min(t1.b-t1.b)-cast(avg((t1.d-t1.a*a) | (select max(t1.e-e) | max(17)*~abs(count(*)-cast(avg(coalesce((select t1.c from t1 where t1.c<=11),19)+ -e) AS integer) | max(t1.c)) | min(b) | (count(*))*min(19)- -count(*) | (cast(avg(b) AS integer)) from t1)) AS integer)-cast(avg(t1.b) AS integer)) from t1)) FROM t1 WHERE +b+(19*19)+c+t1.d<=e-19+case 13 when t1.f+~11+(select cast(avg(t1.e) AS integer) from t1) then (case when not exists(select 1 from t1 where t1.a between d and t1.b) then case c when d then d else t1.f end when not t1.b in (11,t1.c,t1.d) or t1.c<=(d) then (t1.a) else t1.b end) else  -e end or f in (t1.f,d,t1.c)}
SELECT ((select abs(min(t1.b-t1.b)-cast(avg((t1.d-t1.a*a) | (select max(t1.e-e) | max(17)*~abs(count(*)-cast(avg(coalesce((select t1.c from t1 where t1.c<=11),19)+ -e) AS integer) | max(t1.c)) | min(b) | (count(*))*min(19)- -count(*) | (cast(avg(b) AS integer)) from t1)) AS integer)-cast(avg(t1.b) AS integer)) from t1)) FROM t1 WHERE +b+(19*19)+c+t1.d<=e-19+case 13 when t1.f+~11+(select cast(avg(t1.e) AS integer) from t1) then (case when not exists(select 1 from t1 where t1.a between d and t1.b) then case c when d then d else t1.f end when not t1.b in (11,t1.c,t1.d) or t1.c<=(d) then (t1.a) else t1.b end) else  -e end or f in (t1.f,d,t1.c)