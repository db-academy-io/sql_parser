-- randexpr1.test
-- 
-- db eval {SELECT b*~~t1.b*(a-t1.c+t1.b)+13-+~case t1.b when d then case 17*13 when t1.e then case when case when t1.c<>~(11) then t1.e else coalesce((select max( -c) from t1 where (f<>e)),t1.f) end<=a then (t1.c) else t1.c end else  -f end else 13 end*t1.e-t1.f & 13 FROM t1 WHERE NOT (b between (d) and t1.c*(abs(t1.c-13 | t1.d+t1.a)/abs(13)))}
SELECT b*~~t1.b*(a-t1.c+t1.b)+13-+~case t1.b when d then case 17*13 when t1.e then case when case when t1.c<>~(11) then t1.e else coalesce((select max( -c) from t1 where (f<>e)),t1.f) end<=a then (t1.c) else t1.c end else  -f end else 13 end*t1.e-t1.f & 13 FROM t1 WHERE NOT (b between (d) and t1.c*(abs(t1.c-13 | t1.d+t1.a)/abs(13)))