-- 2.1.5 --
double :: Num n => n -> n

double x = x + x

addDoubles x y = x + x + y + y

doubleIf x = if x < 100 then double x else x

