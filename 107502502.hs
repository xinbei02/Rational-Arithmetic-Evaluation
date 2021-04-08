import Prelude hiding ((/))

data MyRational = MyNum Integer Integer

(%) :: Integer -> Integer -> MyRational
a%b
  |b==0 = error "divide by zero"
  |otherwise = MyNum a b

instance Eq MyRational where
  (MyNum a b)==(MyNum c d) = (a*d==b*c)

instance Num MyRational where
  (MyNum a b) + (MyNum c d)
    |a==0 = MyNum c d
    |c==0 = MyNum a b
    |otherwise = MyNum (div (a*d+b*c) gcd_) (div (b*d) gcd_)
                  where gcd_ = gcd (a*d+b*c) (b*d)
  (MyNum a b) * (MyNum c d)
    |a==0 = MyNum 0 1
    |c==0 = MyNum 0 1
    |otherwise = MyNum (div (a*c) gcd_) (div (b*d) gcd_)
                  where gcd_ = gcd (a*c) (b*d)
  negate (MyNum a b) = MyNum (0-a) b
  abs (MyNum a b) = MyNum (abs a) (abs b)
  signum (MyNum a b) = MyNum (signum (a*b)) 1 
  fromInteger a = MyNum a 1

(/) :: MyRational -> MyRational -> MyRational
(MyNum a b) / (MyNum c d)
    |c==0 = error "divide by zero"
    |a==0 = MyNum 0 1
    |otherwise = MyNum (div (a*d) gcd_) (div (b*c) gcd_)
                  where gcd_ = gcd (a*d) (b*c)

infixl 7 /

instance Show MyRational where
  show (MyNum a b)
      |b==1 = show a
      |b<0 = show (symbolReset (reduction (MyNum a b)))
      |(gcd a b)/=1 = show (reduction (MyNum a b))
      |a<0 = "(" ++ show a ++ ") % " ++ show b
      |otherwise = show a ++ " % " ++ show b

reduction :: MyRational -> MyRational
reduction (MyNum a b) = MyNum (div a gcd_) (div b gcd_)
                          where gcd_ = gcd a b

symbolReset :: MyRational -> MyRational
symbolReset (MyNum a b)
  |a<0 && b<0 = abs (MyNum a b)
  |a>0 && b<0 = MyNum (0-a) (0-b)
  |otherwise = MyNum a b

main = return()