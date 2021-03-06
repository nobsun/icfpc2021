module TwoDim (
  Vec,

  (|+|), (|-|),
  negateV,
  normalV,

  (|.|), (|*|),
  (·), (×),

  dist,

  intersect',
  intersect,

  line,
  ) where

import Types (Vec)

{- | 加算
>>> (0,0) |+| (1,2)
(1,2)
>>> (2,1) |+| (0,0)
(2,1)
>>> (1,2) |+| (3,4)
(4,6)
-}
(|+|) :: Num a => Vec a -> Vec a -> Vec a
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)

{- | 符号反転
>>> negateV (0,0)
(0,0)
>>> x = (1,2)
>>> x |+| negateV x
(0,0)
 -}
negateV :: Num a => Vec a -> Vec a
negateV (x, y) = (negate x, negate y)

(|-|) :: Num a => Vec a -> Vec a -> Vec a
v1 |-| v2 = v1 |+| negateV v2

infixl 6 |+|, |-|

{- | 法線
>>> x = (1,2)
>>> x /= (0,0)
True
>>> normalV x /= (0,0)
True
>>> dist (normalV x) == dist x -- 法線を取る操作は長さを変えない
True
 -}
-- normal vector - 法線ベクトル
normalV :: Num a => Vec a -> Vec a
normalV (x, y) = (-y, x)

{- | 内積
>>> x = (1,2)
>>> x |.| (0,0) -- 右零元
0
>>> (0,0) |.| x -- 左零元
0
>>>  x |.| (1,0) == fst x -- 単位ベクトルとの内積 x
True
>>>  x |.| (0,1) == snd x -- 単位ベクトルとの内積 y
True
>>> x |.| normalV x -- 法線との内積は0
0
 -}
--inner product - 内積
(|.|) :: Num a => Vec a -> Vec a -> a
(x1, y1) |.| (x2, y2) = x1 * x2 + y1 * y2

{- | 外積
>>> x = (1,2)
>>> x |*| (0,0)
0
>>> (0,0) |*| x
0
>>> x |*| x
0
>>> y = (2,1)
>>> x |*| y = x |.| normalV y  -- 定義
 -}
-- cross product - 外積
-- 二次元の外積なので一成分しかない
(|*|) :: Num a => Vec a -> Vec a -> a
(x1, y1) |*| (x2, y2) = x1 * y2 - x2 * y1

infixl 7 |.|, |*|

(·) :: Num a => Vec a -> Vec a -> a
(·) = (|.|)

(×) :: Num a => Vec a -> Vec a -> a
(×) = (|*|)

{- | 二乗距離
>>> dist (0,0)
0
>>> dist (1,0)
1
>>> dist (0,1)
1
>>> dist (1,2)
5
 -}
-- 二乗距離
dist :: Num a => Vec a -> a
dist v = v |.| v

-----

-- 線分
type Seg a = (Vec a, Vec a)

{-
 (p0, p1) を通る直線の式
 line p0 p1 x = ( x - p0 ) |.| normalV ( p0 - p1 ) とすると
 line p0 p1 x = 0
 -}

{-
     q0, q1 が (p0, p1) を通る直線の反対側にある
  ⇔ 直線の式に入れたときに符号が逆
  ⇔ 直線の式に入れたものを掛けると負

  line (p0, p1) q0 > 0 && line (p0, p1) q1 < 0
  ||
  line (p0, p1) q0 < 0 && line (p0, p1) q1 > 0

  line (p0, p1) q0 * line (p0, p1) q1 < 0
 -}

{- | 直線
>>> p@(p0,p1) = ((0,0),(6,8))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> line (p0, p1) q0 * line (p0, p1) q1 < 0
True
 -}
line :: Num a => Seg a -> Vec a -> a
line (p0, p1) x = (x |-| p0) |.| normalV (p0 |-| p1)


{-
  線分 (p0, p1) と 線分(q0, q1) の交差判定

  q0, q1 が (p0, p1) を通る直線の反対側にある かつ
  p0, p1 が (q0, q1) を通る直線の反対側にある

  よって

  line (p0, p1) q0 * line (p0, p1) q1 < 0 &&
  line (q0, q1) p0 * line (q0, q1) p1 < 0
-}

{- | 線分の交差 - 端点が線分上なら False
>>> p@(p0,p1) = ((0,0),(6,8))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect' p q
True
>>> p@(p0,p1) = ((0,0),(2,2))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect' p q
False
>>> p@(p0,p1) = ((0,0),(3,4))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect' p q
False
 -}
intersect' :: (Num a, Ord a) => Seg a -> Seg a -> Bool
intersect' p@(p0, p1) q@(q0, q1) =
  line p q0 * line p q1 < 0 &&
  line q p0 * line q p1 < 0

{- | 線分の交差 - 端点が線分上なら True
>>> p@(p0,p1) = ((0,0),(6,8))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect p q
True
>>> p@(p0,p1) = ((0,0),(2,2))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect p q
False
>>> p@(p0,p1) = ((0,0),(3,4))
>>> q@(q0,q1) = ((0,8),(6,0))
>>> intersect p q
True
-}
intersect :: (Num a, Ord a) => Seg a -> Seg a -> Bool
intersect p@(p0, p1) q@(q0, q1) =
  line p q0 * line p q1 <= 0 &&
  line q p0 * line q p1 <= 0
