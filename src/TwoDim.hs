module TwoDim where

type Vec a = (a, a)

(|+|) :: Num a => Vec a -> Vec a -> Vec a
(x1, y1) |+| (x2, y2) = (x1 + y1, x2 + y2)

negV :: Num a => Vec a -> Vec a
negV (x, y) = (negate x, negate y)

(|-|) :: Num a => Vec a -> Vec a -> Vec a
v1 |-| v2 = v1 |+| negV v2

infixl 6 |+|, |-|


-- normal vector - 法線ベクトル
normalV :: Num a => Vec a -> Vec a
normalV (x, y) = (-y, x)


--inner product - 内積
(|.|) :: Num a => Vec a -> Vec a -> a
(x1, y1) |.| (x2, y2) = x1 * x2 + y1 * y2

-- cross product- 外積
-- 二次元の外積なので一成分しかない
(|*|) :: Num a => Vec a -> Vec a -> a
(x1, y1) |*| (x2, y2) = x1 * y2 - x2 * y1

infixl 7 |.|, |*|


-- 線分
type Seg a = (Vec a, Vec a)

{-
 (p0, p1) を通る直線の式
 line p0 p1 x = ( x - p0 ) |.| normalV ( p0 - p1 ) とすると
 line p0 p1 x = 0
 -}

line :: Num a => Seg a -> Vec a -> a
line (p0, p1) x = (x |-| p0) |.| normalV (p0 |-| p1)

{-
     q0, q1 が (p0, p1) を通る直線の反対側にある
  ⇔ 直線の式に入れたときに符号が逆
  ⇔ 直線の式に入れたものを掛けると負

  line (p0, p1) q0 > 0 && line (p0, p1) q1 < 0
  ||
  line (p0, p1) q0 < 0 && line (p0, p1) q1 > 0

  line (p0, p1) q0 * line (p0, p1) q1 < 0
 -}

{-
  線分 (p0, p1) と 線分(q0, q1) の交差判定

  q0, q1 が (p0, p1) を通る直線の反対側にある かつ
  p0, p1 が (q0, q1) を通る直線の反対側にある

  よって

  line (p0, p1) q0 * line (p0, p1) q1 < 0 &&
  line (q0, q1) p0 * line (q0, q1) p1 < 0
-}

crossSeg :: (Num a, Ord a) => Seg a -> Seg a -> Bool
crossSeg  (p0, p1) (q0, q1) =
  line (p0, p1) q0 * line (p0, p1) q1 < 0 &&
  line (q0, q1) p0 * line (q0, q1) p1 < 0
