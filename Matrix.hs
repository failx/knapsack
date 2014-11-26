module Matrix where

type ColVector = [Rational]
type Matrix    = [ColVector]

-- get a nicer matrix representation
printMatrix :: Matrix -> IO ()
printMatrix m = putStrLn . unlines $ map (\l -> unwords $ map show l) (transpose m)

-- matrix transpose operation
transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose m      = map head m : transpose (map tail m)

-- inner product <u, v> of two vectors
inner_product :: ColVector -> ColVector -> Rational
inner_product = (sum .) . zipWith (*)

-- element-wise multiplication
dot_mult :: ColVector -> ColVector -> ColVector
dot_mult = zipWith (*)

-- element-wise substraction
dot_minus :: ColVector -> ColVector -> ColVector
dot_minus = zipWith (-)

-- scalar multiplication
scalar_mult :: Rational -> ColVector -> ColVector
scalar_mult r v = map (* r) v

-- scalar division
scalar_div :: Rational -> ColVector -> ColVector
scalar_div r v = map (/ r) v

-- projection operation
proj :: ColVector -> ColVector -> ColVector
proj u v = scalar_mult (inner_product u v / inner_product u u) u

-- calculate the projection factor required for projection operation
proj_factor :: ColVector -> ColVector -> Rational
proj_factor u v = (inner_product u v) / (inner_product u u)

-- calculates orthogonal matrix and mu coefficients
gram_schmidt :: Matrix -> (Matrix, Matrix)
gram_schmidt m = (reverse orthogonal_matrix, reverse (map reverse mus))
  where
    (orthogonal_matrix, mus) = gram_schmidt_helper m [] []

-- using us for accumulating output vectors and mus for accumulating mus
gram_schmidt_helper :: Matrix -> Matrix -> Matrix -> (Matrix, Matrix)
gram_schmidt_helper []     us mus = (us, mus)
gram_schmidt_helper (v:vs) us mus = gram_schmidt_helper vs (u:us) (mu:mus)
  where
    mu = map (flip proj_factor $ v) us
    u = foldl dot_minus v (zipWith scalar_mult mu us)

lll_helper :: Int -> Matrix -> Matrix
lll_helper k m
  | k + 1 == length m = m
  | k < 2             = lll_helper 2 m
  | otherwise         = m

m = [[3,1], [2,2]] :: Matrix
m' = [[3,1,3],[2,3,4],[0,1,5]] :: Matrix

-- legacy stuff:

-- Gram–Schmidt orthogonalization
gram_schmidt_orthg :: Matrix -> Matrix
gram_schmidt_orthg m = step m []
  where
  step :: Matrix -> [ColVector] -> Matrix
  step [] _      = []
  step (v:vs) us = let u = foldl (\v' u' -> dot_minus v' (proj u' v')) v us
                   in u:(step vs (u:us))

-- magnitude ||v|| of a vector
magnitude :: ColVector -> Rational
magnitude v = toRational . sqrt . fromRational $ inner_product v v

--normalize vector
normalize :: ColVector -> ColVector
normalize v = scalar_mult (1 / (magnitude v)) v
