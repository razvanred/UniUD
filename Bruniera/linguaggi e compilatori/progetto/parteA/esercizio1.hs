import Control.Monad (guard)
----------------------------------------------------------------------
-- Strutture dati
data Matrix a = Mat {
    nexp :: Int ,
    mat :: QT a
} deriving (Eq, Show)

data QT a = C a | Q ( QT a ) ( QT a ) ( QT a ) ( QT a )
    deriving (Eq, Show)

data Vector a = Vec {
    vexp :: Int ,
    vec :: BT a
} deriving (Eq, Show)

data BT a = F a | N (BT a) (BT a)
    deriving (Eq, Show)

----------------------------------------------------------------------
-- Consegna

-- Esegue f validando l'input
-- Se l'input non è compresso lo comprime
-- Se nexp ed vexp sono invalidi restituisce Nothing
validateF :: Eq a => Num a => Vector a -> Matrix a -> Matrix a -> Maybe a
validateF v a b = do
    v' <- maybeCompressVec v
    a' <- maybeCompressMat a
    b' <- maybeCompressMat b
    s <- f v' a' b'
    return s

-- La funzione f della consegna
-- L'unica validazione che esegue è controllare che le dimensioni siano corrette
f :: Num a => Vector a -> Matrix a -> Matrix a -> Maybe a
f v a b = do
    guard (checkSize v a b)
    let ab = sumMat a b
    let abba = sumTranspose ab
    let va = apply abba v
    return (vProduct v va)
--  return (vProduct v $ apply (sumTranspose $ sumMat a b) v)  

----------------------------------------------------------------------
-- Funzioni ausiliarie

transposeMat :: Matrix a -> Matrix a
transposeMat (Mat nexp mat) = Mat nexp (transposeQT mat) --TODO

transposeQT :: QT a -> QT a
transposeQT (C x) = C x  --TODO

sumMat :: Num a => Matrix a -> Matrix a -> Matrix a
sumMat a b = a  --TODO

sumTranspose :: Num a => Matrix a -> Matrix a
sumTranspose m = m  --TODO

apply :: Num a => Matrix a -> Vector a -> Vector a
apply _ v = v --TODO

vProduct :: Num a => Vector a -> Vector a -> a
vProduct v _ = 0 --TODO

----------------------------------------------------------------------
-- Validazione
maybeCompressMat :: Eq a => Matrix a -> Maybe (Matrix a)
maybeCompressMat Mat {nexp, mat} = do 
    cMat <- maybeCompressMatRec nexp mat
    return (Mat nexp cMat)

maybeCompressMatRec :: Eq a => Int -> QT a -> Maybe (QT a)
maybeCompressMatRec exp (Q ul ur ll lr) | exp < 1 = Nothing
                                        | otherwise = do
                                            ul' <- maybeCompressMatRec (exp - 1) ul
                                            ur' <- maybeCompressMatRec (exp - 1) ur
                                            ll' <- maybeCompressMatRec (exp - 1) ll
                                            lr' <- maybeCompressMatRec (exp - 1) lr
                                            return (mergeQT ul' ur' ll' lr')
maybeCompressMatRec exp (C x) | exp < 0 = Nothing
                              | otherwise = Just (C x)

mergeQT :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
mergeQT (C ul) (C ur) (C ll) (C lr) | ul == ur && ul == ll && ul == lr = C ul
                                  | otherwise = Q (C ul) (C ur) (C ll) (C lr)
mergeQT ul ur ll lr = Q ul ur ll lr

-- Comprime la il vettore e controlla che nexp sia valido
maybeCompressVec :: Eq a => Vector a -> Maybe (Vector a)
maybeCompressVec Vec {vexp, vec} = do 
    cVec <- maybeCompressVecRec vexp vec
    return (Vec vexp cVec)

maybeCompressVecRec :: Eq a => Int -> BT a -> Maybe (BT a)
maybeCompressVecRec exp (N l r) | exp < 1 = Nothing
                                        | otherwise = do
                                            l' <- maybeCompressVecRec (exp - 1) l
                                            r' <- maybeCompressVecRec (exp - 1) r
                                            return (mergeBT l' r')
maybeCompressVecRec exp (F x) | exp < 0 = Nothing
                              | otherwise = Just (F x)

mergeBT :: Eq a => BT a -> BT a ->BT a
mergeBT (F l) (F r) | l == r = F l
                    | otherwise = N (F l) (F r)
mergeBT l r = N l r

-- Controlla che le dimensioni combacino
checkSize :: Vector a -> Matrix a -> Matrix a -> Bool
checkSize v a b = (v' == a') && (v' == b')
    where v' = vexp v
          a' = nexp a
          b' = nexp b