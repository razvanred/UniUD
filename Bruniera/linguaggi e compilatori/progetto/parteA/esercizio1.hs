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

-- La funzione f della consegna
-- L'unica validazione che esegue è controllare che le dimensioni siano corrette
f :: Eq a => Num a => Vector a -> Matrix a -> Matrix a -> Maybe a
f v a b = do
    guard (checkSize v a b)
    let ab = sumMat a b
    let abba = sumTransposeMat ab
    let va = apply abba v
    return (productVec v va)
--  return (vProduct v $ apply (sumTranspose $ sumMat a b) v)  

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

----------------------------------------------------------------------
-- Funzioni ausiliarie
-- Le validazioni desiderate sono state fatte prima
-- Tutte le funzioni ausiliarie assumono che l'input sia ben formato
-- Le funzioni ausiliarie si assicurano comunque che i risultati
-- intermedi siano validi (quindi compressi)

transposeMat :: Matrix a -> Matrix a
transposeMat (Mat nexp mat) = Mat nexp (transposeQT mat)

transposeQT :: QT a -> QT a
transposeQT (C x) = C x
transposeQT (Q ul ur ll lr) = Q ul' ll' ur' lr'
    where ul' = transposeQT ul
          ur' = transposeQT ur
          ll' = transposeQT ll
          lr' = transposeQT lr

sumMat :: Eq a => Num a => Matrix a -> Matrix a -> Matrix a
sumMat (Mat nexp mata) (Mat _ matb) = Mat nexp (sumQT mata matb)

sumQT :: Eq a => Num a => QT a -> QT a -> QT a
sumQT (C x) (C y) = C (x + y)
sumQT (C x) (Q ul ur ll lr) = mergeQT ul' ur' ll' lr'
    where ul' = sumQT (C x) ul
          ur' = sumQT (C x) ur
          ll' = sumQT (C x) ll
          lr' = sumQT (C x) lr
sumQT (Q ul ur ll lr) (C y) = mergeQT ul' ur' ll' lr'
    where ul' = sumQT ul (C y)
          ur' = sumQT ur (C y)
          ll' = sumQT ll (C y)
          lr' = sumQT lr (C y)
sumQT (Q ula ura lla lra) (Q ulb urb llb lrb) = mergeQT ul' ur' ll' lr'
    where ul' = sumQT ula ulb
          ur' = sumQT ura urb
          ll' = sumQT lla llb
          lr' = sumQT lra lrb

sumTransposeMat :: Eq a => Num a => Matrix a -> Matrix a
sumTransposeMat (Mat nexp mat) = Mat nexp (sumTransposeQT mat)

sumTransposeQT :: Eq a => Num a => QT a -> QT a
sumTransposeQT (C m) = C m
sumTransposeQT (Q ul ur ll lr) = mergeQT ul' ur' ll' lr'
    where ul' = sumTransposeQT ul
          ur' = sumQT ur $ transposeQT ll
          ll' = transposeQT ur'
          lr' = sumTransposeQT lr


apply :: Eq a => Num a => Matrix a -> Vector a -> Vector a
apply (Mat nexp mat) (Vec _ vec) = Vec nexp (deepApply nexp mat vec)

deepApply :: Eq a => Num a => Int -> QT a -> BT a -> BT a
deepApply exp (C a) (F v) = F ((a * v) * (2 ^ exp))
deepApply exp (C a) (N l r) = mergeBT l' r'
    where l' = deepApply (exp - 1) (C a) l
          r' = deepApply (exp - 1) (C a) r
deepApply exp (Q ul ur ll lr) (F v) = mergeBT (sumBT ul' ur') (sumBT ll' lr')
    where ul' = deepApply (exp - 1) ul (F v)
          ur' = deepApply (exp - 1) ur (F v)
          ll' = deepApply (exp - 1) ll (F v)
          lr' = deepApply (exp - 1) lr (F v)
deepApply exp (Q ul ur ll lr) (N l r) = mergeBT (sumBT ul' ur') (sumBT ll' lr')
    where ul' = deepApply (exp - 1) ul l
          ur' = deepApply (exp - 1) ur l
          ll' = deepApply (exp - 1) ll r
          lr' = deepApply (exp - 1) lr r

sumBT :: Eq a => Num a => BT a -> BT a -> BT a
sumBT (F a) (F b) = F (a + b)
sumBT (F a) (N l r) = mergeBT l' r'
    where l' = sumBT (F a) l
          r' = sumBT (F a) r
sumBT (N l r) (F b) = mergeBT l' r'
    where l' = sumBT l (F b)
          r' = sumBT r (F b)
sumBT (N la ra) (N lb rb) = mergeBT l' r'
    where l' = sumBT la lb
          r' = sumBT ra rb

productVec :: Num a => Vector a -> Vector a -> a
productVec (Vec vexp veca) (Vec _ vecb) = deepProduct vexp veca vecb

deepProduct :: Num a => Int -> BT a -> BT a -> a
deepProduct exp (F a) (F b) = (a * b) * (2 ^ exp)
deepProduct exp (F a) (N l r) = l' + r'
    where l' = deepProduct (exp - 1) (F a) l
          r' = deepProduct (exp - 1) (F a) r
deepProduct exp (N l r) (F b) = l' + r'
    where l' = deepProduct (exp - 1) l (F b)
          r' = deepProduct (exp - 1) r (F b)
deepProduct exp (N la ra) (N lb rb) = l' + r'
    where l' = deepProduct (exp - 1) la lb
          r' = deepProduct (exp - 1) ra rb

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