-- module Main
--     where

import Control.Monad (guard)
-- Scrivere una funzione haskell che calcoli la formula:
-- v(A+B^T)v^T + v(A^T+B)v^T

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
f :: (Eq a, Num a) => Vector a -> Matrix a -> Matrix a -> Maybe a
f v a b = do
    guard (checkSize v a b)
    let ab = sumMat a b
    let abba = sumTransposeMat ab
    let va = apply abba v
    return (productVec v va)
--  return (productVec v $ apply (sumTranspose $ sumMat a b) v)

-- La funzione f senza controlli e senza semplificazioni
-- Per controllare se f è corretta
naiveF :: (Eq a, Num a) => Vector a -> Matrix a -> Matrix a -> a
naiveF v a b = (productVec v ((apply (sumMat a (transposeMat b)) v))) + (productVec v ((apply (sumMat (transposeMat a) b) v)))

-- Esegue f validando l'input
-- Se l'input non è compresso lo comprime
-- Se nexp ed vexp sono invalidi restituisce Nothing
validateF :: (Eq a, Num a) => Vector a -> Matrix a -> Matrix a -> Maybe a
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

sumMat :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
sumMat (Mat nexp mata) (Mat _ matb) = Mat nexp (sumQT mata matb)

sumQT :: (Eq a, Num a) => QT a -> QT a -> QT a
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

sumTransposeMat :: (Eq a, Num a) => Matrix a -> Matrix a
sumTransposeMat (Mat nexp mat) = Mat nexp (sumTransposeQT mat)

sumTransposeQT :: Eq a => Num a => QT a -> QT a
sumTransposeQT (C m) = C (m * 2)
sumTransposeQT (Q ul ur ll lr) = mergeQT ul' ur' ll' lr'
    where ul' = sumTransposeQT ul
          ur' = sumQT ur $ transposeQT ll
          ll' = transposeQT ur'
          lr' = sumTransposeQT lr


apply :: (Eq a, Num a) => Matrix a -> Vector a -> Vector a
apply (Mat nexp mat) (Vec _ vec) = Vec nexp (deepApply nexp mat vec)

deepApply :: (Eq a, Num a) => Int -> QT a -> BT a -> BT a
deepApply exp (C a) (F v) = F ((a * v) * (2 ^ exp))
deepApply exp (C a) (N u l) = mergeBT p' p'
    where l' = deepApply (exp - 1) (C a) u
          r' = deepApply (exp - 1) (C a) l
          p' = sumBT l' r'
deepApply exp (Q ul ur ll lr) (F v) = mergeBT (sumBT ul' ur') (sumBT ll' lr')
    where ul' = deepApply (exp - 1) ul (F v)
          ur' = deepApply (exp - 1) ur (F v)
          ll' = deepApply (exp - 1) ll (F v)
          lr' = deepApply (exp - 1) lr (F v)
deepApply exp (Q ul ur ll lr) (N u l) = mergeBT (sumBT ul' ur') (sumBT ll' lr')
    where ul' = deepApply (exp - 1) ul u
          ur' = deepApply (exp - 1) ur l
          ll' = deepApply (exp - 1) ll u
          lr' = deepApply (exp - 1) lr l

sumBT :: (Eq a, Num a) => BT a -> BT a -> BT a
sumBT (F a) (F b) = F (a + b)
sumBT (F a) (N u l) = mergeBT u' l'
    where u' = sumBT (F a) u
          l' = sumBT (F a) l
sumBT (N u l) (F b) = mergeBT u' l'
    where u' = sumBT u (F b)
          l' = sumBT l (F b)
sumBT (N la ra) (N lb rb) = mergeBT u' l'
    where u' = sumBT la lb
          l' = sumBT ra rb

productVec :: Num a => Vector a -> Vector a -> a
productVec (Vec vexp veca) (Vec _ vecb) = deepProduct vexp veca vecb

deepProduct :: Num a => Int -> BT a -> BT a -> a
deepProduct exp (F a) (F b) = (a * b) * (2 ^ exp)
deepProduct exp (F a) (N u l) = u' + l'
    where u' = deepProduct (exp - 1) (F a) u
          l' = deepProduct (exp - 1) (F a) l
deepProduct exp (N u l) (F b) = u' + l'
    where u' = deepProduct (exp - 1) u (F b)
          l' = deepProduct (exp - 1) l (F b)
deepProduct exp (N la ra) (N lb rb) = u' + l'
    where u' = deepProduct (exp - 1) la lb
          l' = deepProduct (exp - 1) ra rb

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
maybeCompressVecRec exp (N u l) | exp < 1 = Nothing
                                        | otherwise = do
                                            u' <- maybeCompressVecRec (exp - 1) u
                                            l' <- maybeCompressVecRec (exp - 1) l
                                            return (mergeBT u' l')
maybeCompressVecRec exp (F x) | exp < 0 = Nothing
                              | otherwise = Just (F x)

mergeBT :: Eq a => BT a -> BT a ->BT a
mergeBT (F u) (F l) | u == l = F u
                    | otherwise = N (F u) (F l)
mergeBT u l = N u l

-- Controlla che le dimensioni combacino
checkSize :: Vector a -> Matrix a -> Matrix a -> Bool
checkSize v a b = (v' == a') && (v' == b')
    where v' = vexp v
          a' = nexp a
          b' = nexp b

----------------------------------------------------------------------
-- Test per code coverage
-- bisogna decommentare l'header del modulo

-- qm0 x = Mat x $ C 0
-- qm2 = Mat 2 $ Q (C 1) (Q (C 1) (C 2) (C 3) (C 4)) (C 2) (C 4)
-- qm3 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 1) (C 0) (C 6))
-- qm4 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 0) (C 0) (C 6))
-- qm5 = Mat 2 $ Q (C 1) (Q (C 4) (C 3) (C 1) (C 2)) (C 2) (C 4)
-- qm6 = Mat 2 $ Q (C 1) (Q (C 1) (C 2) (C 3) (C 4)) (C 3) (Q (C 1) (C 2) (C 4) (C 3))
-- id2 = Mat 2 $ Q (Q (C 1) (C 0) (C 0) (C 1)) (C 0) (C 0) (Q (C 1) (C 0) (C 0) (C 1))
-- rot2= Mat 2 $ Q (C 0) (Q (C 0) (C 1) (C 1) (C 0)) (Q (C 0) (C 1) (C 1) (C 0)) (C 0)
-- 
-- v0 x = Vec x $ F 0
-- v1 = Vec 2 (N (F 1) (N (F 0) (F 2)))
-- v2 = Vec 2 (N (N (F 0) (F 2)) (F 1))
-- v3 = Vec 2 (N (F 1) (N (F 2) (F 3)))
-- v4 = Vec 2 (N (N (F 1) (F 2)) (F 3))
-- 
-- main = do
--     print $ validateF (Vec 0 (N (F 1) (F 2))) (qm0 0) (qm0 0)
--     print $ validateF (v0 0) (Mat 0 (Q (C 1) (C 2) (C 2) (C 2))) (qm0 0)
--     print $ validateF (v0 0) (qm0 0) (Mat 0 (Q (C 1) (C 2) (C 2) (C 2)))
--     print $ validateF (v0 1) (qm0 0) (qm0 0)
--     print $ validateF (v0 0) (qm0 1) (qm0 0)
--     print $ validateF (v0 0) (qm0 0) (qm0 1)
--     print $ maybeCompressMat (Mat 1 $ Q (C 0) (C 0) (C 0) (C 0))
--     print $ maybeCompressVec (Vec 1 $ N (F 0) (F 0))
--     print $ validateF v1 qm3 qm4
--     print $ f v1 qm2 (qm0 2) -- 80 (F con C, N con Q, F con C, N con C)
--     print $ f v2 qm2 (qm0 2) -- 84 (N con C, F con Q, N con C, F con C)
--     print $ f v1 (qm0 2) qm2 -- 80
--     print $ f v2 (qm0 2) qm2 -- 84
--     print $ f v1 qm5 qm6 -- 156 (C ricorsiva, Q sommata a C, trasposta, Q ricorsiva)
--     print $ f v2 qm5 qm6 -- 148
--     print $ f v1 qm6 qm5 -- 156
--     print $ f v2 qm6 qm5 -- 148
--     print $ f v3 id2 (qm0 2) -- 30 (F con F ed N con N)
--     print $ f v4 id2 (qm0 2) -- 46
--     print $ f v3 rot2 (qm0 2) -- 20 (F con N ed N con F)
--     print $ f v4 rot2 (qm0 2) -- 36