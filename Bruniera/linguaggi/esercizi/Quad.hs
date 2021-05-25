data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)

-- Alberi di esempio per test
t1 = let u = C 1 in buildNSimplify u u u (C 2)
t2 = let u = C 1 in let v = Q u u u u in let w = Q v v v t1 in Q w w w (Q v v v v)
t3 = simplify t2
t4 = Q (C 3) (C 3) (C 3) t3

------------------------------------------------------------------------------

-- BUILD AND SIMPLIFY

quadEq (C hl) (C hr) (C ll) (C lr) = (hl == hr) && (hr == ll) && (ll == lr)
quadEq _ _ _ _ = False

buildNSimplify hl hr ll lr | quadEq hl hr ll lr = hl
                           | otherwise = Q hl hr ll lr

------------------------------------------------------------------------------

-- SIMPLIFY

simplify (C x) = C x
simplify (Q hl hr ll lr) = buildNSimplify hl' hr' ll' lr'
    where hl' = simplify hl
          hr' = simplify hr
          ll' = simplify ll
          lr' = simplify lr

------------------------------------------------------------------------------

-- MAP

-- Notiamo che anche se C non sempre rappresenta un pixel, applicare la stessa funzione a pixel uguali
-- produce lo stesso risultato, quindi non è necessario scendere a livello del pixel
-- Notiamo che `quadMap id` è equivalente a simplify
quadMap f (C x) = C (f x)
quadMap f (Q hl hr ll lr) = buildNSimplify hl' hr' ll' lr'
    where hl' = quadMap f hl
          hr' = quadMap f hr
          ll' = quadMap f ll
          lr' = quadMap f lr

------------------------------------------------------------------------------

-- PIXEL MINIMI

howManyPixels (C x) =  1
howManyPixels (Q hl hr ll lr) = 4 * max (max hl' hr') (max ll' lr')
    where hl' = howManyPixels hl
          hr' = howManyPixels hr
          ll' = howManyPixels ll
          lr' = howManyPixels lr

------------------------------------------------------------------------------

-- INSERT PICTURE

insertPict qf _ (C False) = qf
insertPict _ qt (C True) = qt
insertPict (Q hlf hrf llf lrf)
           (Q hlt hrt llt lrt)
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict hlf hlt hlm
                     hr' = insertPict hrf hrt hrm
                     ll' = insertPict llf llt llm
                     lr' = insertPict lrf lrt lrm
insertPict qf
           (Q hlt hrt llt lrt)
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict qf hlt hlm
                     hr' = insertPict qf hrt hrm
                     ll' = insertPict qf llt llm
                     lr' = insertPict qf lrt lrm
insertPict (Q hlf hrf llf lrf)
           qt
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict hlf qt hlm
                     hr' = insertPict hrf qt hrm
                     ll' = insertPict llf qt llm
                     lr' = insertPict lrf qt lrm
insertPict qf
           qt
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict qf qt hlm
                     hr' = insertPict qf qt hrm
                     ll' = insertPict qf qt llm
                     lr' = insertPict qf qt lrm

------------------------------------------------------------------------------

-- INSERT LOGO

insertLogoRec i (Q hl hr ll lr) qt qm 
  | i >= 3 = buildNSimplify hl' hr ll lr
  | otherwise = buildNSimplify hl hr ll lr'
    where lr' = insertLogoRec (i+1) lr qt qm
          hl' = insertPict hl qt qm
insertLogoRec i qf qt qm 
  | i >= 3 = buildNSimplify hl' qf qf qf
  | otherwise = buildNSimplify qf qf qf lr'
    where lr' = insertLogoRec (i+1) qf qt qm
          hl' = insertPict qf qt qm


insertLogo :: (Eq a) => QT a -> QT a -> QT Bool -> QT a
insertLogo = insertLogoRec 1

------------------------------------------------------------------------------

-- ZIP WITH


quadZipWith (Q hlf hrf llf lrf) (Q hlt hrt llt lrt) f = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith hlf hlt f
          hr' = quadZipWith hrf hrt f
          ll' = quadZipWith llf llt f
          lr' = quadZipWith lrf lrt f
quadZipWith qf (Q hlt hrt llt lrt) f = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith qf hlt f
          hr' = quadZipWith qf hrt f
          ll' = quadZipWith qf llt f
          lr' = quadZipWith qf lrt f
quadZipWith (Q hlf hrf llf lrf) qt f = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith hlf qt f
          hr' = quadZipWith hrf qt f
          ll' = quadZipWith llf qt f
          lr' = quadZipWith lrf qt f
quadZipWith (C cf) (C ct) f = C (f cf ct)

