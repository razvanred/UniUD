data Matrix a = Mat
    { nexp :: Int,
      mat :: QT a
    }

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)

validate Mat {nexp = 0, mat = (Q {})} = Nothing
validate mat@Mat {nexp = 0, mat = (C _)} = Just mat
validate mat@Mat {nexp = nexp, mat = (Q a b c d)} =
    case (validate Mat {nexp = nnexp, mat = a}, validate Mat {nexp = nnexp, mat = b}, validate Mat {nexp = nnexp, mat = c}, validate Mat {nexp = nnexp, mat = d}) of
        (Just _, Just _, Just _, Just _)->mat
        _ -> Nothing
  where
    nnexp = nexp - 1

-- compress :: (Eq a) => QT a -> QT a
compress (C a) = C a
compress (Q (C a) (C b) (C c) (C d))
    | a == b && a == c && a == d = C a
compress (Q a b c d) =
    Q (compress a) (compress b) (compress c) (compress d)

mul (C a1) (C a2)=
    C (a1*a2)
mul (Q a1 b1 c1 d1), (C a2)=
    Q a2*mul a1+a*mul c1 a2*mul b1+a2*mul d1 a2*mul a1+a2*mul c1 a2*mul b1+a1*mul d1

