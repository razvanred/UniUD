-- [1 of 1] Compiling AST2             ( AST2.hs, AST2.o )

==================== Derived instances ====================
Derived class instances:
  instance Eq ASTData where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (Parse a1) -> case b of (Parse b1) -> ((a1 == b1))
                 (ResolveConstants a1)
                   -> case b of (ResolveConstants b1) -> ((a1 == b1))
                 _ -> True
  
  instance Ord ASTData where
    compare a b
      = case a of
          Parse a1
            -> case b of
                 Parse b1 -> (a1 `compare` b1)
                 _ -> LT
          ResolveConstants a1
            -> case b of
                 ResolveConstants b1 -> (a1 `compare` b1)
                 _ -> GT
    (<) a b
      = case a of
          Parse a1
            -> case b of
                 Parse b1 -> (a1 < b1)
                 _ -> True
          ResolveConstants a1
            -> case b of
                 ResolveConstants b1 -> (a1 < b1)
                 _ -> False
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Show ASTData where
    showsPrec a (Parse b1)
      = showParen (a >= 11) ((.) (showString "Parse ") (showsPrec 11 b1))
    showsPrec a (ResolveConstants b1)
      = showParen
          (a >= 11)
          ((.)
             (showString "ResolveConstants {")
             ((.)
                (showString "replacedFromConstant = ")
                ((.) (showsPrec 0 b1) (showString "}"))))
  
  instance Read ASTData where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "Parse")
                 a1 <- step readPrec
                 return (Parse a1))
             +++
               prec
                 11
                 (do expectP (Ident "ResolveConstants")
                     expectP (Punc "{")
                     a1 <- readField "replacedFromConstant" (reset readPrec)
                     expectP (Punc "}")
                     return (ResolveConstants a1)))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Eq a => Eq (Instruction a) where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (NestedBlock a1 a2 a3)
                   -> case b of
                        (NestedBlock b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (ConstantDecl a1 a2 a3 a4)
                   -> case b of
                        (ConstantDecl b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (VariableDecl a1 a2 a3 a4 a5)
                   -> case b of
                        (VariableDecl b1 b2 b3 b4 b5)
                          -> (((a1 == b1))
                                &&
                                  (((a2 == b2))
                                     && (((a3 == b3)) && (((a4 == b4)) && ((a5 == b5))))))
                 (FunctionDecl a1 a2 a3 a4 a5 a6)
                   -> case b of
                        (FunctionDecl b1 b2 b3 b4 b5 b6)
                          -> (((a1 == b1))
                                &&
                                  (((a2 == b2))
                                     &&
                                       (((a3 == b3))
                                          && (((a4 == b4)) && (((a5 == b5)) && ((a6 == b6)))))))
                 (Break a1 a2)
                   -> case b of (Break b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (Continue a1 a2)
                   -> case b of (Continue b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (ReturnVoid a1 a2)
                   -> case b of (ReturnVoid b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (ReturnExp a1 a2 a3)
                   -> case b of
                        (ReturnExp b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (While a1 a2 a3 a4)
                   -> case b of
                        (While b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (IfThen a1 a2 a3 a4)
                   -> case b of
                        (IfThen b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (IfThenElse a1 a2 a3 a4 a5)
                   -> case b of
                        (IfThenElse b1 b2 b3 b4 b5)
                          -> (((a1 == b1))
                                &&
                                  (((a2 == b2))
                                     && (((a3 == b3)) && (((a4 == b4)) && ((a5 == b5))))))
                 (Assignment a1 a2 a3 a4 a5)
                   -> case b of
                        (Assignment b1 b2 b3 b4 b5)
                          -> (((a1 == b1))
                                &&
                                  (((a2 == b2))
                                     && (((a3 == b3)) && (((a4 == b4)) && ((a5 == b5))))))
                 (Expression a1 a2 a3)
                   -> case b of
                        (Expression b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 _ -> True
  
  instance Ord a => Ord (Instruction a) where
    compare a b
      = case a of
          NestedBlock a1 a2 a3
            -> case b of
                 NestedBlock b1 b2 b3
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ -> (a3 `compare` b3)
                               GT -> GT
                        GT -> GT
                 _ -> LT
          ConstantDecl a1 a2 a3 a4
            -> case b of
                 NestedBlock {} -> GT
                 ConstantDecl b1 b2 b3 b4
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ -> (a4 `compare` b4)
                                      GT -> GT
                               GT -> GT
                        GT -> GT
                 _ -> LT
          VariableDecl a1 a2 a3 a4 a5
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 2#)) then
                          LT
                      else
                          case b of
                            VariableDecl b1 b2 b3 b4 b5
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ
                                                   -> case (compare a4 b4) of
                                                        LT -> LT
                                                        EQ -> (a5 `compare` b5)
                                                        GT -> GT
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> GT
          FunctionDecl a1 a2 a3 a4 a5 a6
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 3#)) then
                          LT
                      else
                          case b of
                            FunctionDecl b1 b2 b3 b4 b5 b6
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ
                                                   -> case (compare a4 b4) of
                                                        LT -> LT
                                                        EQ
                                                          -> case (compare a5 b5) of
                                                               LT -> LT
                                                               EQ -> (a6 `compare` b6)
                                                               GT -> GT
                                                        GT -> GT
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> GT
          Break a1 a2
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 4#)) then
                          LT
                      else
                          case b of
                            Break b1 b2
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ -> (a2 `compare` b2)
                                   GT -> GT
                            _ -> GT
          Continue a1 a2
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 5#)) then
                          LT
                      else
                          case b of
                            Continue b1 b2
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ -> (a2 `compare` b2)
                                   GT -> GT
                            _ -> GT
          ReturnVoid a1 a2
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 6#)) then
                          LT
                      else
                          case b of
                            ReturnVoid b1 b2
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ -> (a2 `compare` b2)
                                   GT -> GT
                            _ -> GT
          ReturnExp a1 a2 a3
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 7#)) then
                          GT
                      else
                          case b of
                            ReturnExp b1 b2 b3
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ -> (a3 `compare` b3)
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          While a1 a2 a3 a4
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 8#)) then
                          GT
                      else
                          case b of
                            While b1 b2 b3 b4
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ -> (a4 `compare` b4)
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          IfThen a1 a2 a3 a4
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 9#)) then
                          GT
                      else
                          case b of
                            IfThen b1 b2 b3 b4
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ -> (a4 `compare` b4)
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          IfThenElse a1 a2 a3 a4 a5
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 10#)) then
                          GT
                      else
                          case b of
                            IfThenElse b1 b2 b3 b4 b5
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ
                                                   -> case (compare a4 b4) of
                                                        LT -> LT
                                                        EQ -> (a5 `compare` b5)
                                                        GT -> GT
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          Assignment a1 a2 a3 a4 a5
            -> case b of
                 Expression {} -> LT
                 Assignment b1 b2 b3 b4 b5
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ
                                        -> case (compare a4 b4) of
                                             LT -> LT
                                             EQ -> (a5 `compare` b5)
                                             GT -> GT
                                      GT -> GT
                               GT -> GT
                        GT -> GT
                 _ -> GT
          Expression a1 a2 a3
            -> case b of
                 Expression b1 b2 b3
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ -> (a3 `compare` b3)
                               GT -> GT
                        GT -> GT
                 _ -> GT
  
  instance Show a => Show (Instruction a) where
    showsPrec a (NestedBlock b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "NestedBlock ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (ConstantDecl b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "ConstantDecl ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (VariableDecl b1 b2 b3 b4 b5)
      = showParen
          (a >= 11)
          ((.)
             (showString "VariableDecl ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3)
                            ((.)
                               showSpace
                               ((.) (showsPrec 11 b4) ((.) showSpace (showsPrec 11 b5))))))))))
    showsPrec a (FunctionDecl b1 b2 b3 b4 b5 b6)
      = showParen
          (a >= 11)
          ((.)
             (showString "FunctionDecl ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4)
                                  ((.)
                                     showSpace
                                     ((.)
                                        (showsPrec 11 b5)
                                        ((.) showSpace (showsPrec 11 b6))))))))))))
    showsPrec a (Break b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "Break ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (Continue b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "Continue ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (ReturnVoid b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "ReturnVoid ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (ReturnExp b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "ReturnExp ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (While b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "While ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (IfThen b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "IfThen ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (IfThenElse b1 b2 b3 b4 b5)
      = showParen
          (a >= 11)
          ((.)
             (showString "IfThenElse ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3)
                            ((.)
                               showSpace
                               ((.) (showsPrec 11 b4) ((.) showSpace (showsPrec 11 b5))))))))))
    showsPrec a (Assignment b1 b2 b3 b4 b5)
      = showParen
          (a >= 11)
          ((.)
             (showString "Assignment ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3)
                            ((.)
                               showSpace
                               ((.) (showsPrec 11 b4) ((.) showSpace (showsPrec 11 b5))))))))))
    showsPrec a (Expression b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "Expression ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
  
  instance Read a => Read (Instruction a) where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "NestedBlock")
                 a1 <- step readPrec
                 a2 <- step readPrec
                 a3 <- step readPrec
                 return (NestedBlock a1 a2 a3))
             +++
               (prec
                  10
                  (do expectP (Ident "ConstantDecl")
                      a1 <- step readPrec
                      a2 <- step readPrec
                      a3 <- step readPrec
                      a4 <- step readPrec
                      return (ConstantDecl a1 a2 a3 a4))
                  +++
                    (prec
                       10
                       (do expectP (Ident "VariableDecl")
                           a1 <- step readPrec
                           a2 <- step readPrec
                           a3 <- step readPrec
                           a4 <- step readPrec
                           a5 <- step readPrec
                           return (VariableDecl a1 a2 a3 a4 a5))
                       +++
                         (prec
                            10
                            (do expectP (Ident "FunctionDecl")
                                a1 <- step readPrec
                                a2 <- step readPrec
                                a3 <- step readPrec
                                a4 <- step readPrec
                                a5 <- step readPrec
                                a6 <- step readPrec
                                return (FunctionDecl a1 a2 a3 a4 a5 a6))
                            +++
                              (prec
                                 10
                                 (do expectP (Ident "Break")
                                     a1 <- step readPrec
                                     a2 <- step readPrec
                                     return (Break a1 a2))
                                 +++
                                   (prec
                                      10
                                      (do expectP (Ident "Continue")
                                          a1 <- step readPrec
                                          a2 <- step readPrec
                                          return (Continue a1 a2))
                                      +++
                                        (prec
                                           10
                                           (do expectP (Ident "ReturnVoid")
                                               a1 <- step readPrec
                                               a2 <- step readPrec
                                               return (ReturnVoid a1 a2))
                                           +++
                                             (prec
                                                10
                                                (do expectP (Ident "ReturnExp")
                                                    a1 <- step readPrec
                                                    a2 <- step readPrec
                                                    a3 <- step readPrec
                                                    return (ReturnExp a1 a2 a3))
                                                +++
                                                  (prec
                                                     10
                                                     (do expectP (Ident "While")
                                                         a1 <- step readPrec
                                                         a2 <- step readPrec
                                                         a3 <- step readPrec
                                                         a4 <- step readPrec
                                                         return (While a1 a2 a3 a4))
                                                     +++
                                                       (prec
                                                          10
                                                          (do expectP (Ident "IfThen")
                                                              a1 <- step readPrec
                                                              a2 <- step readPrec
                                                              a3 <- step readPrec
                                                              a4 <- step readPrec
                                                              return (IfThen a1 a2 a3 a4))
                                                          +++
                                                            (prec
                                                               10
                                                               (do expectP (Ident "IfThenElse")
                                                                   a1 <- step readPrec
                                                                   a2 <- step readPrec
                                                                   a3 <- step readPrec
                                                                   a4 <- step readPrec
                                                                   a5 <- step readPrec
                                                                   return
                                                                     (IfThenElse a1 a2 a3 a4 a5))
                                                               +++
                                                                 (prec
                                                                    10
                                                                    (do expectP (Ident "Assignment")
                                                                        a1 <- step readPrec
                                                                        a2 <- step readPrec
                                                                        a3 <- step readPrec
                                                                        a4 <- step readPrec
                                                                        a5 <- step readPrec
                                                                        return
                                                                          (Assignment
                                                                             a1 a2 a3 a4 a5))
                                                                    +++
                                                                      prec
                                                                        10
                                                                        (do expectP
                                                                              (Ident "Expression")
                                                                            a1 <- step readPrec
                                                                            a2 <- step readPrec
                                                                            a3 <- step readPrec
                                                                            return
                                                                              (Expression
                                                                                 a1 a2
                                                                                 a3))))))))))))))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Functor Instruction where
    fmap f (NestedBlock a1 a2 a3)
      = NestedBlock a1 (fmap (\ b1 -> fmap f b1) a2) (f a3)
    fmap f (ConstantDecl a1 a2 a3 a4)
      = ConstantDecl a1 a2 (fmap f a3) (f a4)
    fmap f (VariableDecl a1 a2 a3 a4 a5)
      = VariableDecl a1 a2 (fmap f a3) (fmap f a4) (f a5)
    fmap f (FunctionDecl a1 a2 a3 a4 a5 a6)
      = FunctionDecl
          a1 a2 (fmap (\ b1 -> fmap f b1) a3) (fmap f a4)
          (fmap (\ b2 -> fmap f b2) a5) (f a6)
    fmap f (Break a1 a2) = Break a1 (f a2)
    fmap f (Continue a1 a2) = Continue a1 (f a2)
    fmap f (ReturnVoid a1 a2) = ReturnVoid a1 (f a2)
    fmap f (ReturnExp a1 a2 a3) = ReturnExp a1 (fmap f a2) (f a3)
    fmap f (While a1 a2 a3 a4)
      = While a1 (fmap f a2) (fmap (\ b1 -> fmap f b1) a3) (f a4)
    fmap f (IfThen a1 a2 a3 a4)
      = IfThen a1 (fmap f a2) (fmap (\ b1 -> fmap f b1) a3) (f a4)
    fmap f (IfThenElse a1 a2 a3 a4 a5)
      = IfThenElse
          a1 (fmap f a2) (fmap (\ b1 -> fmap f b1) a3)
          (fmap (\ b2 -> fmap f b2) a4) (f a5)
    fmap f (Assignment a1 a2 a3 a4 a5)
      = Assignment a1 (fmap f a2) a3 (fmap f a4) (f a5)
    fmap f (Expression a1 a2 a3) = Expression a1 (fmap f a2) (f a3)
    (<$) z (NestedBlock a1 a2 a3)
      = NestedBlock a1 (fmap (\ b1 -> (<$) z b1) a2) z
    (<$) z (ConstantDecl a1 a2 a3 a4)
      = ConstantDecl a1 a2 ((<$) z a3) z
    (<$) z (VariableDecl a1 a2 a3 a4 a5)
      = VariableDecl a1 a2 ((<$) z a3) ((<$) z a4) z
    (<$) z (FunctionDecl a1 a2 a3 a4 a5 a6)
      = FunctionDecl
          a1 a2 (fmap (\ b1 -> (<$) z b1) a3) ((<$) z a4)
          (fmap (\ b2 -> (<$) z b2) a5) z
    (<$) z (Break a1 a2) = Break a1 z
    (<$) z (Continue a1 a2) = Continue a1 z
    (<$) z (ReturnVoid a1 a2) = ReturnVoid a1 z
    (<$) z (ReturnExp a1 a2 a3) = ReturnExp a1 ((<$) z a2) z
    (<$) z (While a1 a2 a3 a4)
      = While a1 ((<$) z a2) (fmap (\ b1 -> (<$) z b1) a3) z
    (<$) z (IfThen a1 a2 a3 a4)
      = IfThen a1 ((<$) z a2) (fmap (\ b1 -> (<$) z b1) a3) z
    (<$) z (IfThenElse a1 a2 a3 a4 a5)
      = IfThenElse
          a1 ((<$) z a2) (fmap (\ b1 -> (<$) z b1) a3)
          (fmap (\ b2 -> (<$) z b2) a4) z
    (<$) z (Assignment a1 a2 a3 a4 a5)
      = Assignment a1 ((<$) z a2) a3 ((<$) z a4) z
    (<$) z (Expression a1 a2 a3) = Expression a1 ((<$) z a2) z
  
  instance Foldable Instruction where
    foldr f z (NestedBlock a1 a2 a3)
      = (\ b3 b4 -> foldr (\ b1 b2 -> foldr f b2 b1) b4 b3) a2 (f a3 z)
    foldr f z (ConstantDecl a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1) a3 (f a4 z)
    foldr f z (VariableDecl a1 a2 a3 a4 a5)
      = (\ b1 b2 -> foldr f b2 b1)
          a3 ((\ b3 b4 -> foldr f b4 b3) a4 (f a5 z))
    foldr f z (FunctionDecl a1 a2 a3 a4 a5 a6)
      = (\ b3 b4 -> foldr (\ b1 b2 -> foldr f b2 b1) b4 b3)
          a3
          ((\ b5 b6 -> foldr f b6 b5)
             a4
             ((\ b9 b10 -> foldr (\ b7 b8 -> foldr f b8 b7) b10 b9)
                a5 (f a6 z)))
    foldr f z (Break a1 a2) = f a2 z
    foldr f z (Continue a1 a2) = f a2 z
    foldr f z (ReturnVoid a1 a2) = f a2 z
    foldr f z (ReturnExp a1 a2 a3)
      = (\ b1 b2 -> foldr f b2 b1) a2 (f a3 z)
    foldr f z (While a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1)
          a2
          ((\ b5 b6 -> foldr (\ b3 b4 -> foldr f b4 b3) b6 b5) a3 (f a4 z))
    foldr f z (IfThen a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1)
          a2
          ((\ b5 b6 -> foldr (\ b3 b4 -> foldr f b4 b3) b6 b5) a3 (f a4 z))
    foldr f z (IfThenElse a1 a2 a3 a4 a5)
      = (\ b1 b2 -> foldr f b2 b1)
          a2
          ((\ b5 b6 -> foldr (\ b3 b4 -> foldr f b4 b3) b6 b5)
             a3
             ((\ b9 b10 -> foldr (\ b7 b8 -> foldr f b8 b7) b10 b9)
                a4 (f a5 z)))
    foldr f z (Assignment a1 a2 a3 a4 a5)
      = (\ b1 b2 -> foldr f b2 b1)
          a2 ((\ b3 b4 -> foldr f b4 b3) a4 (f a5 z))
    foldr f z (Expression a1 a2 a3)
      = (\ b1 b2 -> foldr f b2 b1) a2 (f a3 z)
    foldMap f (NestedBlock a1 a2 a3)
      = mappend (foldMap (foldMap f) a2) (f a3)
    foldMap f (ConstantDecl a1 a2 a3 a4)
      = mappend (foldMap f a3) (f a4)
    foldMap f (VariableDecl a1 a2 a3 a4 a5)
      = mappend (foldMap f a3) (mappend (foldMap f a4) (f a5))
    foldMap f (FunctionDecl a1 a2 a3 a4 a5 a6)
      = mappend
          (foldMap (foldMap f) a3)
          (mappend (foldMap f a4) (mappend (foldMap (foldMap f) a5) (f a6)))
    foldMap f (Break a1 a2) = f a2
    foldMap f (Continue a1 a2) = f a2
    foldMap f (ReturnVoid a1 a2) = f a2
    foldMap f (ReturnExp a1 a2 a3) = mappend (foldMap f a2) (f a3)
    foldMap f (While a1 a2 a3 a4)
      = mappend (foldMap f a2) (mappend (foldMap (foldMap f) a3) (f a4))
    foldMap f (IfThen a1 a2 a3 a4)
      = mappend (foldMap f a2) (mappend (foldMap (foldMap f) a3) (f a4))
    foldMap f (IfThenElse a1 a2 a3 a4 a5)
      = mappend
          (foldMap f a2)
          (mappend
             (foldMap (foldMap f) a3) (mappend (foldMap (foldMap f) a4) (f a5)))
    foldMap f (Assignment a1 a2 a3 a4 a5)
      = mappend (foldMap f a2) (mappend (foldMap f a4) (f a5))
    foldMap f (Expression a1 a2 a3) = mappend (foldMap f a2) (f a3)
    null (NestedBlock _ _ _) = False
    null (ConstantDecl _ _ _ _) = False
    null (VariableDecl _ _ _ _ _) = False
    null (FunctionDecl _ _ _ _ _ _) = False
    null (Break _ _) = False
    null (Continue _ _) = False
    null (ReturnVoid _ _) = False
    null (ReturnExp _ _ _) = False
    null (While _ _ _ _) = False
    null (IfThen _ _ _ _) = False
    null (IfThenElse _ _ _ _ _) = False
    null (Assignment _ _ _ _ _) = False
    null (Expression _ _ _) = False
  
  instance Traversable Instruction where
    traverse f (NestedBlock a1 a2 a3)
      = liftA2
          (\ b2 b3 -> NestedBlock a1 b2 b3) (traverse (traverse f) a2) (f a3)
    traverse f (ConstantDecl a1 a2 a3 a4)
      = liftA2
          (\ b3 b4 -> ConstantDecl a1 a2 b3 b4) (traverse f a3) (f a4)
    traverse f (VariableDecl a1 a2 a3 a4 a5)
      = (<*>)
          (liftA2
             (\ b3 b4 b5 -> VariableDecl a1 a2 b3 b4 b5) (traverse f a3)
             (traverse f a4))
          (f a5)
    traverse f (FunctionDecl a1 a2 a3 a4 a5 a6)
      = (<*>)
          ((<*>)
             (liftA2
                (\ b3 b4 b5 b6 -> FunctionDecl a1 a2 b3 b4 b5 b6)
                (traverse (traverse f) a3) (traverse f a4))
             (traverse (traverse f) a5))
          (f a6)
    traverse f (Break a1 a2) = fmap (\ b2 -> Break a1 b2) (f a2)
    traverse f (Continue a1 a2) = fmap (\ b2 -> Continue a1 b2) (f a2)
    traverse f (ReturnVoid a1 a2)
      = fmap (\ b2 -> ReturnVoid a1 b2) (f a2)
    traverse f (ReturnExp a1 a2 a3)
      = liftA2 (\ b2 b3 -> ReturnExp a1 b2 b3) (traverse f a2) (f a3)
    traverse f (While a1 a2 a3 a4)
      = (<*>)
          (liftA2
             (\ b2 b3 b4 -> While a1 b2 b3 b4) (traverse f a2)
             (traverse (traverse f) a3))
          (f a4)
    traverse f (IfThen a1 a2 a3 a4)
      = (<*>)
          (liftA2
             (\ b2 b3 b4 -> IfThen a1 b2 b3 b4) (traverse f a2)
             (traverse (traverse f) a3))
          (f a4)
    traverse f (IfThenElse a1 a2 a3 a4 a5)
      = (<*>)
          ((<*>)
             (liftA2
                (\ b2 b3 b4 b5 -> IfThenElse a1 b2 b3 b4 b5) (traverse f a2)
                (traverse (traverse f) a3))
             (traverse (traverse f) a4))
          (f a5)
    traverse f (Assignment a1 a2 a3 a4 a5)
      = (<*>)
          (liftA2
             (\ b2 b4 b5 -> Assignment a1 b2 a3 b4 b5) (traverse f a2)
             (traverse f a4))
          (f a5)
    traverse f (Expression a1 a2 a3)
      = liftA2 (\ b2 b3 -> Expression a1 b2 b3) (traverse f a2) (f a3)
  
  instance Eq a => Eq (Parameter a) where
    (==) (Param a1 a2 a3 a4) (Param b1 b2 b3 b4)
      = (((a1 == b1))
           && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
  
  instance Ord a => Ord (Parameter a) where
    compare a b
      = case a of
          Param a1 a2 a3 a4
            -> case b of
                 Param b1 b2 b3 b4
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ -> (a4 `compare` b4)
                                      GT -> GT
                               GT -> GT
                        GT -> GT
    (<) a b
      = case a of
          Param a1 a2 a3 a4
            -> case b of
                 Param b1 b2 b3 b4
                   -> case (compare a1 b1) of
                        LT -> True
                        EQ
                          -> case (compare a2 b2) of
                               LT -> True
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> True
                                      EQ -> (a4 < b4)
                                      GT -> False
                               GT -> False
                        GT -> False
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Show a => Show (Parameter a) where
    showsPrec a (Param b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "Param ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
  
  instance Read a => Read (Parameter a) where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "Param")
                 a1 <- step readPrec
                 a2 <- step readPrec
                 a3 <- step readPrec
                 a4 <- step readPrec
                 return (Param a1 a2 a3 a4)))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Functor Parameter where
    fmap f (Param a1 a2 a3 a4) = Param a1 a2 (fmap f a3) (f a4)
    (<$) z (Param a1 a2 a3 a4) = Param a1 a2 ((<$) z a3) z
  
  instance Foldable Parameter where
    foldr f z (Param a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1) a3 (f a4 z)
    foldMap f (Param a1 a2 a3 a4) = mappend (foldMap f a3) (f a4)
    null (Param _ _ _ _) = False
  
  instance Traversable Parameter where
    traverse f (Param a1 a2 a3 a4)
      = liftA2 (\ b3 b4 -> Param a1 a2 b3 b4) (traverse f a3) (f a4)
  
  instance Eq a => Eq (DeclType a) where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (ArrayType a1 a2)
                   -> case b of (ArrayType b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (PointerType a1) -> case b of (PointerType b1) -> ((a1 == b1))
                 _ -> True
  
  instance Ord a => Ord (DeclType a) where
    compare a b
      = case a of
          ArrayType a1 a2
            -> case b of
                 PointerType {} -> LT
                 ArrayType b1 b2
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ -> (a2 `compare` b2)
                        GT -> GT
                 _ -> GT
          PointerType a1
            -> case b of
                 PointerType b1 -> (a1 `compare` b1)
                 _ -> GT
          _ -> case (dataToTag# a) of
                 a#
                   -> case (dataToTag# b) of
                        b#
                          -> if (tagToEnum# (a# <# b#)) :: Bool then
                                 LT
                             else
                                 if (tagToEnum# (a# ==# b#)) :: Bool then EQ else GT
  
  instance Show a => Show (DeclType a) where
    showsPrec _ BoolType = showString "BoolType"
    showsPrec _ CharType = showString "CharType"
    showsPrec _ IntType = showString "IntType"
    showsPrec _ StringType = showString "StringType"
    showsPrec _ FloatType = showString "FloatType"
    showsPrec _ VoidType = showString "VoidType"
    showsPrec a (ArrayType b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "ArrayType ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (PointerType b1)
      = showParen
          (a >= 11) ((.) (showString "PointerType ") (showsPrec 11 b1))
  
  instance Read a => Read (DeclType a) where
    readPrec
      = parens
          (choose
             [("BoolType", return BoolType), ("CharType", return CharType),
              ("IntType", return IntType), ("StringType", return StringType),
              ("FloatType", return FloatType), ("VoidType", return VoidType)]
             +++
               (prec
                  10
                  (do expectP (Ident "ArrayType")
                      a1 <- step readPrec
                      a2 <- step readPrec
                      return (ArrayType a1 a2))
                  +++
                    prec
                      10
                      (do expectP (Ident "PointerType")
                          a1 <- step readPrec
                          return (PointerType a1))))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Functor DeclType where
    fmap f BoolType = BoolType
    fmap f CharType = CharType
    fmap f IntType = IntType
    fmap f StringType = StringType
    fmap f FloatType = FloatType
    fmap f VoidType = VoidType
    fmap f (ArrayType a1 a2)
      = ArrayType (fmap (\ b1 -> fmap f b1) a1) (fmap f a2)
    fmap f (PointerType a1) = PointerType (fmap f a1)
    (<$) z BoolType = BoolType
    (<$) z CharType = CharType
    (<$) z IntType = IntType
    (<$) z StringType = StringType
    (<$) z FloatType = FloatType
    (<$) z VoidType = VoidType
    (<$) z (ArrayType a1 a2)
      = ArrayType (fmap (\ b1 -> (<$) z b1) a1) ((<$) z a2)
    (<$) z (PointerType a1) = PointerType ((<$) z a1)
  
  instance Foldable DeclType where
    foldr f z BoolType = z
    foldr f z CharType = z
    foldr f z IntType = z
    foldr f z StringType = z
    foldr f z FloatType = z
    foldr f z VoidType = z
    foldr f z (ArrayType a1 a2)
      = (\ b3 b4 -> foldr (\ b1 b2 -> foldr f b2 b1) b4 b3)
          a1 ((\ b5 b6 -> foldr f b6 b5) a2 z)
    foldr f z (PointerType a1) = (\ b1 b2 -> foldr f b2 b1) a1 z
    foldMap f BoolType = mempty
    foldMap f CharType = mempty
    foldMap f IntType = mempty
    foldMap f StringType = mempty
    foldMap f FloatType = mempty
    foldMap f VoidType = mempty
    foldMap f (ArrayType a1 a2)
      = mappend (foldMap (foldMap f) a1) (foldMap f a2)
    foldMap f (PointerType a1) = foldMap f a1
    null BoolType = True
    null CharType = True
    null IntType = True
    null StringType = True
    null FloatType = True
    null VoidType = True
    null (ArrayType a1 a2) = (&&) (all null a1) (null a2)
    null (PointerType a1) = null a1
  
  instance Traversable DeclType where
    traverse f BoolType = pure BoolType
    traverse f CharType = pure CharType
    traverse f IntType = pure IntType
    traverse f StringType = pure StringType
    traverse f FloatType = pure FloatType
    traverse f VoidType = pure VoidType
    traverse f (ArrayType a1 a2)
      = liftA2
          (\ b1 b2 -> ArrayType b1 b2) (traverse (traverse f) a1)
          (traverse f a2)
    traverse f (PointerType a1)
      = fmap (\ b1 -> PointerType b1) (traverse f a1)
  
  instance Eq a => Eq (Expr a) where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (UnaryOp a1 a2 a3 a4)
                   -> case b of
                        (UnaryOp b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (BinaryOp a1 a2 a3 a4 a5)
                   -> case b of
                        (BinaryOp b1 b2 b3 b4 b5)
                          -> (((a1 == b1))
                                &&
                                  (((a2 == b2))
                                     && (((a3 == b3)) && (((a4 == b4)) && ((a5 == b5))))))
                 (Ref a1 a2 a3)
                   -> case b of
                        (Ref b1 b2 b3) -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (Deref a1 a2 a3)
                   -> case b of
                        (Deref b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (ArrayAcc a1 a2 a3 a4)
                   -> case b of
                        (ArrayAcc b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (Id a1 a2 a3)
                   -> case b of
                        (Id b1 b2 b3) -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (FunctionCall a1 a2 a3 a4)
                   -> case b of
                        (FunctionCall b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 (BasicLiteral a1 a2 a3)
                   -> case b of
                        (BasicLiteral b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (ArrayLiteral a1 a2 a3)
                   -> case b of
                        (ArrayLiteral b1 b2 b3)
                          -> (((a1 == b1)) && (((a2 == b2)) && ((a3 == b3))))
                 (RangedArray a1 a2 a3 a4)
                   -> case b of
                        (RangedArray b1 b2 b3 b4)
                          -> (((a1 == b1))
                                && (((a2 == b2)) && (((a3 == b3)) && ((a4 == b4)))))
                 _ -> True
  
  instance Ord a => Ord (Expr a) where
    compare a b
      = case a of
          UnaryOp a1 a2 a3 a4
            -> case b of
                 UnaryOp b1 b2 b3 b4
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ -> (a4 `compare` b4)
                                      GT -> GT
                               GT -> GT
                        GT -> GT
                 _ -> LT
          BinaryOp a1 a2 a3 a4 a5
            -> case b of
                 UnaryOp {} -> GT
                 BinaryOp b1 b2 b3 b4 b5
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ
                                        -> case (compare a4 b4) of
                                             LT -> LT
                                             EQ -> (a5 `compare` b5)
                                             GT -> GT
                                      GT -> GT
                               GT -> GT
                        GT -> GT
                 _ -> LT
          Ref a1 a2 a3
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 2#)) then
                          LT
                      else
                          case b of
                            Ref b1 b2 b3
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ -> (a3 `compare` b3)
                                          GT -> GT
                                   GT -> GT
                            _ -> GT
          Deref a1 a2 a3
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 3#)) then
                          LT
                      else
                          case b of
                            Deref b1 b2 b3
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ -> (a3 `compare` b3)
                                          GT -> GT
                                   GT -> GT
                            _ -> GT
          ArrayAcc a1 a2 a3 a4
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 4#)) then
                          LT
                      else
                          case b of
                            ArrayAcc b1 b2 b3 b4
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ -> (a4 `compare` b4)
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> GT
          Id a1 a2 a3
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 5#)) then
                          GT
                      else
                          case b of
                            Id b1 b2 b3
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ -> (a3 `compare` b3)
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          FunctionCall a1 a2 a3 a4
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 6#)) then
                          GT
                      else
                          case b of
                            FunctionCall b1 b2 b3 b4
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ
                                            -> case (compare a3 b3) of
                                                 LT -> LT
                                                 EQ -> (a4 `compare` b4)
                                                 GT -> GT
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          BasicLiteral a1 a2 a3
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# <# 7#)) then
                          GT
                      else
                          case b of
                            BasicLiteral b1 b2 b3
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ
                                     -> case (compare a2 b2) of
                                          LT -> LT
                                          EQ -> (a3 `compare` b3)
                                          GT -> GT
                                   GT -> GT
                            _ -> LT
          ArrayLiteral a1 a2 a3
            -> case b of
                 RangedArray {} -> LT
                 ArrayLiteral b1 b2 b3
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ -> (a3 `compare` b3)
                               GT -> GT
                        GT -> GT
                 _ -> GT
          RangedArray a1 a2 a3 a4
            -> case b of
                 RangedArray b1 b2 b3 b4
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ
                          -> case (compare a2 b2) of
                               LT -> LT
                               EQ
                                 -> case (compare a3 b3) of
                                      LT -> LT
                                      EQ -> (a4 `compare` b4)
                                      GT -> GT
                               GT -> GT
                        GT -> GT
                 _ -> GT
  
  instance Show a => Show (Expr a) where
    showsPrec a (UnaryOp b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "UnaryOp ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (BinaryOp b1 b2 b3 b4 b5)
      = showParen
          (a >= 11)
          ((.)
             (showString "BinaryOp ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3)
                            ((.)
                               showSpace
                               ((.) (showsPrec 11 b4) ((.) showSpace (showsPrec 11 b5))))))))))
    showsPrec a (Ref b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "Ref ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (Deref b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "Deref ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (ArrayAcc b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "ArrayAcc ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (Id b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "Id ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (FunctionCall b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "FunctionCall ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
    showsPrec a (BasicLiteral b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "BasicLiteral ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (ArrayLiteral b1 b2 b3)
      = showParen
          (a >= 11)
          ((.)
             (showString "ArrayLiteral ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.) (showsPrec 11 b2) ((.) showSpace (showsPrec 11 b3))))))
    showsPrec a (RangedArray b1 b2 b3 b4)
      = showParen
          (a >= 11)
          ((.)
             (showString "RangedArray ")
             ((.)
                (showsPrec 11 b1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2)
                      ((.)
                         showSpace
                         ((.) (showsPrec 11 b3) ((.) showSpace (showsPrec 11 b4))))))))
  
  instance Read a => Read (Expr a) where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "UnaryOp")
                 a1 <- step readPrec
                 a2 <- step readPrec
                 a3 <- step readPrec
                 a4 <- step readPrec
                 return (UnaryOp a1 a2 a3 a4))
             +++
               (prec
                  10
                  (do expectP (Ident "BinaryOp")
                      a1 <- step readPrec
                      a2 <- step readPrec
                      a3 <- step readPrec
                      a4 <- step readPrec
                      a5 <- step readPrec
                      return (BinaryOp a1 a2 a3 a4 a5))
                  +++
                    (prec
                       10
                       (do expectP (Ident "Ref")
                           a1 <- step readPrec
                           a2 <- step readPrec
                           a3 <- step readPrec
                           return (Ref a1 a2 a3))
                       +++
                         (prec
                            10
                            (do expectP (Ident "Deref")
                                a1 <- step readPrec
                                a2 <- step readPrec
                                a3 <- step readPrec
                                return (Deref a1 a2 a3))
                            +++
                              (prec
                                 10
                                 (do expectP (Ident "ArrayAcc")
                                     a1 <- step readPrec
                                     a2 <- step readPrec
                                     a3 <- step readPrec
                                     a4 <- step readPrec
                                     return (ArrayAcc a1 a2 a3 a4))
                                 +++
                                   (prec
                                      10
                                      (do expectP (Ident "Id")
                                          a1 <- step readPrec
                                          a2 <- step readPrec
                                          a3 <- step readPrec
                                          return (Id a1 a2 a3))
                                      +++
                                        (prec
                                           10
                                           (do expectP (Ident "FunctionCall")
                                               a1 <- step readPrec
                                               a2 <- step readPrec
                                               a3 <- step readPrec
                                               a4 <- step readPrec
                                               return (FunctionCall a1 a2 a3 a4))
                                           +++
                                             (prec
                                                10
                                                (do expectP (Ident "BasicLiteral")
                                                    a1 <- step readPrec
                                                    a2 <- step readPrec
                                                    a3 <- step readPrec
                                                    return (BasicLiteral a1 a2 a3))
                                                +++
                                                  (prec
                                                     10
                                                     (do expectP (Ident "ArrayLiteral")
                                                         a1 <- step readPrec
                                                         a2 <- step readPrec
                                                         a3 <- step readPrec
                                                         return (ArrayLiteral a1 a2 a3))
                                                     +++
                                                       prec
                                                         10
                                                         (do expectP (Ident "RangedArray")
                                                             a1 <- step readPrec
                                                             a2 <- step readPrec
                                                             a3 <- step readPrec
                                                             a4 <- step readPrec
                                                             return
                                                               (RangedArray a1 a2 a3 a4)))))))))))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Functor Expr where
    fmap f (UnaryOp a1 a2 a3 a4) = UnaryOp a1 a2 (fmap f a3) (f a4)
    fmap f (BinaryOp a1 a2 a3 a4 a5)
      = BinaryOp a1 a2 (fmap f a3) (fmap f a4) (f a5)
    fmap f (Ref a1 a2 a3) = Ref a1 (fmap f a2) (f a3)
    fmap f (Deref a1 a2 a3) = Deref a1 (fmap f a2) (f a3)
    fmap f (ArrayAcc a1 a2 a3 a4)
      = ArrayAcc a1 (fmap f a2) (fmap f a3) (f a4)
    fmap f (Id a1 a2 a3) = Id a1 a2 (f a3)
    fmap f (FunctionCall a1 a2 a3 a4)
      = FunctionCall a1 a2 (fmap (\ b1 -> fmap f b1) a3) (f a4)
    fmap f (BasicLiteral a1 a2 a3) = BasicLiteral a1 (fmap f a2) (f a3)
    fmap f (ArrayLiteral a1 a2 a3)
      = ArrayLiteral a1 (fmap (\ b1 -> fmap f b1) a2) (f a3)
    fmap f (RangedArray a1 a2 a3 a4)
      = RangedArray a1 (fmap f a2) (fmap f a3) (f a4)
    (<$) z (UnaryOp a1 a2 a3 a4) = UnaryOp a1 a2 ((<$) z a3) z
    (<$) z (BinaryOp a1 a2 a3 a4 a5)
      = BinaryOp a1 a2 ((<$) z a3) ((<$) z a4) z
    (<$) z (Ref a1 a2 a3) = Ref a1 ((<$) z a2) z
    (<$) z (Deref a1 a2 a3) = Deref a1 ((<$) z a2) z
    (<$) z (ArrayAcc a1 a2 a3 a4)
      = ArrayAcc a1 ((<$) z a2) ((<$) z a3) z
    (<$) z (Id a1 a2 a3) = Id a1 a2 z
    (<$) z (FunctionCall a1 a2 a3 a4)
      = FunctionCall a1 a2 (fmap (\ b1 -> (<$) z b1) a3) z
    (<$) z (BasicLiteral a1 a2 a3) = BasicLiteral a1 ((<$) z a2) z
    (<$) z (ArrayLiteral a1 a2 a3)
      = ArrayLiteral a1 (fmap (\ b1 -> (<$) z b1) a2) z
    (<$) z (RangedArray a1 a2 a3 a4)
      = RangedArray a1 ((<$) z a2) ((<$) z a3) z
  
  instance Foldable Expr where
    foldr f z (UnaryOp a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1) a3 (f a4 z)
    foldr f z (BinaryOp a1 a2 a3 a4 a5)
      = (\ b1 b2 -> foldr f b2 b1)
          a3 ((\ b3 b4 -> foldr f b4 b3) a4 (f a5 z))
    foldr f z (Ref a1 a2 a3) = (\ b1 b2 -> foldr f b2 b1) a2 (f a3 z)
    foldr f z (Deref a1 a2 a3) = (\ b1 b2 -> foldr f b2 b1) a2 (f a3 z)
    foldr f z (ArrayAcc a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1)
          a2 ((\ b3 b4 -> foldr f b4 b3) a3 (f a4 z))
    foldr f z (Id a1 a2 a3) = f a3 z
    foldr f z (FunctionCall a1 a2 a3 a4)
      = (\ b3 b4 -> foldr (\ b1 b2 -> foldr f b2 b1) b4 b3) a3 (f a4 z)
    foldr f z (BasicLiteral a1 a2 a3)
      = (\ b1 b2 -> foldr f b2 b1) a2 (f a3 z)
    foldr f z (ArrayLiteral a1 a2 a3)
      = (\ b3 b4 -> foldr (\ b1 b2 -> foldr f b2 b1) b4 b3) a2 (f a3 z)
    foldr f z (RangedArray a1 a2 a3 a4)
      = (\ b1 b2 -> foldr f b2 b1)
          a2 ((\ b3 b4 -> foldr f b4 b3) a3 (f a4 z))
    foldMap f (UnaryOp a1 a2 a3 a4) = mappend (foldMap f a3) (f a4)
    foldMap f (BinaryOp a1 a2 a3 a4 a5)
      = mappend (foldMap f a3) (mappend (foldMap f a4) (f a5))
    foldMap f (Ref a1 a2 a3) = mappend (foldMap f a2) (f a3)
    foldMap f (Deref a1 a2 a3) = mappend (foldMap f a2) (f a3)
    foldMap f (ArrayAcc a1 a2 a3 a4)
      = mappend (foldMap f a2) (mappend (foldMap f a3) (f a4))
    foldMap f (Id a1 a2 a3) = f a3
    foldMap f (FunctionCall a1 a2 a3 a4)
      = mappend (foldMap (foldMap f) a3) (f a4)
    foldMap f (BasicLiteral a1 a2 a3) = mappend (foldMap f a2) (f a3)
    foldMap f (ArrayLiteral a1 a2 a3)
      = mappend (foldMap (foldMap f) a2) (f a3)
    foldMap f (RangedArray a1 a2 a3 a4)
      = mappend (foldMap f a2) (mappend (foldMap f a3) (f a4))
    null (UnaryOp _ _ _ _) = False
    null (BinaryOp _ _ _ _ _) = False
    null (Ref _ _ _) = False
    null (Deref _ _ _) = False
    null (ArrayAcc _ _ _ _) = False
    null (Id _ _ _) = False
    null (FunctionCall _ _ _ _) = False
    null (BasicLiteral _ _ _) = False
    null (ArrayLiteral _ _ _) = False
    null (RangedArray _ _ _ _) = False
  
  instance Traversable Expr where
    traverse f (UnaryOp a1 a2 a3 a4)
      = liftA2 (\ b3 b4 -> UnaryOp a1 a2 b3 b4) (traverse f a3) (f a4)
    traverse f (BinaryOp a1 a2 a3 a4 a5)
      = (<*>)
          (liftA2
             (\ b3 b4 b5 -> BinaryOp a1 a2 b3 b4 b5) (traverse f a3)
             (traverse f a4))
          (f a5)
    traverse f (Ref a1 a2 a3)
      = liftA2 (\ b2 b3 -> Ref a1 b2 b3) (traverse f a2) (f a3)
    traverse f (Deref a1 a2 a3)
      = liftA2 (\ b2 b3 -> Deref a1 b2 b3) (traverse f a2) (f a3)
    traverse f (ArrayAcc a1 a2 a3 a4)
      = (<*>)
          (liftA2
             (\ b2 b3 b4 -> ArrayAcc a1 b2 b3 b4) (traverse f a2)
             (traverse f a3))
          (f a4)
    traverse f (Id a1 a2 a3) = fmap (\ b3 -> Id a1 a2 b3) (f a3)
    traverse f (FunctionCall a1 a2 a3 a4)
      = liftA2
          (\ b3 b4 -> FunctionCall a1 a2 b3 b4) (traverse (traverse f) a3)
          (f a4)
    traverse f (BasicLiteral a1 a2 a3)
      = liftA2 (\ b2 b3 -> BasicLiteral a1 b2 b3) (traverse f a2) (f a3)
    traverse f (ArrayLiteral a1 a2 a3)
      = liftA2
          (\ b2 b3 -> ArrayLiteral a1 b2 b3) (traverse (traverse f) a2)
          (f a3)
    traverse f (RangedArray a1 a2 a3 a4)
      = (<*>)
          (liftA2
             (\ b2 b3 b4 -> RangedArray a1 b2 b3 b4) (traverse f a2)
             (traverse f a3))
          (f a4)
  
  instance Show UnaryOp where
    showsPrec _ Not = showString "Not"
    showsPrec _ Neg = showString "Neg"
    showsPrec _ Coertion = showString "Coertion"
    showsPrec _ PreDecr = showString "PreDecr"
    showsPrec _ PreIncr = showString "PreIncr"
    showsPrec _ PostDecr = showString "PostDecr"
    showsPrec _ PostIncr = showString "PostIncr"
  
  instance Eq UnaryOp where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord UnaryOp where
    compare a b
      = case (dataToTag# a) of
          a#
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (a# <# b#)) :: Bool then
                          LT
                      else
                          if (tagToEnum# (a# ==# b#)) :: Bool then EQ else GT
    (<) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# <# b#))
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Read UnaryOp where
    readPrec
      = parens
          (choose
             [("Not", return Not), ("Neg", return Neg),
              ("Coertion", return Coertion), ("PreDecr", return PreDecr),
              ("PreIncr", return PreIncr), ("PostDecr", return PostDecr),
              ("PostIncr", return PostIncr)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Show BinaryOp where
    showsPrec a (ArithmeticOp b1)
      = showParen
          (a >= 11) ((.) (showString "ArithmeticOp ") (showsPrec 11 b1))
    showsPrec a (RelationalOp b1)
      = showParen
          (a >= 11) ((.) (showString "RelationalOp ") (showsPrec 11 b1))
    showsPrec a (BooleanOp b1)
      = showParen
          (a >= 11) ((.) (showString "BooleanOp ") (showsPrec 11 b1))
  
  instance Eq BinaryOp where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (ArithmeticOp a1) -> case b of (ArithmeticOp b1) -> ((a1 == b1))
                 (RelationalOp a1) -> case b of (RelationalOp b1) -> ((a1 == b1))
                 (BooleanOp a1) -> case b of (BooleanOp b1) -> ((a1 == b1))
                 _ -> True
  
  instance Ord BinaryOp where
    compare a b
      = case a of
          ArithmeticOp a1
            -> case b of
                 ArithmeticOp b1 -> (a1 `compare` b1)
                 _ -> LT
          RelationalOp a1
            -> case b of
                 ArithmeticOp {} -> GT
                 RelationalOp b1 -> (a1 `compare` b1)
                 _ -> LT
          BooleanOp a1
            -> case b of
                 BooleanOp b1 -> (a1 `compare` b1)
                 _ -> GT
    (<) a b
      = case a of
          ArithmeticOp a1
            -> case b of
                 ArithmeticOp b1 -> (a1 < b1)
                 _ -> True
          RelationalOp a1
            -> case b of
                 ArithmeticOp {} -> False
                 RelationalOp b1 -> (a1 < b1)
                 _ -> True
          BooleanOp a1
            -> case b of
                 BooleanOp b1 -> (a1 < b1)
                 _ -> False
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Read BinaryOp where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "ArithmeticOp")
                 a1 <- step readPrec
                 return (ArithmeticOp a1))
             +++
               (prec
                  10
                  (do expectP (Ident "RelationalOp")
                      a1 <- step readPrec
                      return (RelationalOp a1))
                  +++
                    prec
                      10
                      (do expectP (Ident "BooleanOp")
                          a1 <- step readPrec
                          return (BooleanOp a1))))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Eq a => Eq (BasicLiteral a) where
    (==) a b
      = case
            (case (dataToTag# a) of
               a# -> case (dataToTag# b) of b# -> a# /=# b#)
        of
          1# -> False
          _ -> case a of
                 (IntLiteral a1 a2)
                   -> case b of (IntLiteral b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (CharLiteral a1 a2)
                   -> case b of (CharLiteral b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (StringLiteral a1 a2)
                   -> case b of
                        (StringLiteral b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (FloatLiteral a1 a2)
                   -> case b of (FloatLiteral b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 (BoolLiteral a1 a2)
                   -> case b of (BoolLiteral b1 b2) -> (((a1 == b1)) && ((a2 == b2)))
                 _ -> True
  
  instance Ord a => Ord (BasicLiteral a) where
    compare a b
      = case a of
          IntLiteral a1 a2
            -> case b of
                 IntLiteral b1 b2
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ -> (a2 `compare` b2)
                        GT -> GT
                 _ -> LT
          CharLiteral a1 a2
            -> case b of
                 IntLiteral {} -> GT
                 CharLiteral b1 b2
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ -> (a2 `compare` b2)
                        GT -> GT
                 _ -> LT
          StringLiteral a1 a2
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (b# ># 2#)) then
                          LT
                      else
                          case b of
                            StringLiteral b1 b2
                              -> case (compare a1 b1) of
                                   LT -> LT
                                   EQ -> (a2 `compare` b2)
                                   GT -> GT
                            _ -> GT
          FloatLiteral a1 a2
            -> case b of
                 BoolLiteral {} -> LT
                 FloatLiteral b1 b2
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ -> (a2 `compare` b2)
                        GT -> GT
                 _ -> GT
          BoolLiteral a1 a2
            -> case b of
                 BoolLiteral b1 b2
                   -> case (compare a1 b1) of
                        LT -> LT
                        EQ -> (a2 `compare` b2)
                        GT -> GT
                 _ -> GT
  
  instance Show a => Show (BasicLiteral a) where
    showsPrec a (IntLiteral b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "IntLiteral ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (CharLiteral b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "CharLiteral ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (StringLiteral b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "StringLiteral ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (FloatLiteral b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "FloatLiteral ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
    showsPrec a (BoolLiteral b1 b2)
      = showParen
          (a >= 11)
          ((.)
             (showString "BoolLiteral ")
             ((.) (showsPrec 11 b1) ((.) showSpace (showsPrec 11 b2))))
  
  instance Read a => Read (BasicLiteral a) where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "IntLiteral")
                 a1 <- step readPrec
                 a2 <- step readPrec
                 return (IntLiteral a1 a2))
             +++
               (prec
                  10
                  (do expectP (Ident "CharLiteral")
                      a1 <- step readPrec
                      a2 <- step readPrec
                      return (CharLiteral a1 a2))
                  +++
                    (prec
                       10
                       (do expectP (Ident "StringLiteral")
                           a1 <- step readPrec
                           a2 <- step readPrec
                           return (StringLiteral a1 a2))
                       +++
                         (prec
                            10
                            (do expectP (Ident "FloatLiteral")
                                a1 <- step readPrec
                                a2 <- step readPrec
                                return (FloatLiteral a1 a2))
                            +++
                              prec
                                10
                                (do expectP (Ident "BoolLiteral")
                                    a1 <- step readPrec
                                    a2 <- step readPrec
                                    return (BoolLiteral a1 a2))))))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Functor BasicLiteral where
    fmap f (IntLiteral a1 a2) = IntLiteral a1 (f a2)
    fmap f (CharLiteral a1 a2) = CharLiteral a1 (f a2)
    fmap f (StringLiteral a1 a2) = StringLiteral a1 (f a2)
    fmap f (FloatLiteral a1 a2) = FloatLiteral a1 (f a2)
    fmap f (BoolLiteral a1 a2) = BoolLiteral a1 (f a2)
    (<$) z (IntLiteral a1 a2) = IntLiteral a1 z
    (<$) z (CharLiteral a1 a2) = CharLiteral a1 z
    (<$) z (StringLiteral a1 a2) = StringLiteral a1 z
    (<$) z (FloatLiteral a1 a2) = FloatLiteral a1 z
    (<$) z (BoolLiteral a1 a2) = BoolLiteral a1 z
  
  instance Foldable BasicLiteral where
    foldr f z (IntLiteral a1 a2) = f a2 z
    foldr f z (CharLiteral a1 a2) = f a2 z
    foldr f z (StringLiteral a1 a2) = f a2 z
    foldr f z (FloatLiteral a1 a2) = f a2 z
    foldr f z (BoolLiteral a1 a2) = f a2 z
    foldMap f (IntLiteral a1 a2) = f a2
    foldMap f (CharLiteral a1 a2) = f a2
    foldMap f (StringLiteral a1 a2) = f a2
    foldMap f (FloatLiteral a1 a2) = f a2
    foldMap f (BoolLiteral a1 a2) = f a2
    null (IntLiteral _ _) = False
    null (CharLiteral _ _) = False
    null (StringLiteral _ _) = False
    null (FloatLiteral _ _) = False
    null (BoolLiteral _ _) = False
  
  instance Traversable BasicLiteral where
    traverse f (IntLiteral a1 a2)
      = fmap (\ b2 -> IntLiteral a1 b2) (f a2)
    traverse f (CharLiteral a1 a2)
      = fmap (\ b2 -> CharLiteral a1 b2) (f a2)
    traverse f (StringLiteral a1 a2)
      = fmap (\ b2 -> StringLiteral a1 b2) (f a2)
    traverse f (FloatLiteral a1 a2)
      = fmap (\ b2 -> FloatLiteral a1 b2) (f a2)
    traverse f (BoolLiteral a1 a2)
      = fmap (\ b2 -> BoolLiteral a1 b2) (f a2)
  
  instance Show RelOp where
    showsPrec _ NotEq = showString "NotEq"
    showsPrec _ GreaterThanEq = showString "GreaterThanEq"
    showsPrec _ GreaterThan = showString "GreaterThan"
    showsPrec _ LessThanEq = showString "LessThanEq"
    showsPrec _ LessThan = showString "LessThan"
    showsPrec _ Eq = showString "Eq"
  
  instance Eq RelOp where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord RelOp where
    compare a b
      = case (dataToTag# a) of
          a#
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (a# <# b#)) :: Bool then
                          LT
                      else
                          if (tagToEnum# (a# ==# b#)) :: Bool then EQ else GT
    (<) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# <# b#))
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Read RelOp where
    readPrec
      = parens
          (choose
             [("NotEq", return NotEq), ("GreaterThanEq", return GreaterThanEq),
              ("GreaterThan", return GreaterThan),
              ("LessThanEq", return LessThanEq), ("LessThan", return LessThan),
              ("Eq", return Eq)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Show BoolOp where
    showsPrec _ Or = showString "Or"
    showsPrec _ And = showString "And"
  
  instance Eq BoolOp where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord BoolOp where
    compare a b
      = case a of
          Or
            -> case b of
                 Or -> EQ
                 _ -> LT
          And
            -> case b of
                 And -> EQ
                 _ -> GT
    (<) a b
      = case a of
          Or
            -> case b of
                 Or -> False
                 _ -> True
          And
            -> case b of
                 And -> False
                 _ -> False
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Read BoolOp where
    readPrec = parens (choose [("Or", return Or), ("And", return And)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Show ArithOp where
    showsPrec _ Add = showString "Add"
    showsPrec _ Sub = showString "Sub"
    showsPrec _ Mul = showString "Mul"
    showsPrec _ Mod = showString "Mod"
    showsPrec _ Pow = showString "Pow"
    showsPrec _ Div = showString "Div"
  
  instance Eq ArithOp where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord ArithOp where
    compare a b
      = case (dataToTag# a) of
          a#
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (a# <# b#)) :: Bool then
                          LT
                      else
                          if (tagToEnum# (a# ==# b#)) :: Bool then EQ else GT
    (<) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# <# b#))
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Read ArithOp where
    readPrec
      = parens
          (choose
             [("Add", return Add), ("Sub", return Sub), ("Mul", return Mul),
              ("Mod", return Mod), ("Pow", return Pow), ("Div", return Div)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Eq AssignmentOp where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord AssignmentOp where
    compare a b
      = case (dataToTag# a) of
          a#
            -> case (dataToTag# b) of
                 b#
                   -> if (tagToEnum# (a# <# b#)) :: Bool then
                          LT
                      else
                          if (tagToEnum# (a# ==# b#)) :: Bool then EQ else GT
    (<) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# <# b#))
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Show AssignmentOp where
    showsPrec _ BasicAssignment = showString "BasicAssignment"
    showsPrec _ AssignMul = showString "AssignMul"
    showsPrec _ AssignAdd = showString "AssignAdd"
    showsPrec _ AssignDiv = showString "AssignDiv"
    showsPrec _ AssignSub = showString "AssignSub"
    showsPrec _ AssignPow = showString "AssignPow"
    showsPrec _ AssignAnd = showString "AssignAnd"
    showsPrec _ AssignOr = showString "AssignOr"
  
  instance Read AssignmentOp where
    readPrec
      = parens
          (choose
             [("BasicAssignment", return BasicAssignment),
              ("AssignMul", return AssignMul), ("AssignAdd", return AssignAdd),
              ("AssignDiv", return AssignDiv), ("AssignSub", return AssignSub),
              ("AssignPow", return AssignPow), ("AssignAnd", return AssignAnd),
              ("AssignOr", return AssignOr)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance Eq Ident where
    (==) :: Ident -> Ident -> Bool
    (/=) :: Ident -> Ident -> Bool
    (==)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool)
          ((==) @String)
    (/=)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool)
          ((/=) @String)
  
  instance Ord Ident where
    compare :: Ident -> Ident -> Ordering
    (<) :: Ident -> Ident -> Bool
    (<=) :: Ident -> Ident -> Bool
    (>) :: Ident -> Ident -> Bool
    (>=) :: Ident -> Ident -> Bool
    max :: Ident -> Ident -> Ident
    min :: Ident -> Ident -> Ident
    compare
      = coerce
          @(String -> String -> Ordering) @(Ident -> Ident -> Ordering)
          (compare @String)
    (<)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool) ((<) @String)
    (<=)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool)
          ((<=) @String)
    (>)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool) ((>) @String)
    (>=)
      = coerce
          @(String -> String -> Bool) @(Ident -> Ident -> Bool)
          ((>=) @String)
    max
      = coerce
          @(String -> String -> String) @(Ident -> Ident -> Ident)
          (max @String)
    min
      = coerce
          @(String -> String -> String) @(Ident -> Ident -> Ident)
          (min @String)
  
  instance Show Ident where
    showsPrec a (Ident b1)
      = showParen (a >= 11) ((.) (showString "Ident ") (showsPrec 11 b1))
  
  instance Read Ident where
    readPrec
      = parens
          (prec
             10
             (do expectP (Ident "Ident")
                 a1 <- step readPrec
                 return (Ident a1)))
    readList = readListDefault
    readListPrec = readListPrecDefault
  
  instance IsString Ident where
    fromString :: String -> Ident
    fromString
      = coerce
          @(String -> String) @(String -> Ident) (fromString @String)
  
  instance Eq Modality where
    (==) a b
      = case (dataToTag# a) of
          a# -> case (dataToTag# b) of b# -> (tagToEnum# (a# ==# b#))
  
  instance Ord Modality where
    compare a b
      = case a of
          ModalityVal
            -> case b of
                 ModalityVal -> EQ
                 _ -> LT
          ModalityRef
            -> case b of
                 ModalityRef -> EQ
                 _ -> GT
    (<) a b
      = case a of
          ModalityVal
            -> case b of
                 ModalityVal -> False
                 _ -> True
          ModalityRef
            -> case b of
                 ModalityRef -> False
                 _ -> False
    (<=) a b = not ((<) b a)
    (>) a b = (<) b a
    (>=) a b = not ((<) a b)
  
  instance Show Modality where
    showsPrec _ ModalityVal = showString "ModalityVal"
    showsPrec _ ModalityRef = showString "ModalityRef"
  
  instance Read Modality where
    readPrec
      = parens
          (choose
             [("ModalityVal", return ModalityVal),
              ("ModalityRef", return ModalityRef)])
    readList = readListDefault
    readListPrec = readListPrecDefault
  

Derived type family instances:



==================== Filling in method body ====================
Eq [ASTData]
  /= = $dm/= @(ASTData)



==================== Filling in method body ====================
Ord [ASTData]
  max = $dmmax @(ASTData)



==================== Filling in method body ====================
Ord [ASTData]
  min = $dmmin @(ASTData)



==================== Filling in method body ====================
Show [ASTData]
  show = $dmshow @(ASTData)



==================== Filling in method body ====================
Show [ASTData]
  showList = $dmshowList @(ASTData)



==================== Filling in method body ====================
Read [ASTData]
  readsPrec = $dmreadsPrec @(ASTData)



==================== Filling in method body ====================
Eq [Instruction a]
  /= = $dm/= @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  < = $dm< @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  <= = $dm<= @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  > = $dm> @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  >= = $dm>= @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  max = $dmmax @(Instruction a)



==================== Filling in method body ====================
Ord [Instruction a]
  min = $dmmin @(Instruction a)



==================== Filling in method body ====================
Show [Instruction a]
  show = $dmshow @(Instruction a)



==================== Filling in method body ====================
Show [Instruction a]
  showList = $dmshowList @(Instruction a)



==================== Filling in method body ====================
Read [Instruction a]
  readsPrec = $dmreadsPrec @(Instruction a)



==================== Filling in method body ====================
Foldable [Instruction]
  fold = $dmfold @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldMap' = $dmfoldMap' @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldr' = $dmfoldr' @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldl = $dmfoldl @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldl' = $dmfoldl' @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldr1 = $dmfoldr1 @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  foldl1 = $dmfoldl1 @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  toList = $dmtoList @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  length = $dmlength @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  elem = $dmelem @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  maximum = $dmmaximum @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  minimum = $dmminimum @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  sum = $dmsum @(Instruction)



==================== Filling in method body ====================
Foldable [Instruction]
  product = $dmproduct @(Instruction)



==================== Filling in method body ====================
Traversable [Instruction]
  sequenceA = $dmsequenceA @(Instruction)



==================== Filling in method body ====================
Traversable [Instruction]
  mapM = $dmmapM @(Instruction)



==================== Filling in method body ====================
Traversable [Instruction]
  sequence = $dmsequence @(Instruction)



==================== Filling in method body ====================
Eq [Parameter a]
  /= = $dm/= @(Parameter a)



==================== Filling in method body ====================
Ord [Parameter a]
  max = $dmmax @(Parameter a)



==================== Filling in method body ====================
Ord [Parameter a]
  min = $dmmin @(Parameter a)



==================== Filling in method body ====================
Show [Parameter a]
  show = $dmshow @(Parameter a)



==================== Filling in method body ====================
Show [Parameter a]
  showList = $dmshowList @(Parameter a)



==================== Filling in method body ====================
Read [Parameter a]
  readsPrec = $dmreadsPrec @(Parameter a)



==================== Filling in method body ====================
Foldable [Parameter]
  fold = $dmfold @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldMap' = $dmfoldMap' @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldr' = $dmfoldr' @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldl = $dmfoldl @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldl' = $dmfoldl' @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldr1 = $dmfoldr1 @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  foldl1 = $dmfoldl1 @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  toList = $dmtoList @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  length = $dmlength @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  elem = $dmelem @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  maximum = $dmmaximum @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  minimum = $dmminimum @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  sum = $dmsum @(Parameter)



==================== Filling in method body ====================
Foldable [Parameter]
  product = $dmproduct @(Parameter)



==================== Filling in method body ====================
Traversable [Parameter]
  sequenceA = $dmsequenceA @(Parameter)



==================== Filling in method body ====================
Traversable [Parameter]
  mapM = $dmmapM @(Parameter)



==================== Filling in method body ====================
Traversable [Parameter]
  sequence = $dmsequence @(Parameter)



==================== Filling in method body ====================
Eq [DeclType a]
  /= = $dm/= @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  < = $dm< @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  <= = $dm<= @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  > = $dm> @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  >= = $dm>= @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  max = $dmmax @(DeclType a)



==================== Filling in method body ====================
Ord [DeclType a]
  min = $dmmin @(DeclType a)



==================== Filling in method body ====================
Show [DeclType a]
  show = $dmshow @(DeclType a)



==================== Filling in method body ====================
Show [DeclType a]
  showList = $dmshowList @(DeclType a)



==================== Filling in method body ====================
Read [DeclType a]
  readsPrec = $dmreadsPrec @(DeclType a)



==================== Filling in method body ====================
Foldable [DeclType]
  fold = $dmfold @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldMap' = $dmfoldMap' @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldr' = $dmfoldr' @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldl = $dmfoldl @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldl' = $dmfoldl' @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldr1 = $dmfoldr1 @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  foldl1 = $dmfoldl1 @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  toList = $dmtoList @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  length = $dmlength @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  elem = $dmelem @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  maximum = $dmmaximum @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  minimum = $dmminimum @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  sum = $dmsum @(DeclType)



==================== Filling in method body ====================
Foldable [DeclType]
  product = $dmproduct @(DeclType)



==================== Filling in method body ====================
Traversable [DeclType]
  sequenceA = $dmsequenceA @(DeclType)



==================== Filling in method body ====================
Traversable [DeclType]
  mapM = $dmmapM @(DeclType)



==================== Filling in method body ====================
Traversable [DeclType]
  sequence = $dmsequence @(DeclType)



==================== Filling in method body ====================
Eq [Expr a]
  /= = $dm/= @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  < = $dm< @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  <= = $dm<= @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  > = $dm> @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  >= = $dm>= @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  max = $dmmax @(Expr a)



==================== Filling in method body ====================
Ord [Expr a]
  min = $dmmin @(Expr a)



==================== Filling in method body ====================
Show [Expr a]
  show = $dmshow @(Expr a)



==================== Filling in method body ====================
Show [Expr a]
  showList = $dmshowList @(Expr a)



==================== Filling in method body ====================
Read [Expr a]
  readsPrec = $dmreadsPrec @(Expr a)



==================== Filling in method body ====================
Foldable [Expr]
  fold = $dmfold @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldMap' = $dmfoldMap' @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldr' = $dmfoldr' @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldl = $dmfoldl @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldl' = $dmfoldl' @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldr1 = $dmfoldr1 @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  foldl1 = $dmfoldl1 @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  toList = $dmtoList @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  length = $dmlength @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  elem = $dmelem @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  maximum = $dmmaximum @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  minimum = $dmminimum @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  sum = $dmsum @(Expr)



==================== Filling in method body ====================
Foldable [Expr]
  product = $dmproduct @(Expr)



==================== Filling in method body ====================
Traversable [Expr]
  sequenceA = $dmsequenceA @(Expr)



==================== Filling in method body ====================
Traversable [Expr]
  mapM = $dmmapM @(Expr)



==================== Filling in method body ====================
Traversable [Expr]
  sequence = $dmsequence @(Expr)



==================== Filling in method body ====================
Show [UnaryOp]
  show = $dmshow @(UnaryOp)



==================== Filling in method body ====================
Show [UnaryOp]
  showList = $dmshowList @(UnaryOp)



==================== Filling in method body ====================
Eq [UnaryOp]
  /= = $dm/= @(UnaryOp)



==================== Filling in method body ====================
Ord [UnaryOp]
  max = $dmmax @(UnaryOp)



==================== Filling in method body ====================
Ord [UnaryOp]
  min = $dmmin @(UnaryOp)



==================== Filling in method body ====================
Read [UnaryOp]
  readsPrec = $dmreadsPrec @(UnaryOp)



==================== Filling in method body ====================
Show [BinaryOp]
  show = $dmshow @(BinaryOp)



==================== Filling in method body ====================
Show [BinaryOp]
  showList = $dmshowList @(BinaryOp)



==================== Filling in method body ====================
Eq [BinaryOp]
  /= = $dm/= @(BinaryOp)



==================== Filling in method body ====================
Ord [BinaryOp]
  max = $dmmax @(BinaryOp)



==================== Filling in method body ====================
Ord [BinaryOp]
  min = $dmmin @(BinaryOp)



==================== Filling in method body ====================
Read [BinaryOp]
  readsPrec = $dmreadsPrec @(BinaryOp)



==================== Filling in method body ====================
Eq [BasicLiteral a]
  /= = $dm/= @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  < = $dm< @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  <= = $dm<= @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  > = $dm> @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  >= = $dm>= @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  max = $dmmax @(BasicLiteral a)



==================== Filling in method body ====================
Ord [BasicLiteral a]
  min = $dmmin @(BasicLiteral a)



==================== Filling in method body ====================
Show [BasicLiteral a]
  show = $dmshow @(BasicLiteral a)



==================== Filling in method body ====================
Show [BasicLiteral a]
  showList = $dmshowList @(BasicLiteral a)



==================== Filling in method body ====================
Read [BasicLiteral a]
  readsPrec = $dmreadsPrec @(BasicLiteral a)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  fold = $dmfold @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldMap' = $dmfoldMap' @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldr' = $dmfoldr' @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldl = $dmfoldl @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldl' = $dmfoldl' @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldr1 = $dmfoldr1 @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  foldl1 = $dmfoldl1 @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  toList = $dmtoList @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  length = $dmlength @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  elem = $dmelem @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  maximum = $dmmaximum @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  minimum = $dmminimum @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  sum = $dmsum @(BasicLiteral)



==================== Filling in method body ====================
Foldable [BasicLiteral]
  product = $dmproduct @(BasicLiteral)



==================== Filling in method body ====================
Traversable [BasicLiteral]
  sequenceA = $dmsequenceA @(BasicLiteral)



==================== Filling in method body ====================
Traversable [BasicLiteral]
  mapM = $dmmapM @(BasicLiteral)



==================== Filling in method body ====================
Traversable [BasicLiteral]
  sequence = $dmsequence @(BasicLiteral)



==================== Filling in method body ====================
Show [RelOp]
  show = $dmshow @(RelOp)



==================== Filling in method body ====================
Show [RelOp]
  showList = $dmshowList @(RelOp)



==================== Filling in method body ====================
Eq [RelOp]
  /= = $dm/= @(RelOp)



==================== Filling in method body ====================
Ord [RelOp]
  max = $dmmax @(RelOp)



==================== Filling in method body ====================
Ord [RelOp]
  min = $dmmin @(RelOp)



==================== Filling in method body ====================
Read [RelOp]
  readsPrec = $dmreadsPrec @(RelOp)



==================== Filling in method body ====================
Show [BoolOp]
  show = $dmshow @(BoolOp)



==================== Filling in method body ====================
Show [BoolOp]
  showList = $dmshowList @(BoolOp)



==================== Filling in method body ====================
Eq [BoolOp]
  /= = $dm/= @(BoolOp)



==================== Filling in method body ====================
Ord [BoolOp]
  max = $dmmax @(BoolOp)



==================== Filling in method body ====================
Ord [BoolOp]
  min = $dmmin @(BoolOp)



==================== Filling in method body ====================
Read [BoolOp]
  readsPrec = $dmreadsPrec @(BoolOp)



==================== Filling in method body ====================
Show [ArithOp]
  show = $dmshow @(ArithOp)



==================== Filling in method body ====================
Show [ArithOp]
  showList = $dmshowList @(ArithOp)



==================== Filling in method body ====================
Eq [ArithOp]
  /= = $dm/= @(ArithOp)



==================== Filling in method body ====================
Ord [ArithOp]
  max = $dmmax @(ArithOp)



==================== Filling in method body ====================
Ord [ArithOp]
  min = $dmmin @(ArithOp)



==================== Filling in method body ====================
Read [ArithOp]
  readsPrec = $dmreadsPrec @(ArithOp)



==================== Filling in method body ====================
Eq [AssignmentOp]
  /= = $dm/= @(AssignmentOp)



==================== Filling in method body ====================
Ord [AssignmentOp]
  max = $dmmax @(AssignmentOp)



==================== Filling in method body ====================
Ord [AssignmentOp]
  min = $dmmin @(AssignmentOp)



==================== Filling in method body ====================
Show [AssignmentOp]
  show = $dmshow @(AssignmentOp)



==================== Filling in method body ====================
Show [AssignmentOp]
  showList = $dmshowList @(AssignmentOp)



==================== Filling in method body ====================
Read [AssignmentOp]
  readsPrec = $dmreadsPrec @(AssignmentOp)



==================== Filling in method body ====================
Show [Ident]
  show = $dmshow @(Ident)



==================== Filling in method body ====================
Show [Ident]
  showList = $dmshowList @(Ident)



==================== Filling in method body ====================
Read [Ident]
  readsPrec = $dmreadsPrec @(Ident)



==================== Filling in method body ====================
Eq [Modality]
  /= = $dm/= @(Modality)



==================== Filling in method body ====================
Ord [Modality]
  max = $dmmax @(Modality)



==================== Filling in method body ====================
Ord [Modality]
  min = $dmmin @(Modality)



==================== Filling in method body ====================
Show [Modality]
  show = $dmshow @(Modality)



==================== Filling in method body ====================
Show [Modality]
  showList = $dmshowList @(Modality)



==================== Filling in method body ====================
Read [Modality]
  readsPrec = $dmreadsPrec @(Modality)


