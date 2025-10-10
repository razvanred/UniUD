module TypeChecker.Intrinsics where

import AST
import Data.Set
import TypeChecker.TypeUtils

intrinsicsDecls =
    [ FunctionDecl
        ( 1,
          1
        )
        "writeInt"
        [ Param
            ( 1,
              14
            )
            ModalityVal
            "x"
            DIntType
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = IntType,
                  sSide = LeftValue,
                  sBinding = Nothing
                }
            )
        ]
        DVoidType
        [ ReturnVoid
            ( 1,
              1
            )
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = VoidType,
                  sSide = RightValue,
                  sBinding = Nothing
                }
            )
        ]
        ( Step3
            { serrors = fromList [],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType =
                FunctionType
                    [   ( ModalityVal,
                          IntType
                        )
                    ]
                    VoidType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          32
        )
        "writeFloat"
        [ Param
            ( 1,
              47
            )
            ModalityVal
            "x"
            DFloatType
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = FloatType,
                  sSide = LeftValue,
                  sBinding = Nothing
                }
            )
        ]
        DVoidType
        [ ReturnVoid
            ( 1,
              32
            )
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = VoidType,
                  sSide = RightValue,
                  sBinding = Nothing
                }
            )
        ]
        ( Step3
            { serrors = fromList [],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType =
                FunctionType
                    [   ( ModalityVal,
                          FloatType
                        )
                    ]
                    VoidType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          67
        )
        "writeChar"
        [ Param
            ( 1,
              81
            )
            ModalityVal
            "x"
            DCharType
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = CharType,
                  sSide = LeftValue,
                  sBinding = Nothing
                }
            )
        ]
        DVoidType
        [ ReturnVoid
            ( 1,
              67
            )
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = VoidType,
                  sSide = RightValue,
                  sBinding = Nothing
                }
            )
        ]
        ( Step3
            { serrors = fromList [],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType =
                FunctionType
                    [   ( ModalityVal,
                          CharType
                        )
                    ]
                    VoidType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          100
        )
        "writeString"
        [ Param
            ( 1,
              116
            )
            ModalityVal
            "x"
            DStringType
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = StringType,
                  sSide = LeftValue,
                  sBinding = Nothing
                }
            )
        ]
        DVoidType
        [ ReturnVoid
            ( 1,
              100
            )
            ( Step3
                { serrors = fromList [],
                  swarnings = fromList [],
                  sReplacedFromConstant = Nothing,
                  sType = VoidType,
                  sSide = RightValue,
                  sBinding = Nothing
                }
            )
        ]
        ( Step3
            { serrors = fromList [],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType =
                FunctionType
                    [   ( ModalityVal,
                          StringType
                        )
                    ]
                    VoidType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          137
        )
        "readInt"
        []
        DIntType
        []
        ( Step3
            { serrors = fromList [NonTotalFunction],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType = FunctionType [] IntType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          157
        )
        "readFloat"
        []
        DFloatType
        []
        ( Step3
            { serrors = fromList [NonTotalFunction],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType = FunctionType [] FloatType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          181
        )
        "readChar"
        []
        DCharType
        []
        ( Step3
            { serrors = fromList [NonTotalFunction],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType = FunctionType [] CharType,
              sSide = RightValue,
              sBinding = Nothing
            }
        ),
      FunctionDecl
        ( 1,
          203
        )
        "readString"
        []
        DStringType
        []
        ( Step3
            { serrors = fromList [NonTotalFunction],
              swarnings = fromList [],
              sReplacedFromConstant = Nothing,
              sType = FunctionType [] StringType,
              sSide = RightValue,
              sBinding = Nothing
            }
        )
    ]
