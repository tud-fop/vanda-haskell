module Vanda.CBSM.ProductOrderTests where
import Vanda.CBSM.ProductOrder


import Test.HUnit


tests :: Test
tests = TestList
  [ "PartialOrd.comparePO: Int^2" ~: allProducts 0 (int 1)

  , "PartialOrd.comparePO: Int^4" ~: TestList
    [ allProducts (0 :&: 0) (int 0 :&: int 1)
    , allProducts (0 :&: 0) (int 1 :&: int 0)
    , allProducts (0 :&: 0) (int 1 :&: int 1)
    ]

  , "Enums.succs/preds: Int^3" ~:
    let triple :: Int -> Int -> Int -> Product (Product Int Int) Int
        triple x y z = x :&: y :&: z
    in TestList
    [ TestCase $ assertBool "succs (0 :&: 0 :&: 0)"
      $ eqListsets (succs (triple 0 0 0))
                   [triple 0 0 1, triple 0 1 0, triple 1 0 0]
    , TestCase $ assertBool "preds (1 :&: 1 :&: 1)"
      $ eqListsets (preds (triple 1 1 1))
                   [triple 0 1 1, triple 1 0 1, triple 1 1 0]
    ]

  , "LEqSet: addLub" ~: TestList
    [ TestCase $ addLub (int 0) (LubSet [ ]) @?= LubSet [0]
    , TestCase $ addLub (int 1) (LubSet [0]) @?= LubSet [1]
    , TestCase $ addLub (int 0) (LubSet [1]) @?= LubSet [1]
    , TestCase $ addLub (int 0 :&: int 2) (LubSet [0 :&: 1, 1 :&: 0]) @?= LubSet [0 :&: 2, 1 :&: 0]
    , TestCase $ addLub (int 2 :&: int 0) (LubSet [0 :&: 1, 1 :&: 0]) @?= LubSet [0 :&: 1, 2 :&: 0]
    ]
  ]


int :: Int -> Int
int = id


-- | 'allProducts l h' shall succeed for every 'l <? h'.
allProducts :: PartialOrd b => b -> b -> Test
allProducts l h
  = TestList
    [ comparePO (l :&: l) (l :&: l) ~=? Just EQ
    , comparePO (l :&: l) (l :&: h) ~=? Just LT
    , comparePO (l :&: l) (h :&: l) ~=? Just LT
    , comparePO (l :&: l) (h :&: h) ~=? Just LT
    , comparePO (l :&: h) (l :&: l) ~=? Just GT
    , comparePO (l :&: h) (l :&: h) ~=? Just EQ
    , comparePO (l :&: h) (h :&: l) ~=? Nothing
    , comparePO (l :&: h) (h :&: h) ~=? Just LT
    , comparePO (h :&: l) (l :&: l) ~=? Just GT
    , comparePO (h :&: l) (l :&: h) ~=? Nothing
    , comparePO (h :&: l) (h :&: l) ~=? Just EQ
    , comparePO (h :&: l) (h :&: h) ~=? Just LT
    , comparePO (h :&: h) (l :&: l) ~=? Just GT
    , comparePO (h :&: h) (l :&: h) ~=? Just GT
    , comparePO (h :&: h) (h :&: l) ~=? Just GT
    , comparePO (h :&: h) (h :&: h) ~=? Just EQ
    ]
