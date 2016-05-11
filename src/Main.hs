module Main where

-- data Category = Category
--               | Arrow
--               | Edge

data Category = Category { label  :: String
                         , cats   :: [Category]
                         , rs     :: [Arrow] }
              | Default deriving (Show, Eq)
                                   
type Nodes = [Category]             
type Arrow = (Category, Category)
-- type Edge  = (Arrow, Arrow)

-- listNodes :: Category -> Nodes
-- listNodes (Category { label = l, cats  = cs, rs     = as}) = cs

listNodes :: Category -> Nodes
listNodes (Category l cs rs) = cs
  
isElem :: Category -> Category -> Bool
isElem cat (Category _ cs _) = True `elem` map (cat ==) cs

isElem':: Category -> Arrow -> Bool
isElem' cat (c1, c2) = cat == c1 || cat == c2

-- | given a category and another category          
-- listArrows :: Category -> [Category] -> [Arrow]  
-- listArrows cat = undefined           

-- | A : [B, C]
-- | D : [C, E]
-- | cat_ - category, e.g. catA (categoryA)
-- | r_   - arrow,    e.g. rBC  (arrowBC, arrow from B to C)

rBC :: Arrow
rBC = (catB, catC)
  
rCB :: Arrow
rCB = (catC, catB)
  
rCE :: Arrow
rCE = (catC, catE)
  
rEC :: Arrow
rEC = (catE, catC)

catA ::   Category
catA =    Category { label  = "catA"
                   , cats   = [catB                 -- | cat(s) ~ categor(y/ies)
                              ,catC]
                   , rs     = [rBC                  -- | rs ~ arrows              
                              ,rCB]}

catB ::   Category
catB =    Category { label  = "catB"
                   , cats   = []
                   , rs     = []}

catC ::   Category
catC =    Category { label  = "catC"
                   , cats   = []
                   , rs     = []}          

catD ::   Category
catD =    Category { label  = "catD"
                   , cats   = [catC
                              ,catE]
                   , rs     = [rCE
                              ,rEC]}

catE ::   Category
catE =    Category { label  = "catE"
                   , cats   = []
                   , rs     = []}                    

catAll :: Category
catAll =  Category { label  = "all"
                   , cats   = [catA
                              ,catB
                              ,catC
                              ,catD
                              ,catE]
                   , rs     = []}

-- | Partial Function for Category. According to Theorem 1.2::Entropy
-- | e.g. 
--   catC * catA   = Category { label = "catZ", cats = [catC, catB], rs = [rCB, rBC] }
--   catC * catAll = Category { label = "catZ", cats = [catC, catB, catE], rs = [rCB, rBC, rCE, rEC] }
     
apply :: Category -> Category -> Category
apply cat Default = apply cat catAll
apply (Category l1 cats1 rs1) (Category l2 cats2 rs2) = 
          Category { label = "catZ"
                   , cats  = cats1 ++ cats2
                   , rs    = rs1   ++ rs2 }

main :: IO ()
main = do
     putStrLn "Hey!"
