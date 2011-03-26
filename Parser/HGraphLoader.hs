-----------------------------------------------------------------------------
--
-- Module      :  HGraphLoader
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser.HGraphLoader (
loadHGraph
) where



import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language ( emptyDef )
import Data.List
import Data.Char
import Data.Either
import GHC.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set


newtype NT' = NT' String deriving (Ord,Show,Eq)
newtype T' = T' String deriving (Ord,Show,Eq)
type LHS' = NT'
type GRAMMAR = (Set T', Set NT', NT', [RULE'])
data RULE' = R' LHS' RHS' WEIGHT' deriving (Ord,Show,Eq)
data RHS' = Leaf NT' | Branch T' [Either T' NT']  deriving (Ord,Show,Eq)
newtype WEIGHT' = W' Double   deriving (Show,Eq)

instance Ord WEIGHT' where
    compare (W' w1) (W' w2) = compare w2 w1
       -- | w1 > w2 = LT -- inverse
       -- | otherwise = GT


type HWeights hEdge hWeight = hEdge -> [hWeight] -> hWeight
type HBack hEdge hNode = hNode -> [(hEdge,[hNode])]
type HGraph hNode hEdge hWeight = ([hNode],HBack hEdge hNode ,HWeights hEdge hWeight )
type HQuery hNode hEdge hWeight = (hNode,HGraph hNode hEdge hWeight)
data HPath hEdge = B hEdge [HPath hEdge] deriving (Eq,Show)
data Pair a b = P a b deriving Show

configStyle   = emptyDef
                { P.commentLine    = "%"
                , P.reservedNames  = ["#","->"]
                , P.reservedOpNames= []
                , P.caseSensitive  = True
                }

lexer       = P.makeTokenParser configStyle

parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
float       = P.float lexer
integer     = P.integer lexer
reserved    = P.reserved lexer
symbol      = P.symbol lexer

p_NT :: Parser NT'
p_NT = P.lexeme lexer $ do
                            c  <- lower
                            cs <- many (alphaNum <|> char '_' <|> char '+' <|> char '-')
                            return (NT' (c:cs))

p_T :: Parser T'
p_T =  P.lexeme lexer $ do
                            c  <- upper <|> oneOf ":\'`+-_$"
                            cs <- many (alphaNum <|> (oneOf "+_-\'´`$"))
                            return (T' (c:cs))

p_RULE :: Parser RULE'
p_RULE = P.lexeme lexer $ do
                             lhs <- p_LHS
                             symbol "->"
                             rhs <- p_RHS
                             symbol "#"
                             weight <- p_WEIGHT
                             return (R' lhs rhs weight)

p_LHS :: Parser LHS'
p_LHS = p_NT

p_RHS :: Parser RHS'
p_RHS = p_RHS_Leaf <|> p_RHS_Branch

p_RHS_Leaf :: Parser RHS'
p_RHS_Leaf = P.lexeme lexer $ do
                                  nt <- p_NT
                                  return (Leaf nt)

p_RHS_Branch :: Parser RHS'
p_RHS_Branch = P.lexeme lexer $ do
                                    t <- p_T
                                    eithers <- parens (many p_RHS_Branch_Either)
                                    return (Branch t eithers)

p_RHS_Branch_Either :: Parser (Either T' NT')
p_RHS_Branch_Either = do
                        nt <- p_NT
                        return (Right nt)

                      <|> do
                        t <- p_T
                        return (Left t)


p_WEIGHT :: Parser WEIGHT'
p_WEIGHT = do
                weight <- float <|> float_withoutLeadingZero
                return (W' weight)

float_withoutLeadingZero :: Parser Double
float_withoutLeadingZero =  P.lexeme lexer ( do
                                char '.'
                                digits <- many1 digit <?> "float_withoutLeadingZero"
                                return (foldr op 0.0 digits)
                             <?> "float_withoutLeadingZero")
                                where
                                    op d f    = (f + fromIntegral (digitToInt d))/10.0


p_GRAMMAR :: Parser GRAMMAR
p_GRAMMAR = do
            s <- p_NT
            rules <- (many p_RULE)
            let (ts,nts) = getSymbols rules
            return (ts,Set.insert s nts,s,rules)

getSymbols :: [RULE'] -> (Set T', Set NT')
getSymbols rules = foldr f (Set.empty,Set.empty) rules
    where
        f (R' lhs rhs weight) (ts,nts) = (Set.union ts' ts,Set.union nts' (Set.insert lhs nts))
            where
                (ts',nts') = getSymbols' rhs
                getSymbols' :: RHS' -> (Set T', Set NT')
                getSymbols' (Leaf nt) = (Set.empty, Set.singleton nt)
                getSymbols' (Branch t ls) =  (Set.insert t ts, nts)
                    where (ts,nts) = (Set.fromList $ lefts ls, Set.fromList $ rights ls)


transform :: GRAMMAR ->  HQuery Int Int WEIGHT'
transform (ts,nts,s,rules) = ((Map.!) ntMap s, (nodes, (IntMap.!) hBack, (IntMap.!) hWeights))
    where
         rules' = map (\(sym, (R' lhs rhs weight)) -> (sym,((Map.!) ntMap lhs), map ((Map.!) ntMap) (f rhs) ,weight))  (zip [1..] rules)
         hBack = foldr (\(sym,dst,srcs,_) m -> IntMap.insertWith (++) dst [(sym,srcs)] m) defaultMap rules'
         hWeights = IntMap.fromList $ map (\(s,_,_,w) -> (s,foldr (\(W' d) (W' d') -> W' (d * d')) w)) rules'
         ntMap = Map.fromAscList $  zip (Set.toAscList nts) [1..]
         f (Leaf nt) = [nt]
         f (Branch t list) = rights list
         defaultMap = IntMap.fromAscList (map (\x -> (x,[]) ) nodes)
         nodes = Map.elems ntMap

loadHGraph file = do
                parsedcontent <- parseFromFile p_GRAMMAR file
                case parsedcontent of
                    (Left err) -> return (Left (concatMap messageString (errorMessages err)))
                    (Right a) -> return (Right $ transform a)

{-
data Symbol = T String | NT String deriving (Eq, Show)
	
configStyle   = emptyDef
                { P.commentLine    = "%"
                , P.reservedNames  = ["#","->"]
                , P.reservedOpNames= []
                , P.caseSensitive  = True
                }

configline = P.lexeme lexer  $  do
                gSym <- gSymP
                symbol "->"
                (try gSymP)
                gSyms  <-  gSymsP
                symbol "#"
                weight <- float
                -- return (gSym,gSyms,weight)
                return (gSym,gSyms,weight)
config = do
            iNode <- P.lexeme lexer gSymP
            h <- P.lexeme lexer (many configline)
            return (iNode,h)


gSymsP :: Parser [Symbol]
gSymsP = P.lexeme lexer  $  parens (many gSymP) <|> do
                                                        c <- gSymP
                                                        return [c]

gSymP :: Parser Symbol
gSymP = P.lexeme lexer  $ gSymNtP <|> gSymTP
    where
        gSymTP  =  do
                               c  <- upper <|> oneOf ":\'`+-_$"
                               cs <- many (alphaNum <|> (oneOf "+_-\'´`$"))
                               return (T (c:cs))

        gSymNtP =  do
                               c  <- lower
                               cs <- many (alphaNum <|> char '_' <|> char '+' <|> char '-')
                               return (NT (c:cs))









newtype DDouble = D Double deriving (Eq,Show)

instance Ord (DDouble) where
    compare (D d) (D d') = compare d' d
--
f :: (Symbol,[(Symbol,[Symbol],Double)]) -> (Int,([Int],Int -> [(Int,[Int])],Int -> [DDouble] -> DDouble ))
f ((NT initN),list) = (iNode,hGraph)
  where
        (dests,srcslist,weights) = unzip3 list
        dests' = map (\(NT a) -> a) dests
        srcslist' = map ((map (\(NT a) -> a)).(filter isNt)) srcslist
            where
                isNt (NT _) = True
                isNt (T _) = False
        nMap = Map.fromList $ zip (nub (initN:(dests' ++ (concat srcslist')))) [0..]
        iNode = (Map.!) nMap initN
        list' = zip4 [0..] (map ((Map.!) nMap) dests') (map (map ((Map.!) nMap)) srcslist') weights
        hGraph = (Map.elems nMap, ((Map.!) hBack), ((Map.!) hWeights))
        hWeights = Map.fromList $ map (\(s,_,_,w) -> (s,foldr (\(D d) (D d') -> D (d * d')) (D w))) list'
        hBack = foldr (\(sym,dst,srcs,_) m -> Map.insertWith (++) dst [(sym,srcs)] m) Map.empty list'


loadHGraph file = do
                parsedcontent <- parseFromFile config file
                case parsedcontent of
                    Left err -> return (Left (concatMap messageString (errorMessages err)))
                    Right a -> return (Right (f a))

-}
