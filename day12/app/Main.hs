module Main where

import Text.Parsec (parse, ParseError, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, char, string)
import Text.Parsec.Combinator (sepBy)

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Either
import Data.Maybe

type Graph = Map String [String]

-- parsing code
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseNode :: Parser String
parseNode = do { node <- many letter; return node }

parseLink :: Parser (String, String)
parseLink = do { node1 <- parseNode ; char '-' ; node2 <- parseNode ; return (node1, node2) }

parseLinks :: Parser [(String, String)]
parseLinks = sepBy parseLink (char '\n')

-- convert list of edges into a graph
makeGraph :: [(String, String)] -> Graph
makeGraph = (foldl insertLink Map.empty) . addFlippedLinks
    where
        addFlippedLinks links = links ++ (map (uncurry $ flip (,)) links)
        insertLink graph (start, end) = Map.insertWith (++) start [end] graph

-- gets all paths from start to end
getPaths :: Graph -> [[String]]
getPaths graph = getPaths' ["start"]
    where
        getPaths' visited@("end": rest) = [visited]
        getPaths' visited@(curr: rest) = concat $ map (\n -> getPaths' (n : visited)) nextNodes
            where
                isSmallCave node = (node /= "start") && (all isLower node)
                smallVisited = filter isSmallCave visited
                hasSmallBeenVisitedTwice = ((length (nub smallVisited)) - length smallVisited) == -2
                cantBeNext node = (node == "start") || ((isSmallCave node) && hasSmallBeenVisitedTwice)
                nextNodes = filter (not . cantBeNext) (fromJust $ Map.lookup curr graph)


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let edges = fromRight [] (regularParse (parseLinks) input)
    print $ length $ getPaths $ makeGraph edges
