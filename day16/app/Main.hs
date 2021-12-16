module Main where

import Data.List.Split (chunksOf)

type PacketVersion = Int
type PacketType = Int
data Packet = Literal PacketVersion PacketType Int | Operation PacketVersion PacketType [Packet] deriving (Show)

-- chat to hex digits
asHex :: Char -> [Int]
asHex '0' = [0,0,0,0]
asHex '1' = [0,0,0,1]
asHex '2' = [0,0,1,0]
asHex '3' = [0,0,1,1]
asHex '4' = [0,1,0,0]
asHex '5' = [0,1,0,1]
asHex '6' = [0,1,1,0]
asHex '7' = [0,1,1,1]
asHex '8' = [1,0,0,0]
asHex '9' = [1,0,0,1]
asHex 'A' = [1,0,1,0]
asHex 'B' = [1,0,1,1]
asHex 'C' = [1,1,0,0]
asHex 'D' = [1,1,0,1]
asHex 'E' = [1,1,1,0]
asHex 'F' = [1,1,1,1]

-- binary to decimal
binToDec :: [Int] -> Int
binToDec = sum . (zipWith (\i b -> b * (2 ^ i)) [0..]) . reverse

-- take n elements and return them and the remaining elements in a list
takeDrop :: Int -> [a] -> ([a], [a])
takeDrop n elems = (take n elems, drop n elems)

-- parse packet and return the rest of the sequence
parsePacket :: [Int] -> (Packet, [Int])
parsePacket sequence
    | packetType == 4   = (Literal packetVersion packetType literalVal, sequenceWithoutLiteral)
    | lengthTypeId == 0 = (Operation packetVersion packetType packets0, sequenceWithoutPackets0)
    | lengthTypeId == 1 = (Operation packetVersion packetType packets1, sequenceWithoutPackets1)
    where
        (versionSeq, sequenceWithoutVersion) = takeDrop 3 sequence
        (typeSeq, sequenceWithoutType) = takeDrop 3 sequenceWithoutVersion
        packetVersion = binToDec versionSeq
        packetType = binToDec typeSeq

        -- literal parsing code
        (literalVal, sequenceWithoutLiteral) = parseLiteral sequenceWithoutType
        parseLiteral :: [Int] -> (Int, [Int])
        parseLiteral seq = (binToDec literalSeq, drop (numDigitsToParse * 5) seq)
            where
                digitChunks = chunksOf 5 seq :: [[Int]]
                numDigitsToParse = (+1) $ length $ takeWhile (\(last: digit) -> last == 1) digitChunks :: Int
                literalSeq = (concat . map tail . take numDigitsToParse) digitChunks

        -- operator parsing code
        (lengthTypeId : sequenceWithoutLengthTypeID) = sequenceWithoutType

        -- length type id == 0
        (packets0, sequenceWithoutPackets0) = parseLengthTypeId0 sequenceWithoutLengthTypeID
        parseLengthTypeId0 :: [Int] -> ([Packet], [Int])
        parseLengthTypeId0 seq = (parseSubPackets packetSeq [], seqWithoutPackets)
            where
                (numBitsSeq, seqWithoutNumBits) = takeDrop 15 seq
                numPakcetBits = binToDec numBitsSeq
                (packetSeq, seqWithoutPackets) = takeDrop numPakcetBits seqWithoutNumBits

        -- parse sub packets until string is exhausted
        parseSubPackets :: [Int] -> [Packet] -> [Packet]
        parseSubPackets [] packets = reverse packets
        parseSubPackets packetSeq packets = parseSubPackets packetSeq' (packet: packets)
            where
                (packet, packetSeq') = parsePacket packetSeq

        -- length type id == 1
        (packets1, sequenceWithoutPackets1) = parseLengthTypeId1 sequenceWithoutLengthTypeID
        parseLengthTypeId1 :: [Int] -> ([Packet], [Int])
        parseLengthTypeId1 seq = (reverse packets, seqWithoutPackets)
            where
                (numPacketsSeq, seqWithoutNumPackets) = takeDrop 11 seq
                numPackets = binToDec numPacketsSeq
                (packets, seqWithoutPackets) = last $ take (numPackets + 1) $ iterate (\(ps, ss) -> let (p, ss') = parsePacket ss in (p: ps, ss')) ([], seqWithoutNumPackets)


sumVersions :: Packet -> Int
sumVersions (Literal v _ _) = v
sumVersions (Operation v _ packets) = v + (sum . map (sumVersions)) packets

eval :: Packet -> Int
eval (Literal _ 4 val) = val
eval (Operation _ 0 packets)  = (sum . map eval) packets
eval (Operation _ 1 packets)  = (product . map eval) packets
eval (Operation _ 2 packets)  = (minimum . map eval) packets
eval (Operation _ 3 packets)  = (maximum . map eval) packets
eval (Operation _ 5 [p1, p2]) = fromEnum ((eval p1) > (eval p2))
eval (Operation _ 6 [p1, p2]) = fromEnum ((eval p1) < (eval p2))
eval (Operation _ 7 [p1, p2]) = fromEnum ((eval p1) == (eval p2))

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let inputBinary = (concat . map asHex) input
    print $ sumVersions $ fst $ parsePacket $ inputBinary
    print $ eval $ fst $ parsePacket $ inputBinary