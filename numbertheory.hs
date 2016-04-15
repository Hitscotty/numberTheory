{-@author: Jonathan Portorreal-}
import Data.List
import Data.Maybe
import Text.Printf
import Data.Char

{- Used to test code for simple functions -}
test    = "VVHQWVVRMHUSGJG"

{- Used two versions to check for possible typos -}

kaiser  = "fpkzpiswquwfcroqrhzfcfpgzhivkwhhauripqswpipwwahbisz\
          \ghhyvbitaczluanptymzegzhyvzmwycjiuvvosfefbgyvuprwha\
          \vyximpawyxtkguhovkbjvialmtquszsduwjvijtquswzitmyhgh\
          \szmvosfieaqjquwuwphzfcuvvviwliqvocnlqzolbbixqxlrnldw\
          \wnvurmogavuxiiutwmididssurpbtvivpqlvoswsgvvymcrfwyo\
          \wwltmyhgvsdvyhgmpulkuumpaenfwhixceaovpkqpacnlqzcu\
          \ymsrbjlzywelgcsfsbmfjcorfzklgijfmpooorfmfimgmemtfhbiy\
          \mpvtbmeoguslefqquzczqlqbhnlqqtschixgdphnidtkcsmjqmn\
          \pbaweceoompadgashhqzplgmezljbaurrmnscqwtqrooxjazvo\
          \sgsebrhfnhuacwdyedmfavyvqtcawirepkwgvifeglbbmekquh\
          \yqbwthfcieegysuxnmuawhhunhlfyrfipkaivqwhashgdcgsonx\
          \tmvpaysrpkzrcwmxrlolezkgtwwlqtfqslduvurwqeecphbcqac\
          \usmwszakkslipbqisujuzuafuxqjkvzikuavhbxeemtpcowoipk\
          \wxefmhvfnlqvqisftdqblvcwfzwlgckzqhpquromjvkyzqzyvifh\
          \zwviswsymcwduvqvvmclwaugawgi"

scotty =  "fpkzpiswquwfcroqrhzfcfpgzhivkwhhauripqswpipwwahbisz\
           \ghhyvbitaczluanptymzegzhyvzmwycjiuvvosfefbgyvuprwha\
           \vyximpawyxtkguhovkbjvialmtquszsduwjvijtquswzitmyhgh\
           \szmvosfieaqjquwuwphzfcuvvviwliqvocnlqzolbbixqxlrnldw\
           \wnvurmogavuxiiutwmididssurpbtvivpqlvoswsgvvymcrfwyo\
           \wwltmyhgvsdvyhgmpulkuumpaenfwhixceaovpkqpacnlqzcu\
           \ymsrbjlzywelgcsfsbmfjcorfzklgijfmpooorfmfimgmemtfhbiy\
           \mpvtbmeoguslefqquzczqlqbhnlqqtschixgdphnidtkcsmjqmn\
           \pbaweceoompadgashhqzplgmezljbaurrmnscqwtqrooxjazvo\
           \sgsebrhfnhuacwdyedmfavyvqtcawirepkwgivfeglbbmekquh\
           \yqbwthfcieegysuxnmuawhhunhlfyrfipkaivqwhashgdcdsonx\
           \tmvpaysrpkzrcwmxrlolezkgtwwlqtfqslduvurwqeecphbcqac\
           \usmwszakkslipbqisujuzuafuxqjkvzikuavhbxeemtpcowoipk\
           \wxefmhvnlqvqisftdqblvcwfzwlgckzqhpquromjvkyzqzyvifh\
           \zwviswsymcwduvqvvmclwaugawgi"

{-takes an integer and a string and creates an array of tuples (a,b); 'a' being the first letter
of string and 'b' the displaced letter where the displacement is the given integer-}
displacement :: Int -> [Char] -> [(Char,Char)]
displacement n list     = zip list newList
          where newList = (drop n list)

{- takes an array of displaced tuples and returns a count of each tuple that has the same
values; this number is the coincidence count-}
coincidences :: [(Char,Char)] -> Int
coincidences displaced = length $ filter (\(a,b) -> a == b) displaced

{- Creates an array of coincidences from displacements 1 to 7-}
displayCoincidences :: [Char] -> [Int]
displayCoincidences list = map coincidences $ zipWith displacement [1..7] (replicate 7 list)

{- Create  an array of arrays where the inner arrays contain tuples of displacements
1 to 6. Then create an array of coincidences and indice of the maximum in this array would be the
keylength.-}
keyLength :: [Char] -> Int
keyLength list = (+1) . fromJust $ elemIndex findMax coincidenceCount
        where
              coincidenceCount = displayCoincidences list
              findMax          = maximum coincidenceCount

--                      helper functions
{- Takes and integer and any array groups together all first letters to n - letters of keylength-}
groupe :: Int -> [a] -> [a]
groupe _ [] = []
groupe n l | n > 0     = head (take n l) : (groupe n (drop n l))
           | otherwise = error "Negative n"

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs' :: String -> [(Char,Int)]
freqs' xs = zip (['A'..'Z']) ([(count x xs)| x <- ['a'..'z']])

freqs :: String -> [Int]
freqs xs = [(count x xs)| x <- ['a'..'z']]
--------------------------------------------------------------------
{-
Takes an integer and a string and creates tuples of occurences
-}
firstMethod :: Int -> [Char] -> [(Char,Int)]
firstMethod n list = zip ['a'..'z'] $ freqs shifted
     where shifted = groupe (keyLength list) ( drop n list)

{- takes a letter and finds the distance from e-}
getKey :: Char -> Int
getKey k = abs (ord 'e' - ord k)

-- Helper functions for testing keys
--------------------------------------------------------------------
-- Perform encryption or decryption, depending on f.
crypt f key = map toLetter . zipWith f (cycle key)
      where toLetter = chr . (+) (ord 'a')

-- Encrypt or decrypt one letter.
enc :: Char -> Char -> Int
enc k c = (ord k + ord c) `mod` 26

dec :: Char -> Char -> Int
dec k c = (ord c - ord k) `mod` 26

-- Given a key, encrypt or decrypt an input string.
encrypt :: [Char] -> [Char] -> [Char]
encrypt = crypt enc

decrypt :: [Char] -> [Char] -> [Char]
decrypt = crypt dec

-- Convert a string to have only upper case letters.
convert = map toUpper . filter isLetter

--                        RSA ENCRYPTION
--------------------------------------------------------------------

divisors :: Integer -> [Integer]
divisors ns = [x | x <- [1..ns], mod ns x == 0]


isPrime :: Integer -> Bool
isPrime n = n >= 1 && null [m | m <- [2..(n-1)], n `mod` m == 0, (m^2) <= n]

findPrimes :: Int -> [Integer]
findPrimes n = take n [x | x <- [1..], isPrime x]
