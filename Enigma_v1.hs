module Ciphers where
    {- V1, Written by: Daniel Matyas Perendi -}
    import AssignmentHelp
    import Data.List (nubBy, sortBy, sort) --needed for validateCipher, removeDuplicates and letterStats
    import Data.Ord (comparing) --needed for letterStats
    import Data.Function (on) --needed for removeDuplicates
    import Data.Char --needed for partialDecode

    {- Checking if the sorted cipher contains the same elements as the uppercase alphabet -}
    validateCipher :: Cipher -> Bool
    validateCipher c  
        | sort c == ['A'..'Z'] = True
        | otherwise = False
    {- Testing: I called this function on the rotors supplied in AssignmentHelp.hs and it returned true for all of them, furthermore I 
    called this function on "EKMFLGDQVZNTOWYHXUSPAIBRCj" to try lowercase, and on "EEMFLGDQVZNTOWYHXUSPAIBRCJ" to try duplicates, 
    it returend false for both of them -}

    {- Using zip on the alphabet and the list which is created by separating the cipher
     at (length-offset) and switching the two sublists, then getting the specific character's matching encoded character -}
    encode :: Cipher -> Int -> Char -> Char
    encode cipher offset letter =  head [y | (x,y) <- zip ['A'..'Z'] (snd(splitAt (length cipher - offset) cipher) ++ fst(splitAt (length cipher - offset) cipher)), letter == x]
    {- Testing: I called this function on the example cipher from the handout (EKMFLGDQVZNTOWYHXUSPAIBRCJ) and 2 as offset.
    Character 'A' returned 'C', character 'K' returned 'V' -}

    {- Helper function for encodeMessage, turns a character into a string -}
    charToString :: Char -> String
    charToString c = [c]
    
    {- encodeMessage takes a cipher, and offset and a string, then it encodes every character in the string one by one using the encode function. -}
    encodeMessage :: Cipher -> Int -> String -> String
    encodeMessage c o [] = []
    encodeMessage c o (x:xs) = charToString (encode c o x) ++ encodeMessage c o xs
    {- Testing: I used the example mentioned in the assignment handout, so with 0 offset encoding "SPANGLES" returned "SHEWDTLS",
    with an offset of 5 it returned "WNIVKDJW" -}

    {- Using zip on the alphabet and the list which is created by separating the cipher
     at (lenght-offset) and switching the two sublists, then getting the specific encoded character's matching original character -}
    reverseEncode :: Cipher -> Int -> Char -> Char
    reverseEncode cipher offset letter = head [x | (x,y) <- zip ['A'..'Z'] (snd(splitAt (length cipher - offset) cipher) ++ fst(splitAt (length cipher - offset) cipher)), letter == y]
    {- Testing: As it should return the original value if I call this function on the encode function, I tried the following:
    reverseEncode "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 7 (encode "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 7 'A'), it returned 'A'-}

    {- reverseEncodeMessage takes a cipher, an offset and a string, and decodes every character one by one using the reverseEncode function -}
    reverseEncodeMessage :: Cipher -> Int -> String -> String
    reverseEncodeMessage c o [] = []
    reverseEncodeMessage c o (x:xs) = charToString (reverseEncode c o x) ++ reverseEncodeMessage c o xs
    {- Testing: I used the same method to test this function as in the testing of reverseEncode:
    reverseEncodeMessage "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 (encodeMessage "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 "HELLOITSME"), and it returned HELLOITSME so it worked -}

    {- Helper function for letterStats counting the occurences of every letter in a string returning a list containing these numbers(times 100) -}
    count :: String -> [Int]
    count (x:xs) = [100*(length (filter (==x) (x:xs)))] ++ count xs

    {- Helper function for letterStats removing duplicates considering the first tuple -}
    removeDuplicates :: Eq a => [(a, b)] -> [(a, b)]
    removeDuplicates = nubBy ((==) `on` fst)

    {- letterStats takes a string, zip it with the list we get from the count function, then remove the duplicates and zeros, then sort it in descending order.
    We get a list of tuples where the first element is a Char and the second is the percentage of its occurences in the string -}
    letterStats :: String -> [(Char, Int)]
    letterStats [] = []
    letterStats string = sortBy (flip $ comparing snd) (removeDuplicates (filter (\(x,y) -> 0 < y) (zip string (map (`div` length string) (count string)))))
    {- Testing: I tried an easy word where I can calculate the percentages to check the result, so I called the function on "HELLO"
    where it returned the following: [('L',40),('H',20),('E',20),('O',20)] , which is the correct answer -}

    {- Helper function for partialDecode (it takes 2 characters and a string) replacing the first character's all occurences in the string with the second character -}
    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c

    {- partialDecode takes two arguments: a list of (Char,Char) tuples and a string. It replaces all occurences of the second char in the string with the lowercase first 
    character for each tuple recursively using the replace function -}
    partialDecode :: [(Char,Char)] -> String -> String
    partialDecode [] string = string
    partialDecode (x:xs) string = replace (snd x) (toLower (fst x)) (partialDecode xs string)
    {- Testing: I tested this function with the "HELLO" string where I wanted to replace the H with G and L with P. I got the following result:
    "gEppO", so it worked.
    Decoding the mystery: partialDecode [('M','P'),('E','W'),('S','A'),('A','X'),('G','M'),('T','J'),('H','C'),('I','Q'),('Y','R'),('O','F'),('U','D'),('B','B'),('N','Y'),('V','K'),
    ('L','N'),('D','H'),('R','E'),('P','V'),('C','L'),('K','Z'),('F','T'),('W','S'),('Z','U')] mystery
    "itseasytobreakasubstitutioncipherprovidedyouhavealongenoughmessagestopletsmakethisonealittlebitlongerstopokitshouldbetherightsortofsizenowstopmaybenotletsincreasethemessage
    lengthabitmorestopkeepthismessagesecretorshareifyouwantthewholeclasstogetthebonusmarksstop" -}