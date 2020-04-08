module Ciphers where
    {- V2, Written by: Daniel Matyas Perendi -}
    import AssignmentHelp
    import Data.List --needed for validateCipher
    import Data.Char --needed for partialDecode

    alphabet = ['A'..'Z']

    {- Checking if the sorted cipher contains the same elements as the uppercase alphabet -}
    validateCipher :: Cipher -> Bool
    validateCipher c  
        | sort c == alphabet = True
        | otherwise = False
    {- Testing: I called this function on the rotors supplied in AssignmentHelp.hs and it returned true for all of them, furthermore I 
    called this function on "EKMFLGDQVZNTOWYHXUSPAIBRCj" to try lowercase, and on "EEMFLGDQVZNTOWYHXUSPAIBRCJ" to try duplicates, 
    it returend false for both of them -}

    {- Using zip on the alphabet and the list which is created by separating the cipher
     at (length-offset) and switching the two sublists, then getting the specific character's matching encoded character
     UPDATED: I used head,fst and snd, so I changed it to the solution code to make it more efficient -}
    encode :: Cipher -> Int -> Char -> Char
    encode cipher offset c = cipher !! (((alphaPos c)+offset) `mod` 26)
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

    {-- (Added from solutions) findLoc returns the position of an item in a list when we know that
      we will find the item in the list --}
    findLoc :: Eq a => a -> [a] -> Int
    findLoc item [single] = 0
    findLoc item (x:xs)
        | item == x = 0
        | otherwise = 1 + findLoc item xs

    {- Using zip on the alphabet and the list which is created by separating the cipher at (lenght-offset) 
    and switching the two sublists, then getting the specific encoded character's matching original character
    UPDATED: I used head,fst and snd, so I changed it to the solution code to make it more efficient -}
    reverseEncode :: Cipher -> Int -> Char -> Char
    reverseEncode cipher offset c
        = alphabet !! (((findLoc c cipher)-offset) `mod` 26)

    {- Testing: As it should return the original value if I call this function on the encode function, I tried the following:
    reverseEncode "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 7 (encode "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 7 'A'), it returned 'A'-}

    {- reverseEncodeMessage takes a cipher, an offset and a string, and decodes every character one by one using the reverseEncode function -}
    reverseEncodeMessage :: Cipher -> Int -> String -> String
    reverseEncodeMessage c o [] = []
    reverseEncodeMessage c o (x:xs) = charToString (reverseEncode c o x) ++ reverseEncodeMessage c o xs
    {- Testing: I used the same method to test this function as in the testing of reverseEncode:
    reverseEncodeMessage "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 (encodeMessage "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 6 "HELLOITSME"),
     and it returned HELLOITSME so it worked -}

    {- UPDATED: I had rounding problems, so I used the code from the solutions -}
    letterStats :: String -> [(Char, Int)]
    letterStats message
        = mergesort correctOrder (filter (\(c, p) -> p /= 0) counts)
            where
            counts =  map (\c -> freqChar c) alphabet
            freqChar c = (c, percent ((length.filter (== c)) message) n)
            n = length message
            correctOrder (c1, p1) (c2, p2) = p1 > p2
    {- Testing: I tried an easy word where I can calculate the percentages to check the result, so I 
    called the function on "HELLO" where it returned the following: [('L',40),('H',20),('E',20),('O',20)] , which is the correct answer -}

    {- Helper function for partialDecode (it takes 2 characters and a string) replacing the first character's
     all occurences in the string with the second character -}
    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c

    {- partialDecode takes two arguments: a list of (Char,Char) tuples and a string. It replaces all occurences of the second char in the
    string with the lowercase first character for each tuple recursively using the replace function UPDATE: using pattern matching now -}
    partialDecode :: [(Char,Char)] -> String -> String
    partialDecode [] string = string
    partialDecode ((x,y):rest) string = replace y (toLower x) (partialDecode rest string)
    {- Testing: I tested this function with the "HELLO" string where I wanted to replace the H with G and L with P. I got the following result:
    "gEppO", so it worked.
    Decoding the mystery: partialDecode [('M','P'),('E','W'),('S','A'),('A','X'),('G','M'),('T','J'),('H','C'),('I','Q'),('Y','R'),('O','F'),
    ('U','D'),('B','B'),('N','Y'),('V','K'),('L','N'),('D','H'),('R','E'),('P','V'),('C','L'),('K','Z'),('F','T'),('W','S'),('Z','U')] mystery
    "itseasytobreakasubstitutioncipherprovidedyouhavealongenoughmessagestopletsmakethisonealittlebitlongerstopokitshouldbetherightsortofsize
    nowstopmaybenotletsincreasethemessagelengthabitmorestopkeepthismessagesecretorshareifyouwantthewholeclasstogetthebonusmarksstop" -}

    {- Start of Assignment 2 -}

    type Rotor = Cipher
    type Reflector = [(Char,Char)]
    type Offsets = (Int,Int,Int)
    type Steckerboard = [(Char,Char)]
    type Menu = [Int]
    type Crib = ([Char],[Char])
    data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets 
        | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

    {- Advancing a set of offsets according to the rules of the Enigma -}
    advance :: Offsets -> Offsets
    advance (ol,om,or)
        | or < 25 = (ol,om,or+1)
        | om < 25 = (ol,om+1,0)
        | ol < 25 = (ol+1,0,0)
        | otherwise = (0,0,0)

    {- Advancing rotors in an engima -}
    advanceEnigma :: Enigma -> Enigma
    advanceEnigma (SimpleEnigma lr mr rr r offsets) = (SimpleEnigma lr mr rr r (advance offsets))
    advanceEnigma (SteckeredEnigma lr mr rr r offsets sb) = (SteckeredEnigma lr mr rr r (advance offsets) sb)

    {- pairLetters is pairing the specific letter to its pair in a list of (char,char) pairs -}
    pairLetters :: Char -> [(Char,Char)] -> Char
    pairLetters c [] = c -- if no pair found, returns itself
    pairLetters c ((x,y):ps) 
        | x == c = y
        | y == c = x
        | otherwise = pairLetters c ps

    {- Modified version of encode optimised for the enigma machine -}
    encode2 :: Cipher -> Int -> Char -> Char
    encode2 cipher offset c = alphabet !! (((findLoc (encode cipher offset c) alphabet)-offset) `mod` 26)

    {- Modified version of reverseEncode optimised for the enigma machine -}
    reverseEncode2 :: Cipher -> Int -> Char -> Char
    reverseEncode2 cipher offset c
        = (reverseEncode cipher offset (alphabet !! (((findLoc c alphabet)+offset) `mod` 26)))

    {- Encodes a char using RR then MR then LR, then swapping the letter using a reflector
     (1st half of the encoding process -}
    encodeRightToLeft :: Char -> Enigma -> Char
    encodeRightToLeft c (SimpleEnigma lr mr rr r (x,y,z)) = 
        pairLetters (encode2 lr x (encode2 mr y (encode2 rr z c))) r
    encodeRightToLeft c (SteckeredEnigma lr mr rr r (x,y,z) sb) = 
        pairLetters (encode2 lr x (encode2 mr y (encode2 rr z (pairLetters c sb)))) r

    {- Using reverseEncode decodes a char using LR, then MR and finally RR 
    (2nd half of the encoding process) -}
    encodeLeftToRight :: Char -> Enigma -> Char
    encodeLeftToRight c (SimpleEnigma lr mr rr r (x,y,z)) = 
        reverseEncode2 rr z (reverseEncode2 mr y (reverseEncode2 lr x c))
    encodeLeftToRight c (SteckeredEnigma lr mr rr r (x,y,z) sb) = 
        pairLetters (reverseEncode2 rr z (reverseEncode2 mr y (reverseEncode2 lr x c))) sb

    {- enigmaEncode encodes a letter through the enigma first right to left, then reverseEncodes left to right -}
    enigmaEncode :: Char -> Enigma -> Char
    enigmaEncode c e = encodeLeftToRight (encodeRightToLeft c (advanceEnigma e)) (advanceEnigma e)
    {- Testing: I tested letter 'A' with te(specified later), returned 'N' as it's supposed to, tested 'N' as well,
    which then returned 'A', so it's symmetric. I tested a steckered enigma (tse) with an empty list as steckerboard,
    it worked the same way as a SimpleEnigma. I tested it with a sample steckerboard as well and it worked properly.
    I assumed that the steckerboard is given properly (no more than 10 pairs and no repeated pairs, one letter only 
    appear once in the list) -}

    {- enigmaEncodeMessage encodes a message while advancing the Enigma after every key press -}
    enigmaEncodeMessage :: String -> Enigma -> String
    enigmaEncodeMessage [] e = []
    enigmaEncodeMessage (x:xs) e = charToString(enigmaEncode x e) ++ enigmaEncodeMessage xs (advanceEnigma e)
    {- Testing: first of all I tested if the rotors are working the way thes supposed to by encoding 'AAAA' which
    returned 4 different characters, so the rotor positions changed every time. I also tested it on the long message
    given by the professor, and it returned an identical encoded message to the one on the assigment webpage. This
    function has been tested with a steckerboard as well and it worked as it was supposed to. I assumed that the 
    steckerboard is given properly (no more than 10 pairs and no repeated pairs, one letter only appear once in the list) -}

    {- Helper functions for longestMenu: -}

    {- Finding the positions of a character in a list of characters -}
    findCharPos :: Char -> [Char] -> [Int]
    findCharPos ch l = [ y | (x, y) <- zip l [0..], x == ch ]

    {- findNext is given a position, a plain text and a cipher text, and returns the first position of the next
    character in the menu in the plain text-}
    findNext :: Int -> [Char] -> [Char] -> Int
    findNext pos p c
        | (c !! pos ) `elem` p = (plainPos!!0)
        | otherwise = pos
            where 
                plainPos = (findCharPos (c !! pos ) p)
    
    {- allPossSteps is given a position, a plain text and a cipher text returns a list of all possible steps from
    the given index to build the menu -}
    allPossSteps :: Int -> [Char] -> [Char] -> [Int]
    allPossSteps pos p c
        |(c !! pos) `elem` p = findCharPos (p !! (findNext pos p c)) p
        |otherwise = []

    {- ind helper function takes the plain text and returns the list of 
    character indices -}
    ind :: [Char] -> [Int]
    ind p = [0..(length p)-1]
 
    {- initMenu builds the initial list of menus from a list of indices -}
    initMenu :: [Int] -> [Menu]
    initMenu [] = []
    initMenu (i:is) = [[i]] ++ initMenu is

    {- build takes a menu and a list of possible next steps, then returns new list of menus adding 
    the possible steps to the end of the menu (list of menus) -}
    build :: Menu -> [Int] -> [Menu]
    build menu [] = []
    build menu (s:ss) = [menu ++ [s]] ++ build menu ss

    {- buildMenu is given a list of menus, a plain text and a cipher text extends the current list of menus
    with one possible step from their last indices then removes the ones with duplicate indices-}
    buildMenu :: [Menu] -> [Char] -> [Char] -> [Menu]
    buildMenu [] p c = []
    buildMenu (i:is) p c = cleanList ((build i (allPossSteps (last i) p c)) ++ buildMenu is p c)

    {- hasNoDuplicates is given a list returns a boolean whether the list contains duplicates
    (helper function for cleanList) -}
    hasNoDuplicates :: Eq a => [a] -> Bool
    hasNoDuplicates [] = True
    hasNoDuplicates (x:xs)
        | x `elem` xs = False
        | otherwise = hasNoDuplicates xs

    {- cleanList is given a list menus deletes menus with duplicate indices -}
    cleanList :: [Menu] -> [Menu]
    cleanList l = filter hasNoDuplicates l

    {- loopBuildMenu is given an initial list of menus looping through the buildMenu operations until no longer
    menus exist -}
    loopBuildMenu :: [Menu] -> [Char] -> [Char] -> [Menu]
    loopBuildMenu m p c  
        | newMenus == [] = m
        | otherwise = loopBuildMenu newMenus p c
        where newMenus = buildMenu m p c

    {- longestMenu is given a Crib returns one of the longest menus (the first one in the list of longest menus).
    I am assuming that the plain text and the cipher text are the same size in the crib -}
    longestMenu :: Crib -> Menu
    longestMenu (x,y) = (loopBuildMenu (initMenu (ind x)) x y) !! 0
    {- Testing: Tested on the given example in the assignment handout, however I didn't get the same result,
    I got another list of size 17. Because my result started with index 13, I tested this function on a message 
    and cipher of size 13, which returned the right solution following the steps in the example crib table. -}
    
    {- Enigma examples for testing purposes -}
    te2 = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0)
    te = SimpleEnigma rotor3 rotor2 rotor1 reflectorB (25, 25, 25)
    tse = SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (25,25,25) []
    tse2 = SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,0) [('A','G'),('R','E'),('Z','P')]