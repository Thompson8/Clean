module stackCalculator

import StdEnv
import StdMaybe

isDecimalDigit :: Char -> Bool
isDecimalDigit x 
	| x == '0' = True
	| x == '1' = True
	| x == '2' = True
	| x == '3' = True
	| x == '4' = True
	| x == '5' = True
	| x == '6' = True
	| x == '7' = True
	| x == '8' = True
	| x == '9' = True
	| otherwise = False 

parseDigit :: Char -> Int
parseDigit x 
	| x == '0' = 0
	| x == '1' = 1
	| x == '2' = 2
	| x == '3' = 3
	| x == '4' = 4
	| x == '5' = 5
	| x == '6' = 6
	| x == '7' = 7
	| x == '8' = 8
	| x == '9' = 9	

isSign :: Char -> Bool
isSign x
	| x == '+' = True
	| x == '-' = True
	| otherwise = False

isInteger :: String -> Bool
isInteger s
	| s == "" = False
	| otherwise = ((isSign x && (not (isEmpty xs)))
		|| (isDecimalDigit x)) && (all isDecimalDigit xs) 	
		where [x:xs] = fromString s	  

strToInteger :: String -> Int
strToInteger s 
	| x == '-' = ~ (fst (parseCharArrayToInt xs))
	| x == '+' = fst (parseCharArrayToInt xs)
	| otherwise = fst (parseCharArrayToInt [x:xs])
	where [x:xs] = fromString s

parseCharArrayToInt :: [Char] -> (Int, Int)
parseCharArrayToInt [x:xs]
	| xs == [] = (parseDigit x , 10)
	| otherwise = (s + (c * (parseDigit x)), c * 10)
		where (s, c) = parseCharArrayToInt xs

::Stack a :== [a]
Empty :: (Stack a)
Empty = []
isEmptyStack :: (Stack a) -> Bool
isEmptyStack [] = True
isEmptyStack s = False
top :: (Stack a) -> a
top [e:s] = e
top _ = abort "not enough parameters"
push :: (Stack a) a -> Stack a
push s e = [e:s]
pop :: (Stack a) -> (a, Stack a)
pop [e:s] = (e, s)
pop _ = abort "not enough parameters"
pop2 :: (Stack a) -> (a, a, Stack a)
pop2 [e1:e2:s] = (e1, e2, s)
pop2 _ = abort "not enough parameters"

::IFun1 :== (String, (Int -> Int))
::IFun2 :== (String, (Int -> Int -> Int))

findFunction :: [(String,f)] String -> Maybe f
findFunction [] s = Nothing

findFunction [(k, e):xs] s   
	| k == s =  Just e	
	| otherwise = findFunction xs s
			
step :: ([IFun1],[IFun2]) (Stack Int) String -> (Stack Int)
step (i1, i2) stack s
		| isInteger s = push stack (strToInteger s)
		| isJust m1 = push s1 ((fromJust m1) e1)
		| isJust m2 = push s2 ((fromJust m2) e2 e3)
		| otherwise = abort "unknown operator"
		where m1 = findFunction i1 s
			  m2 = findFunction i2 s
		      (e1, s1) = pop stack
			  (e2, e3, s2) = pop2 stack
	  

scrape :: [Char] -> [String]
scrape [] = []
scrape cs=:[c:_]
	| isSpace c = scrape (dropWhile isSpace cs)
	| otherwise = [toString word:scrape rest]
	where
    	(word, rest) = span (not o isSpace) cs
    
split :: String -> [String]
split s = scrape (fromString s)
    	
evaluate :: ([IFun1],[IFun2]) String -> Int
evaluate (iFun1, iFun2) str = top (process (iFun1, iFun2) (split str) Empty)
	where 
		process :: ([IFun1],[IFun2]) [String] (Stack Int) -> (Stack Int)
		process (iFun1, iFun2) [x:xs] stack
		 	| xs == [] = step (iFun1, iFun2) stack x
		 	| otherwise = process (iFun1, iFun2) xs (step (iFun1, iFun2) stack x)

IFun1s :: [IFun1]
IFun1s = [("abs",abs),("inc",(+) 1),("dec", (+) (-1))]

IFun2s :: [IFun2]
IFun2s = [("+",(+)),("*",(*)),("-",(-)),("/",(/)),("%",(rem))]
    	
rpn :: String -> Int
rpn str = evaluate (IFun1s, IFun2s) str

//1. Decimális számjegyek vizsgálata
//Start = all isDecimalDigit ['0'..'9']
//Start = all (not o isDecimalDigit) (['A'..'Z'] ++ ['a'..'z'] ++ fromString "{}$+!*/@")

//2. Elõjel-e?
//Start = all isSign ['+', '-']
//Start = all (not o isSign) (['0'..'9'] ++ fromString "{}$!*/@")

//3. Szöveg egész számmá alakíthatósága
//Start = [isInteger "", isInteger "123", isInteger "-123", isInteger "+123", isInteger "DEADBEEF", isInteger "12AF", isInteger "123.123"]

//4. Konvertálás egész számmá
//Start = [strToInteger "0", strToInteger "0042", strToInteger "123456789123456789", strToInteger "-123456789123456789", strToInteger "+123456789123456789"]
	
//5. A verem definíciója
//Start = push (push Empty "b") "a"
//Start = push (push (push (push (push (push (push (push (push (push Empty 10) 9) 8) 7) 6) 5) 4) 3) 2) 1 
//Start = push (push (push Empty "amaz") "az") "ez"

//6. Érték lerakása a verembe
//Start = push [] 1
//Start = push ['a','s','k'] '!'

//7. Érték kivétele a verembõl
//Start = pop ['a','s','k','!']
//Start = pop [1]

//8. A legutolsó két érték kivétele a verembõl
//Start = pop2 ['a','s','k','!']
//Start = pop2 [1,2]

//9. Keresés a függvények közt
//Start = (fromJust (findFunction [("~",~),("abs",abs)] "~")) (-9)
//Start = (fromJust (findFunction [("+",(+))] "+")) 1 2

//10. Elem feldolgozása verem használatával
//Start = step ([],[("+", (+))]) [1,2] "+"
//Start = step ([],[("+", (+))]) [1,2] "!"
//Start = step ([("~", ~)],[("+", (+))]) [1,2] "~"
//Start = step ([("-", ~)],[("-", (-))]) [1,2] "-"
//Start = step ([],[("+",(+))]) [1] "+"
//Start = step ([("-", ~)],[]) [] "-"

//11. Postfix kifejezés kiértékelése adott mûveletekkel
//Start = evaluate ([],[("+",(+)),("-",(-))]) "3 4 +"
//Start = evaluate ([("abs",abs)],[]) "-4 abs"
//Start = evaluate ([("abs",abs)],[("+",(+)),("-",(-))]) "3 -4 abs +"
//Start = evaluate ([],[("+",(+)),("*",(*)),("/",/)]) "10 2 * 3 4 + * 560 /"	

//12. Számológép szabványos mûveletekkel
//Start = rpn "3 4 +"
//Start = rpn "-4 abs"	
//Start = rpn "3 -4 abs +"
//Start = rpn "9 inc 2 * 3 5 dec + * 512 /"
//Start = rpn "9 5 * 7 + 10 %"









