module basicCellAutomata

import StdEnv

:: Cell :== Int
:: Alive :== Bool
:: Interval :== (Int, Int)
:: Neighbourhood :== (Alive, Alive, Alive)

initial :: Cell -> Alive
initial x 
		| x == 0 = True
		| otherwise = False

takeInterval :: Interval (Cell -> Alive) -> [Alive]
takeInterval (x, y) f = map f [x..y] 

rule51 :: Neighbourhood -> Alive
rule51 (_, m, _) = not m

rule150 :: Neighbourhood -> Alive
rule150 (l, m, r) 
		| l == True && m == False && r == False = True
		| l == False && m == True && r == False = True
		| l == False && m == False && r == True = True
		| l == True && m == True && r == True = True
		| otherwise = False
		
:: State :== (Cell -> Alive)

step :: (Neighbourhood -> Alive) State -> State
step rule state = (\x -> rule ((state (x - 1)), (state x), (state (x + 1))))		

run :: (Neighbourhood -> Alive) State -> [State]
run rule state = [state] ++ run rule (step rule state)

displayCell :: Bool -> Char
displayCell x
	| x = '#' 
	| otherwise = '_'

displayState :: Interval State -> String
displayState (x, y) state = toString (map (displayCell o state) [x..y])

display :: Interval Int [State] -> String
display (x, y) steps f = concat (map (\z ->  toString(displayState (x, y) z)) (take steps  f))
						where
							concat :: [String] -> String 
							concat [x:xs]
									| xs == [] = (toString x)
									| otherwise = (toString x) +++ "\n" +++ concat xs
																			
//1. Kezdõállapot
//Start = initial 0
//Start = initial 3
//Start = initial (-2)

//2. A vonal egy szeletének elemzése
//Start = takeInterval (-3,3) initial
//Start = takeInterval (0,0) initial
//Start = takeInterval (0,-3) initial

//3. Triviális automata definiálása
//Start = rule51 (False,False,False)
//Start = rule51 (False,True,False)
//Start = rule51 (True,False,True)
//Start = rule51 (True,True,True)

//4. Egy automata definiálása
//Start = rule150 (False,False,False)
//Start = rule150 (False,True,False)
//Start = rule150 (True,False,False)
//Start = rule150 (True,False,True)		
//Start = rule150 (True,True,True)

//5. Az automata léptetése
//Start = step rule150 initial 0
//Start = step rule150 initial 1
//Start = step rule150 initial 2
//Start = map (step rule51 initial) [-3..2] 

//6. Az automata futtatása
//Start = map (\x -> map x [0..5]) (take 10 (run rule150 initial))
//Start = map (\x -> map x [0..5]) (take 10 (run rule51 initial))

//7. Egy cella megjelenítése
//Start = displayCell True
//Start = displayCell False

//8. Egy állapot megjelenítése
//Start = displayState (0,6) initial
//Start = displayState (-3,3) initial
//Start = displayState (3,6) (step rule51 initial)
//Start = displayState (3,3) (step rule150 initial)

//9. Állapotsorozat megjelenítése
//Start = display (3,3) 5 (run rule51 initial)
//Start = display (3,3) 5 (run rule150 initial)

