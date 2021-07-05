import System.Environment
import Data.Char

chars :: [Char]       -- chars is a list that holds the characters 
chars =  ['0','1','2','3','4','5','6','7',
          '8','9','A','B','C','D','E','F']

d2c:: Int->Int->Char                -- takes base and value, returns coversion of the value in that base
d2c base c = char_repr chars base c -- char_repr is defined as locally
 where
  char_repr::[Char]->Int->Int->Char -- function iterates over the chars list and to find the suitable representation in the base given
  char_repr arr base index          
   |index > base   = error "invalid digit"  -- if the found index in the chars is greater than the base give error
   |index == base  = arr !!0                -- the case where base and index are equal 
   |otherwise      = arr !!index            -- otherwise return the chars's value at the index -> return chars[index]


c2d::Int->Char->Int                         -- converts character to digit
c2d base c = getIndex chars base 0 c        -- takes integer (base) and character to convert into that base
 where 
  getIndex::[Char]->Int->Int->Char->Int     -- getIndex is a function defined locally, iterates as "index" is the counter, returns integer
  getIndex [] _ _ _ = error "invalid digit"       -- if the char is not found give error
  getIndex (a:arr) _ index _
   |index > base         = error "invalid digit"  -- the number can not be represent in that base since it is bigger than the base value
   |(not (base ==16)) && (chars!!base)  == c  = 0 -- base 16 must be taken care of since it is the maximum base value 
   |(chars!!index) == c  = index                  -- if the input char is equal to the char at the chars[index] then return index
   |otherwise            = getIndex arr base (index+1) c -- otherwise recursively call and increment index value by 1 


n2l::Int->Int->[Int]                        -- converts a number into a list
n2l base target
 |target < base  = [target `mod` base]      -- if the target value is smaller than the base-> this is the last digit to add in the list 
 |otherwise      = n2l base (target `div` base) ++ [target `mod` base]  -- divide the target value by base and add it to the list recursively


l2n:: [Int] -> Int -> Int                      -- takes the integer list and the base 
l2n [] _       = 0                             -- if the list is empty
l2n arr base   = listIter (reverse arr) base 0 -- tail recursive 
 where 
  listIter::[Int] -> Int -> Int->Int           -- listIter takes an list, base and acc
  listIter [] _ _ = 0                          -- empty list return zero
  listIter (a:arr') base acc = a*(my_exp base acc) + listIter arr' base (acc+1)  -- compute the power of the index and sum recursively 
   where
    my_exp::Int->Int->Int                      -- my_exp returns the exponent of the given base  
    my_exp _ 0 = 1                             -- 0th base is 1
    my_exp x i = x*my_exp x (i-1)              -- multiply the value as the i goes 0

add:: Int->Int->Int -> IO()                    -- takes the base and two integers, outputs the results in the given formats
add base dc1 dc2 = do
    let rep1 = n2l base dc1                    -- first inputs list representation
    putStrLn (show rep1)                       -- output the representation

    let num_rep1 = [d2c base a | a<-rep1]      -- construct the string representation of num1 with given base
    putStrLn (show num_rep1)                   -- 

    let rep2 = n2l base dc2                    -- second inputs list representation
    putStrLn (show rep2)                       -- output the representation

    let num_rep2 = [d2c base a | a<-rep2]      -- construct the string representation of num2 with given base
    putStrLn (show num_rep2)                   -- output the string
      
    let result = n2l base (dc1 + dc2)          -- the convert the summation of the two operand into digit list 
    putStrLn (show result)                     -- 

    let result_rep2 = l2n result base          -- the digit list representation is converted into string
    putStrLn (show result_rep2)                -- 


listConstruct::[Char] ->[Int]                  -- converts the string into a digit list
listConstruct []  = []
listConstruct (a:ax') = (ord a -48):listConstruct ax' 

main:: IO()  --main function
main = do 
    input_line <- getArgs                        -- get arguments from commandline
    let x    = input_line!!1                     -- get the base
    let operation = head input_line              -- the operation type 
    let base  = l2n ([c2d 10 a | a<-x]) 10       -- base is converted to an integer from string type
    --let base = l2n (listConstruct x) 10

    if operation == "d2c" then do                -- operation type is digit to character
       let operand = l2n (listConstruct (input_line!!2)) 10   -- converting the operand to integer
       let result  = ("'") ++[(d2c base operand)]++ ("'")     -- call digit to character function
       putStrLn (result)

    else if operation == "c2d" then do           -- operation type is character to digit 
       let operand = input_line!!2               -- get the operand
       let result  = c2d base (operand!!0)       -- call c2d function to get the digit value for the given character in the base desired
       putStrLn (show result)

    else if operation == "n2l" then do           -- operation type is number to list
       let operand = l2n (listConstruct(input_line!!2)) 10 -- convert operand into digit list from string and list from number in (base 10)
       let result  = n2l base operand            -- convert the number into list with the input base value
       putStrLn (show result)

    else if operation == "l2n" then do           -- operation type is list to number
       let operand = [l2n(listConstruct a) 10 | a<-tail(tail input_line)] -- the digit list in input starts from the 2nd index of the args
       let result  = l2n operand base            -- convert list to number in the given base
       putStrLn (show result)
    
    else do
        let operand1 = l2n (listConstruct(input_line!!2)) 10 -- convert the first operand into base 10 integer
        let operand2 = l2n (listConstruct(input_line!!3)) 10 -- convert the second operand into base 10 integer
        add base operand1 operand2                           -- call add function with the parameters
    return()

