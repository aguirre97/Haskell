-- Alexandra Aguirre
-- Assignment 2
-- Haskell Math Functions
-- COP 4020 Spring 2018
-- 1/19/18

-- Create two functions: one creates a random list of integers (which you should simply hard-code), 
-- and the other creates a random list (which you should hard-code) of floating point numbers.

-- Create the following functions: from a randomized list find the mean, medium, max, min, standard deviation 
-- (Do not use any built in/library functions that perform those specific functions)

----------------------------------------------

-- random list of ints
listy = [1, 5, 2, 4, 3]

-- random list of floats
listyFloat = [1.3, 5.6, 4.3, 2.2, 3.8]

-- get the length and sum of the list
-- this will be used for the working out the mean
lenInt = length listy
sumInt = sum listy
lenFloat = length listyFloat
sumFloat = sum listyFloat

-- sorting function done with quicksort
-- will be used to find the min and max
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- sort the lists
sortedInt = quicksort listy
sortedFloat = quicksort listyFloat

-- max of both lists
maxInt = last sortedInt
maxFloat = last sortedFloat

-- min of both lists
minInt = head sortedInt
minFloat = head sortedFloat

-- need to convert the length of the float list to a 'float' number
-- in order to do the mean calculation
floatFromEnum :: (Enum a) => a -> Float
floatFromEnum = fromIntegral . fromEnum 
floatyLen = floatFromEnum lenFloat

-- mean of the integer list
meanInt = sumInt `div` lenInt

-- mean of the float list
meanFloat a b = a / b
floatListMean = meanFloat sumFloat floatyLen

--medium of lists
medInt = sortedInt !! (lenInt `div` 2)
medFloat = sortedFloat !! (lenFloat `div` 2)


-- below are the operations to find the Standard Deviation

-- part 1: subtract the mean from the list of numbers and square the result
squareInt = [(x - meanInt) * (x - meanInt) | x <- listy]
squareFloat = [(x `subtract` floatListMean) * (x `subtract` floatListMean) | x <- listyFloat]

-- part 2: work out the mean for the squared differences
sumSquareInt = sum squareInt
meanSquareInt = sumSquareInt `div` lenInt

sumSquareFloat = sum squareFloat
meanSquareFloat = sumSquareFloat / floatyLen


-- part 3: take the square root of the mean
stdDevInt = sqrt(fromIntegral meanSquareInt)
stdDevFloat = sqrt(meanSquareFloat)

-------------------------------------------------------------

-- main function that prints all of the required math algorithms

main = do 
    putStr "Random list of integers: " 
    print (listy)
    putStrLn ""
    putStr "Random list of floats: "
    print (listyFloat)
    putStrLn ""
    putStr "Mean of integers: "
    print (meanInt)
    putStrLn ""
    putStr "Mean of floats: "
    print (floatListMean)
    putStrLn ""
    putStr "Medium of integers: "
    print (medInt)
    putStrLn ""
    putStr "Medium of floats: "
    print (medFloat)
    putStrLn ""
    putStr "Max of integers: "
    print (maxInt)
    putStrLn ""
    putStr "Max of floats: "
    print (maxFloat)
    putStrLn ""
    putStr "Min of integers: "
    print (minInt)
    putStrLn ""
    putStr "Min of floats: "
    print (minFloat)
    putStrLn ""
    putStr "Standard Deviation of integers: "
    print (stdDevInt)
    putStrLn ""
    putStr "Standard Deviation of floats: "
    print (stdDevFloat)
    putStrLn ""