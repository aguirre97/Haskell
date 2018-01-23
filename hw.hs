-- Alexandra Aguirre
-- COP 4020 Spring 2018
-- Assignment 1
-- 1/13/18
   
-- First say hello and give your name.

-- Use the following lists:
-- let colors = ["Red","Green","Blue","White","Black","Yellow","Magenta","Cyan","Gray","Salmon","Purple","Lime"]
-- let months = ["January","February","March","April","May","June","July","August","September","October","November","December"]

-- Now combine these lists into a list of tuples, each with a color and a month.
--Find the first and last tuple of the list
--Find the third tuple of the list
--Reverse the list

------------------------------------------------

-- given list of colors and months
colors = ["Red","Green","Blue","White","Black","Yellow","Magenta","Cyan","Gray","Salmon","Purple","Lime"]
months = ["January","February","March","April","May","June","July","August","September","October","November","December"]

tuplesFunc = zip colors months

firstTuple = head tuplesFunc   

lastTuple = last tuplesFunc

thirdTuple = tuplesFunc !! 2

reverseList = reverse tuplesFunc

main = do 
    putStrLn "Hello, my name is Alexandra Aguirre"
    putStrLn "List of Tuples"
    print (tuplesFunc)
    putStrLn "First Tuple of the Set"
    print (firstTuple)
    putStrLn "Last Tuple of the Set"
    print (lastTuple)
    putStrLn "Third Tuple of the Set"
    print (thirdTuple)
    putStrLn "Reverse the List"
    print (reverseList)