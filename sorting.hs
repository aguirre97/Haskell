-- Alexandra Aguirre
-- COP 4020
-- Assignment 3 - Sorting Functions
-- 1/30/18


-- these sorting functions work for Integers and others

-------------------------------------------------------------

-- insertion sort

insertionSort :: Ord a => [a] -> [a]
-- if its empty, return empty
insertionSort [] = []
-- only one element in the list, return that element
insertionSort [x] = [x]

insertionSort (x:xs) = inserted ( insertionSort xs)
    where inserted [] = [x]
          inserted (y:ys)
              | x < y = x : y : ys
              | otherwise = y : inserted ys

-------------------------------------------------------------

-- bubble sort

bubbleSort :: (Ord a) => [a] -> [a]
-- if its empty, return empty
bubbleSort [] = []
-- only one element in the list, return that element
bubbleSort [x] = [x]

bubbleSort (x:y:xs) = if sorted compare' then compare' else bubbleSort compare'
    where compare' = (min x y) : bubbleSort ((max x y):xs)

-- helper function used to see if the list is sorted or not 
sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

-------------------------------------------------------------

-- selection sort

selectionSort :: (Ord a) => [a] -> [a]
-- if its empty, return empty
selectionSort [] = []
-- only one element in the list, return that element
selectionSort [x] = [x]

selectionSort xs = let x = maximum xs in selectionSort (remove x xs) ++ [x] 
  where remove _ [] = []
        remove y (x:xs)
          | x == y = xs
          | otherwise = x : remove y xs