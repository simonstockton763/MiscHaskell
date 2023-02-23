--Simon Stockton  

third (a,b,c) = c


reverseList [a] = [a]
reverseList (x:xs) =  reverseList xs ++ [x]


member x [] = False
member x (hd : tl)
  | x == hd  = True
  |otherwise = member x tl


union [] (lst) =lst
union (hd:lst1) (lst2)
  | member hd lst2 = union lst1 lst2
  | otherwise =  hd : (union lst1 lst2) 


intersection (lst) []  = []
intersection [] (lst) = []
intersection (hd:lst1) (lst2)
  | member hd lst2 = hd : (intersection lst1 lst2)
  | otherwise = intersection lst1 lst2


partition x [] = ([],[])
partition x (hd:lst)
  | hd < x = (hd :  fst (partition x lst),  snd (partition x lst))
  | otherwise =  (fst (partition x lst) , hd : snd (partition x lst))


quicksort [] = []
quicksort (hd:lst) = quicksort (fst (partition hd lst)) ++ [hd] ++ quicksort (snd (partition hd lst))

getbias [a,b]= b-a
getbias (frst:scnd:lst)= scnd - frst  + (getbias lst)

minimizeBias ratings = getbias (quicksort ratings)

