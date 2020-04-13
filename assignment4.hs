maxlist :: [Int] -> Int
maxlist l = 
  let pmaxim :: [Int] -> Int 
      pmaxim []  = error "Empty list" 
      pmaxim [x]  = (x)
      pmaxim (x:xs)            
        | x > t     = (x)      
        | otherwise = (t)      
        where (t) = pmaxim xs 
  in pmaxim l                  
  
delete :: Int -> [a] -> [a]
delete _ []  = []
delete n (x:xs)  = delete' (x:xs) n 1 where
    delete' (x:xs) n i = (if (n `divides` i) then
        [] else
        [x])
        ++ (delete' xs n (i+1))
    delete' [] _ _ = []
    divides x y = y `mod` x == 0

isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs)
  where insert [] = [x]
        insert (y:ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys

rotate :: Int -> [a] -> [a]
rotate 0 y = y
rotate x y = rotate (x-1) (last y : init y)

single :: [Int] -> [[Int]]
single [] = []
single (x:xs) = [[x]] ++ single(xs)

double [] = []
double [x] = [x]
double (x:y:ys) = 2*x:y:double ys