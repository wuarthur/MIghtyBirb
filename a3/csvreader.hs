splitsep :: (a -> Bool) -> [a] -> [[a]]

splitsep _ [] = [[]]
splitsep sep lst = (beforesep sep lst): splitsep sep (aftersep sep lst)
  where
      --beforesep sep lst gets all of the chars before the first instance of sep
      beforesep _ [] = []
      beforesep sep (h:t)
        | sep h = []
        | otherwise = h:beforesep sep t
      --aftersep sep lst gets all of the chars after the first instance of sep
      aftersep _ [] = []
      aftersep sep (h:t)
        | sep h = t
        | otherwise = aftersep sep t

{-
splitsep (\ x -> mod x 2 == 0) [1,3,4,5,7,9,8,8,55,45,48] => [[1,3],[5,7,9],[],[55,45],[]]
splitsep (==',') "comma,separated,list,as,in,a,csv,file" => ["comma","separated","list","as","in","a","csv","file",""]
-}

csvreader filename =
  do
    file <- readFile filename
    print [ splitsep (==',') x | x <- (splitsep (=='\n') file)]

{-
csvreader "test.csv" => [["Day","Month","Received","Sold",""],["12","May","200","20",""],["10","July","","23",""],[""]]
-}
