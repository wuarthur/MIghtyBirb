module Csvreader where

splitsep :: (a -> Bool) -> [a] -> [[a]]
-- splitsep sep lst splits a list into a list of lists by sep
splitsep _ [] = [[]]
splitsep sep lst = if length (beforesep sep lst) == length lst then [lst] else (beforesep sep lst):splitsep sep (aftersep sep lst)
  where
    --beforesep sep lst gets all of the chars before the first instance of sep
    beforesep _ [] = []
    beforesep sep (h:t)
      | sep h = []
      | otherwise = h:beforesep sep t
    --aftersep sep lst excludes all of the chars after the first instance of sep
    aftersep _ [] = []
    aftersep sep (h:t)
       | sep h = t
       | otherwise = aftersep sep t

{-
splitsep (==',') "comma,separated,list,as,in,a,csv,file" => ["comma","separated","list","as","in","a","csv","file"]
splitsep (==',') "csv,,with,missing,elts,,," => ["csv","","with","missing","elts","","",""]
splitsep (`elem` " ,.?!") "What? is this thing? ... called Love." => ["What","","is","this","thing","","","","","","called","Love",""]
splitsep (==',') [] => [""]
splitsep (\ x -> mod x 2 == 0) [1,3,4,5,7,9,8,8,55,45,48] => [[1,3],[5,7,9],[],[55,45],[]]
-}

csvreader :: FilePath -> IO ()
-- csvreader filename reads a file and parses it with splitsep
csvreader filename =
  do
    file <- readFile filename
    print [ splitsep (==',') x | x <- (splitsep (=='\n') file)]

{-
csvreader "test.csv" => [["Day","Month","Received","Sold"],["12","May","200","20"],["10","July","","23"],[""]]
-}
