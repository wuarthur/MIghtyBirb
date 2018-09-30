-- CPSC 312 - 2018 - Games in Haskell
--  Copyright David Poole 2018, released on GNU General Public License

module TwentyQs where
import System.IO
import Text.Read

---------------------
--fixdel is defined below
--remove once
remove [] = []
remove (h:t)
   | t == [] = []
   | head t == '\DEL' = tail t
   | otherwise = (h:(remove t))

--call remove till all delete is removed
removeAll [] = []
removeAll list
    | elem '\DEL' list = removeAll (remove list)
    | otherwise = list


fixdel iostr = removeAll (read ("\"" ++ iostr++"\"") :: String)

------------------------------------

data QATree = QLeaf String
            | QNode String QATree QATree
       deriving (Show)

initQATree = QNode "Is it living?"
                (QNode "Is it a person?"
                    (QLeaf "Justin Bieber")
                    (QLeaf "J-35, one of the southern resident killer whales"))
                (QNode "Is it a physical object?"
                    (QLeaf "Vancouver")
                    (QLeaf "CPSC 312"))

play :: QATree -> IO QATree
play tree =
   do
      putStrLn "Do you want to play 20 questions?"
      ans <- getLine
      if (( fixdel ans) `elem` ["y","yes","ye","oui"])
        then do
           putStrLn "Think of an entity"
           newtree <- askabout tree
           play newtree
        else return tree

askabout :: QATree -> IO QATree
askabout (QLeaf ans) =
  do
    putStrLn("Is it "++ans++"????")
    line <- getLine
    if (( fixdel line) `elem` ["y","yes","ye","oui"])
       then return (QLeaf ans)
       else do
          putStrLn("What were you thinking of?")
          newAns <-   getLine             --get new answer
          putStrLn("you said: "++ (fixdel newAns))
          putStrLn("Give a question")
          newQ <- getLine               --get new question
          putStrLn("you said: "++ (fixdel newQ))
          return (QNode (fixdel newQ)            --this added the new questions and include its answer
                      (QLeaf ( fixdel newAns))
                      (QLeaf ans))

askabout (QNode q yes no) =
  do
    putStrLn(q)
    line <- getLine
    if ( ( fixdel line) `elem` ["y","yes","ye","oui"])
       then do
            newyes <- askabout yes
            return (QNode q newyes no)
       else do
            newno <- askabout no
            return (QNode q yes newno)

go :: IO QATree
go = play initQATree
