module ReadmeGen() where

import System.Directory 
import Data.List (isSuffixOf, partition, sort)
import Data.Char

main :: IO()
main = do
    a <- writeRec "./imgs"
    writeFile "./README.md" a

writeRec :: String -> IO String
writeRec arq = do
    dirs <- listDirectory arq
    let (imgs', childs') = filterImgsDirs dirs
        childs = ((arq++"/")++) <$> sort childs'
        imgs = ((arq++"/")++) <$> sort imgs'
        acc = writeGroup arq imgs
    cRes <- sequence (writeRec <$> childs) 
    return (acc ++ foldl (++) [] cRes)  

writeGroup :: String -> [String] -> String
writeGroup gName imgs = 
    "#### " ++ gName ++ 
    "\n"++gName++"|----|-----\n" ++
    ":-----------:|:--:|:----:\n" ++
     concat (writeImgCell <$> sepGroups imgs) ++
    "\n\n"

sepGroups :: [String] -> [(String,String,String)]
sepGroups [] = []
sepGroups (x:[]) = [(x,"","")]
sepGroups (x:y:[]) = [(x,y,"")]
sepGroups (x:y:z:xs) = (x,y,z) : sepGroups xs

writeImgCell :: (String,String,String) -> String
writeImgCell (a1,a2,a3) = 
    rule a1 ++ " | " ++ rule a2 ++ " | " ++ rule a3 ++"\n" ++
    cell a1 ++ " | " ++ cell a2 ++ " | " ++ cell a3 ++"\n"
    where 
        rule = reverse . takeWhile (/= '/') . dropWhile (/= '.') . reverse
        cell x = "![]("++x++")"

filterImgsDirs :: [String] -> ([String],[String])
filterImgsDirs = partition isImg 
    where 
        isImg path = any (\s -> isSuffixOf s path) [".png", ".svg", ".jpeg", ".bmp"]