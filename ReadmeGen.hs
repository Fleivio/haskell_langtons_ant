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
    "\n<div float=\"left\">\n" ++
     concat (writeImgCell <$> imgs) ++
    "</div>\n\n"

writeImgCell :: String -> String
writeImgCell path = 
      "<p>\n"
    ++"<img src=\""++path++"\"width=\"200\"/>\n"
    ++"<span>"++rule++"</span>\n"
    ++"</p>\n"
    where rule = reverse $ takeWhile (/= '/') $ dropWhile (/= '.') $ reverse path

filterImgsDirs :: [String] -> ([String],[String])
filterImgsDirs = partition isImg 
    where 
        isImg path = any (\s -> isSuffixOf s path) [".png", ".svg", ".jpeg", ".bmp"]