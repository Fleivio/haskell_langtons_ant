module ReadmeGen() where

import System.Directory 
import Data.List (isSuffixOf, partition)
import Data.Char

main :: IO()
main = do
    a <- writeRec "./imgs"
    writeFile "./README.md" a

writeRec :: String -> IO String
writeRec arq = do
    dirs <- listDirectory arq
    let (imgs', childs') = filterImgsDirs dirs
        childs = ((arq++"/")++) <$> childs'
        imgs = ((arq++"/")++) <$> imgs'
        acc = writeGroup arq imgs
    cRes <- sequence (writeRec <$> childs) 
    return (acc ++ foldl (++) [] cRes)  

writeGroup :: String -> [String] -> String
writeGroup gName imgs = 
    "#### " ++ gName ++ 
    "\n<p float=\"left\">" ++
    concat (writeImgCell <$> imgs) ++
    "</p>\n\n"

writeImgCell :: String -> String
writeImgCell path = 
    "<img src=\"" ++ path ++ "\" width=\"400\" />"
    -- ++ "<span>"++ rule ++"</span>\n</div>\n"
    -- where
    --     rule = filter isUpper . reverse . takeWhile (/= '/') $ dropWhile (/= '.') $ reverse path

filterImgsDirs :: [String] -> ([String],[String])
filterImgsDirs = partition isImg 
    where 
        isImg path = any (\s -> isSuffixOf s path) [".png", ".svg", ".jpeg", ".bmp"]