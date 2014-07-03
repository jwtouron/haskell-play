import Data.List

perms :: [[Int]]
perms = sort $ permutations [0..9]

main = print $ perms !! 999999
