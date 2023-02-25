module MiniLang (
    someFunc,
) where
import Relude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Elementami podstawowej wersji języka Mini są następujące terminale:
-- - słowa kluczowe: program if else while read write return int double bool true false
-- - operatory i symbole specjalne: = || && | & == != > >= < <= + - * / ! ~ ( ) { } ;
-- - identyfikatory i liczby
