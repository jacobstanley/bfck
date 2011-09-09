{-# LANGUAGE TypeSynonymInstances #-}

-- import Control.Monad.State
import Data.Monoid
import Data.Char (ord)
import Prelude hiding (Left, Right)

main :: IO ()
main = putStrLn $ translate $ test2

test :: Bfck
test = do
    push 100
    push 20
    add

test2 :: Bfck
test2 = puts "Hello World!\n"

------------------------------------------------------------------------

push :: Int -> Bfck
push x = left >> set x

pop :: Bfck
pop = right

add :: Bfck
add = do
  loop $ do
    dec
    right
    inc
    left
  right

------------------------------------------------------------------------

memclr :: Int -> Int -> Bfck
memclr dst len = do
    jump dst
    replicateB len (clear >> right)

cpy :: Int -> Int -> Bfck
cpy src dst = do
    jump src
    loop $ do
       dec
       jump dst
       inc
       jump eax
       inc
    mov eax src

mov :: Int -> Int -> Bfck
mov src dst = do
    jump src
    while
    dec
    jump dst
    inc
    end

eax :: Int
eax = (-1)

ebx :: Int
ebx = (-2)

------------------------------------------------------------------------

loop :: Bfck -> Bfck
loop b = while >> b >> end

set :: Int -> Bfck
set = (clear >>) . incN

clear :: Bfck
clear = while >> dec >> end

replicateB :: Int -> Bfck -> Bfck
replicateB n (Bfck b) = Bfck $ concat $ replicate n b

incN :: Int -> Bfck
incN n = replicateB n inc

decN :: Int -> Bfck
decN n = replicateB n dec

incC :: Char -> Bfck
incC = incN . ord

decC :: Char -> Bfck
decC = decN . ord

setS :: String -> Bfck
setS = mconcat . map go
  where
    go :: Char -> Bfck
    go c = incC c >> right

putchar :: Char -> Bfck
putchar c = incC c >> put >> decC c

puts :: String -> Bfck
puts = mconcat . map putchar

------------------------------------------------------------------------
-- Base Language

instance Monoid a => Monoid (BfckM a) where
    mempty                   = Bfck []
    mappend (Bfck x) (Bfck y) = Bfck (x ++ y)

instance Monad BfckM where
    return _             = Bfck []
    (Bfck x) >> (Bfck y) = Bfck (x ++ y)
    x >>= f              = x >> f (error "BfckM: invalid use of monadic bind")

stmts :: Bfck -> [Stmt]
stmts (Bfck xs) = xs

type Bfck = BfckM ()
data BfckM a = Bfck [Stmt]
    deriving Show

data Stmt
    = Left  | Right | Jump Int
    | Inc   | Dec
    | Put   | Get
    | While | End
    deriving Show

left :: Bfck
left = Bfck [Left]

right :: Bfck
right = Bfck [Right]

jump :: Int -> Bfck
jump x = Bfck [Jump x]

inc :: Bfck
inc = Bfck [Inc]

dec :: Bfck
dec = Bfck [Dec]

put :: Bfck
put = Bfck [Put]

get :: Bfck
get = Bfck [Get]

while :: Bfck
while = Bfck [While]

end :: Bfck
end = Bfck [End]

------------------------------------------------------------------------
-- C Printer

translate :: Bfck -> String
translate b = unlines
    [ "#define MEMSIZE 1024"
    , "int main() {"
    , "int i;"
    , "char mem[MEMSIZE];"
    , "char *ptr = &mem[512];"
    , "for (i = 0; i < MEMSIZE; i++) {"
    , "  mem[i] = 0;"
    , "}"
    , code b
    , "return 0;"
    , "}"
    ]
  where
    code = concatMap translateS . normalize . stmts

translateS :: Stmt -> String
translateS Left  = "++ptr;"
translateS Right = "--ptr;"
translateS Inc   = "++*ptr;"
translateS Dec   = "--*ptr;"
translateS Put   = "putchar(*ptr);"
translateS Get   = "*ptr=getchar();"
translateS While = "while(*ptr){"
translateS End   = "}"
translateS x     = error $ "translateS: cannot print " ++ show x

------------------------------------------------------------------------
-- Pretty Printer

pretty :: Bfck -> String
pretty = map prettyS . normalize . stmts

prettyS :: Stmt -> Char
prettyS Left  = '<'
prettyS Right = '>'
prettyS Inc   = '+'
prettyS Dec   = '-'
prettyS Put   = '.'
prettyS Get   = ','
prettyS While = '['
prettyS End   = ']'
prettyS x     = error $ "prettyS: cannot print " ++ show x

normalize :: [Stmt] -> [Stmt]
normalize = fld 0
  where
    fld :: Int -> [Stmt] -> [Stmt]
    fld _ []     = []
    fld p (x:xs) = x' ++ fld p' xs
      where
        (p', x') = go p x

    go :: Int -> Stmt -> (Int, [Stmt])
    go p Left     = (p - 1, [Left])
    go p Right    = (p + 1, [Right])
    go p (Jump n) = (n, shift $ n - p)
    go p x        = (p, [x])

    shift 0         = []
    shift x | x > 0 = stmts $ replicateB   x  right
            | x < 0 = stmts $ replicateB (-x) left
