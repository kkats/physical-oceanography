{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where
---
--- | Generate module OceanographicToolbox from
---   gsw_oceanographic_toolbox.f90
---
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, put, get)
import Data.Char (isSpace, toLower)
import Data.List (find, isPrefixOf)
import Data.UnixTime
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr)
import System.IO
import System.Environment (getArgs, getProgName)
import Text.Printf

data Type    = TBD | Double | DoubleArray | Int | IntArray
                                                deriving (Show, Eq)

data VarInfo = VarInfo { inFunc   :: Bool,
                         funcName :: String,  -- if function
                         subName  :: String,  -- if subroutine
                         args     :: [(String, Type)],
                         outputh  :: Handle
                       }

type VarState = StateT VarInfo IO

foreign import ccall "size_r14_" size_r14 :: Ptr Int -> Ptr Int -> IO ()

main :: IO ()
main = do

    args <- getArgs

    when (length args /= 1) $ getProgName >>= \p -> error ("Usage: " ++ p ++ " src.f90")

    let src = head args
    -- check if r14 and DP are equivallent
    (sizeR14, sizeDP) <- alloca $ \xp ->
                            alloca $ \yp ->
                                size_r14 xp yp >> (,) <$> peek xp <*> peek yp
    when (sizeR14 /= sizeDP) $ error "r14 and double precision are different"

    bracket (open src) close work
 where
    open src = do
        h <- openFile src ReadMode
        hSetNewlineMode h universalNewlineMode -- handle '\r\n'
        g <- openFile "GSWtools.hs" WriteMode
        outPreamble g
        return (h,g)

    close (h,g) = hClose h >> hClose g

    work (h,g) = do
        ls   <- lines `fmap` hGetContents h
        info <- execStateT (mapM_ parseLine ls) (VarInfo False "" "" [] g)
        outPut info


parseLine :: String -> VarState ()
parseLine []      = return ()
parseLine l@(c:_) = if c == '!'        -- comment
                      then return ()
                      else do
  let ws = words l
  if null ws
    then return ()
    else do
      now <- get
      if (ws !! 0) == "function" || (ws !! 0) == "subroutine"      -- new function/subroutine
        then do
            when (inFunc now) $ liftIO (outPut now) -- finish previous function

            let (fsname, as) = fdec $ tail ws -- brand new
            -- use lower case only
            -- first item is the function/subroutine name
            if (ws !! 0) == "function"
              then put $ VarInfo True fsname ""  -- function
                       ((map toLower fsname,TBD):(map (\a -> (map toLower a,TBD)) as)) (outputh now)
              else put $ VarInfo True "" fsname -- subroutine
                       ((map toLower fsname,TBD):(map (\a -> (map toLower a,TBD)) as)) (outputh now)
        else put (vdec l now) -- otherwise

-- possible variable declaration line
vdec :: String -> VarInfo -> VarInfo
vdec xs info
    = let (lhs,rhs') = span (/= ':') $ skipSpace xs
          (r0, rhs)  = span (== ':') rhs'
       in if r0 /= "::"
            then info -- not a variable declaration line
            else let t  = extractType $ commaToList lhs
                     vs = extractVar  $ commaToList rhs
                     as = map (\a -> if fst a `elem` vs && snd a == TBD
                                       then (fst a,t)
                                       else a) $ args info
                  in info {args = as} -- replace only args

extractType :: [String] -> Type
extractType xs = if "real(r14)" `elem` xs
                 then case find (isPrefixOf "dimension") xs of
                        Nothing -> Double
                        Just _  -> DoubleArray
                 else if "integer" `elem` xs
                 then case find (isPrefixOf "dimension") xs of
                        Nothing -> Int
                        Just _  -> IntArray
                 else error $ "extractType: unrecognized " ++ (show xs)

extractVar :: [String] -> [String]
extractVar = map (map toLower . fst . span (/= '='))

-- function definition line
fdec :: [String] -> (String, [String])
fdec xs = let (fn,arglist) = span (/= '(')
                                $ skipSpace $ unwords xs
           in (fn,commaToList $ (init . tail) arglist)

--- misc.
-- comma separated -> list
-- the order must be preserved
commaToList :: String -> [String]
commaToList xs = let (a0,b0) = span (/= ',') xs
                  in if a0 == ""
                       then []
                       else let (_,b1) = span (==',') b0
                             in a0:(commaToList b1)

skipSpace :: String -> String
skipSpace xs = let (a0,b0) = span (not . isSpace) xs
                in if b0 == ""
                     then a0
                     else let (_,b1) = span (isSpace) b0
                           in a0++(skipSpace b1)


-- output
outPreamble :: Handle -> IO ()
outPreamble g = do
    hPutStrLn g "{-# LANGUAGE ForeignFunctionInterface #-}"
    hPutStr g "-- Automatically generated by Gentoolbox.hs at "
    getUnixTime >>= formatUnixTime mailDateFormat >>= hPutStrLn g . show
    hPutStrLn g "module Oceanogr.GSWtools where"
    hPutStrLn g ""
    hPutStrLn g "import Oceanogr.GSWcaller"
    hPutStrLn g ""
    hPutStrLn g "import Foreign.Ptr (Ptr)"
    hPutStrLn g "import Foreign.C.Types"
    hPutStrLn g ""
    return ()

outPut :: VarInfo -> IO ()
outPut info = do
    let VarInfo _ fname sname as g = info

    hPrintf g "---\n"
    if null fname && (not . null) sname
    --
    -- subroutine
    --
    then do
      hPrintf g "---\n"
      hPrintf g "--- XXX PLEASE EDIT XXX\n"
      hPrintf g "---\n"
      hPrintf g "%s ::  " sname
      mapM_ (\(a,t) -> case t of
                         Double      -> hPutStrLn g $ "        -> Double   -- ^ " ++ a
                         DoubleArray -> hPutStrLn g $ "        -> [Double] -- ^ " ++ a
                         Int         -> hPutStrLn g $ "        -> Int      -- ^ " ++ a
                         IntArray    -> hPutStrLn g $ "        -> [Int]    -- ^ " ++ a
                         _           -> error "impossible") (tail as)
      hPutStrLn g "        -> IO ()"
      hPrintf g "%s " sname
      mapM_ (\(a,_) -> hPrintf g "%s " a) (tail as)
      hPutStrLn g " = do"
      mapM_ (\(a,t) -> case t of
                         DoubleArray ->
                             hPrintf g "    -- %sp <- mallocArray n? :: IO (Ptr CDouble)\n" a
                         IntArray ->
                             hPrintf g "    -- %sp <- mallocArray n? :: IO (Ptr CInt)\n" a
                         _ -> return ()) (tail as)
      mapM_ (\(a,t) -> case t of
                         DoubleArray ->
                             hPrintf g "    withArray (map realToFrac %s) (\\%sp ->\n" a a
                         IntArray    ->
                             hPrintf g "    withArray (map fromIntegral %s) (\\%sp ->\n" a a
                         Double      ->
                             hPrintf g "    alloca (\\%sp -> poke %sp (realToFrac %s) >> \n" a a a
                         Int         ->
                             hPrintf g "    alloca (\\%sp -> poke %sp (fromIntegral %s) >> \n" a a a
                         _           -> error "impossible") (tail as)
      hPrintf g "    %s_ " sname
      mapM_ (\(a,_) -> hPrintf g "%sp " a) (tail as)
      hPrintf g "\n    "
      mapM_ (\(_,t) -> hPrintf g ")") (tail as)
      mapM_ (\(a,t) -> case t of
                         DoubleArray ->
                            hPrintf g "\n    -- %s' <- (map realToFrac) `fmap` peekArray n? %sp\n    free %sp\n" a a a
                         IntArray ->
                            hPrintf g "\n    -- %s <- (map fromIntegral) `fmap` peekArray n? %sp\n    free %sp\n" a a a
                         _ -> return ()) (tail as)
      hPrintf g "    -- return ("
      mapM_ (\(a,_) -> hPrintf g "%s'," a) (tail as)
      hPrintf g ")\n"
      hPrintf g "\nforeign import ccall \"%s_\" %s_ :: " sname sname
      mapM_ (\(a,t) -> hPrintf g "Ptr C%s -> " (case t of
                                                  Double -> "Double"
                                                  DoubleArray -> "Double"
                                                  Int -> "Int"
                                                  IntArray -> "Int")) (tail as)
      hPrintf g "IO ()\n\n"
      hPrintf stderr "Please hand-edit the subroutine in GSWtools :%s\n" sname
    --
    -- function
    --
    else do
      when (not $ null sname && (not . null) fname) $ error "not function, not subroutine?"
      hPrintf g "%s :: " fname
      mapM_ (\(_,t) -> hPrintf g "%s -> " (show t)) (tail as)
      hPrintf g "IO %s\n" (show . snd . head $ as)

      let narg = length as - 1
      hPrintf g "%s " fname
      mapM_ (\(a,_) -> hPrintf g "%s " a) (tail as)
      hPutStrLn g " ="
      hPrintf g "    realToFrac `fmap` with%dArgs (map realToFrac [" narg
      if length as > 2
      then mapM_ (\(a,_) -> hPrintf g "%s," a) (init . tail $ as) >> hPrintf g "%s" (fst . last $ as)
      else hPrintf g "%s" (fst . last $ as)
      hPrintf g "]) [] %s_\n\n" fname
      hPrintf g "foreign import ccall \"%s_\" %s_ :: " fname fname
      mapM_ (\(a,t) -> hPrintf g "Ptr C%s -> " (show t)) (tail as)
      hPrintf g "IO C%s\n\n" (show . snd . head $ as)
