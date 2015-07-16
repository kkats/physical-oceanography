module Main where
import Oceanogr.GSW
import Oceanogr.GSWtools

main :: IO ()
main = do
    -- toolbox
    -- should be 35.5010
    sp <- gsw_sp_from_c 43.6 15 300
    print sp
    print "should be 35.5010"   
