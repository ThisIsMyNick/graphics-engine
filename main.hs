import Types
import Line
import Matrix
import Parser
import Display
import Control.Monad as M
import Control.Monad.State
import qualified Data.Vector as V

main :: IO ()
main = do
  parseFile "script.mdl"
