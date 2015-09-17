module Attoparsec.Adapters.Parsec where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as AI
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Pos as P
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Cont

attoToParsec :: A.Parser a -> P.Parsec ByteString () a
attoToParsec atto = P.mkPT (return . fmap return . convResult . parseState atto)

parseState :: A.Parser a -> P.State ByteString () -> A.Result a
parseState atto st = A.parse atto (P.stateInput st)

convResult :: A.Result r -> P.Consumed (P.Reply ByteString () r)
convResult (A.Fail bs ctxs msg) = P.Consumed (P.Error (simpleParseError msg))
convResult (A.Partial cont) = error "Can't convert incremental parsers"
convResult (A.Done leftovers r) = P.Consumed (P.Ok r (mkState leftovers)
                                                     (simpleParseError "ok"))

mkState :: a -> P.State a ()
mkState a = P.State { P.stateInput = a
                    , P.statePos = P.newPos "too lazy right now" 0 0
                    , P.stateUser = ()
                    }

simpleParseError :: String -> P.ParseError
simpleParseError str = P.newErrorMessage (P.Message str)
                                         (P.newPos "internal attoparsec parser" 0 0)
