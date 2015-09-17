module AttoToParsec where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as AI
import qualified Text.Parsec as P
import Data.ByteString
import Data.Functor.Identity (Identity)

attoToParsec :: A.Parser a -> P.Parsec ByteString () a
attoToParsec atto = P.mkPT (convResult . parseState atto)

parseState :: A.Parser a -> P.State ByteString () -> A.Result a
parseState atto st = A.parse atto (P.stateInput st)

convResult :: A.Result r -> Identity (P.Consumed (Identity (P.Reply ByteString () r)))
convResult (A.Fail bs ctxs msg) = undefined
