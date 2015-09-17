module AttoToParsec where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as AI
import qualified Text.Parsec as P
import Data.ByteString
import Data.Functor.Identity (Identity)

attoToParsec :: A.Parser a -> P.Parsec ByteString () a
attoToParsec atto = P.mkPT (convResult . stateResult atto)

convState :: P.State ByteString a -> AI.State ByteString
convState P.State { P.stateInput = s } = undefined

stateResult :: A.Parser a -> P.State ByteString () -> A.Result a
stateResult atto st = A.parse atto (_ st)

convResult :: A.Result r -> Identity (P.Consumed (Identity (P.Reply ByteString () r)))
convResult (A.Fail bs ctxs msg) = undefined
