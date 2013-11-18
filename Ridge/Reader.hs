module Ridge.Reader where
import Control.Monad
import qualified Data.EDN as EDN
import qualified Data.ByteString.Lazy as BL
import Ridge.Types

readString :: BL.ByteString -> Maybe Object
readString = EDN.parseMaybe >=> (return . toObject)
