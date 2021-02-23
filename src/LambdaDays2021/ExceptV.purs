module LambdaDays2021.ExceptV
       ( Payload
       , Id (..)
       , Event (..)
       , Validator (..)
       ) where

import Control.Applicative (pure)
import Control.Monad (class Monad)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either)
import Data.Symbol (SProxy (..))
import Data.Unit (Unit, unit)
import Data.Variant (Variant, inj)
import Type.Row (type (+))

data Payload
newtype Id a  = MkId String
newtype Event = MkEvent { eventId :: String, payload :: Payload }

newtype Host   = Host String
newtype Port   = Port Int
newtype DBName = DBName String

type ConnectionInfo
  = { host     :: Host
    , port     :: Port
    , database :: DBName
    }

foreign import data Connection :: Type

-- Algebra for the persistence API
newtype Persistor err (m :: Type -> Type) evt
  = MkPersistor
    { insertEvent :: Connection -> evt -> m (Id evt)
    , connect :: ConnectionInfo -> m (Either err Connection)
    }

-- Algebra for validating events
newtype Validator err (m :: Type -> Type) evt
  = MkValidator
    { validateEvent :: evt -> m (Either err evt)
    }

-- Algebra for checking and reading config
-- newtype Configurator err (m :: Type -> Type) varname
-- = MkConfigurator
-- { readEnvVar :: _
-- , validate :: _
-- }

data ConfigErr
  = UnsetConfig (Array String)
  | InvalidConfig (Array String)

data DBErr
  = DBConnectErr ConnectionInfo
  | DBQueryErr String (Array String)

data EventErr
  = EventErr Event (Array String)

type ConfigError r     = (configError     :: ConfigErr | r)
type DatabaseError r   = (databaseError   :: DBErr     | r)
type InvalidEvent r    = (invalidEvent    :: EventErr  | r)

databaseError :: forall r. DBErr -> Variant (DatabaseError + r)
databaseError = inj (SProxy :: SProxy "databaseError")

invalidEvent :: forall r. EventErr -> Variant (InvalidEvent + r)
invalidEvent = inj (SProxy :: SProxy "invalidEvent")

type ConsumerError r
  = ( DatabaseError
    + InvalidEvent
    + ConfigError
    + r
    )

