module LambdaDays2021.FinalTagless where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Category ((<<<))
import Data.Either (Either)
import Data.Functor ((<$>))
import Data.Identity (Identity)
import Data.Unit (Unit, unit)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Nonbili.Postgres as Pg
import Data.Argonaut.Core (Json)

data DBErr cfg
  = DBConnectErr cfg
  | DBSQLErr String

newtype DBAlgebra
  (eff :: Type -> Type) -- the effectful context in which database actions are performed
  (conn :: Type)        -- the connection or pool type
  (cfg :: Type)         -- the configuration type
  (param :: Type)       -- the parameter type
  (result :: Type)      -- the result type
  = DBAlgebra
    { connect :: cfg -> eff (Either (DBErr cfg) conn)
    , insert_ :: conn -> String -> Array param -> eff (Either (DBErr cfg) Unit)
    , query   :: conn -> String -> Array param -> eff (Either (DBErr cfg) result)
    , close   :: conn -> eff Unit
    }

newtype LogAlgebra
  (eff :: Type -> Type)  -- the effectful context in which log actions are performed
  (msg :: Type)          -- the message type
  = LogAlgebra
    { log :: msg -> eff Unit
    }

-- implementations for unit testing
pureLogger :: LogAlgebra Identity String
pureLogger
  = LogAlgebra { log: \_ -> pure unit }

pureDB :: DBAlgebra Identity Unit Unit Unit Unit
pureDB
  = DBAlgebra { connect: \cfg -> pure (pure unit)
              , insert_: \conn cmd params -> pure (pure unit)
              , query: \conn query params -> pure (pure unit)
              , close: \conn -> pure unit
              }

-- implementations as a first draft of real operational usage
lambdaCloudwatchLogger :: LogAlgebra Aff String
lambdaCloudwatchLogger = LogAlgebra { log: \msg -> liftEffect (Console.log msg)
                                    }

lambdaPostgresDB :: DBAlgebra Aff Pg.Pool Pg.Config (Array Json) (Pg.Result String)
lambdaPostgresDB = DBAlgebra { connect: \cfg -> liftEffect (Pg.newPool cfg) >>= (pure <<< pure)
                             , insert_: \pool cmd params -> Pg.withTransaction pool \client -> Pg.execute client cmd params >>= (pure <<< pure)
                             , query: \pool query params -> Pg.withTransaction pool \client -> Pg.query client query params >>= (pure <<< pure)
                             , close: \pool -> pure unit
                             }
