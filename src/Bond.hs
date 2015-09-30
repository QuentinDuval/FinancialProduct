module Bond where


import Data.Time
import FinProduct
import Flow
import IndexMonad



-- | Test financial product to build bond products

type Nominal = Double
type Periods = [FlowDate]


create :: Nominal -> Periods -> Product
create nominal periods = undefined

