module Bond where


import FinProduct
import IndexMonad



-- | Test financial product to build bond products

type Nominal = Double
type Periods = Int


create :: Nominal -> Periods -> Product
create nominal periods = undefined

