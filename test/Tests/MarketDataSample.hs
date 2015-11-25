module Tests.MarketDataSample (
    module Exports,
    mds,
) where

import Eval             as Exports
import TestMarketData   as Exports
import Utils.Time       as Exports



mds :: TestMarketData
mds = initMds
    [(Stock "GOLD"     , const 15.0)
    ,(Stock "SILV"     , const 11.0)
    ,(Stock "USD"      , const 1.0)
    ,(Stock "EUR"      , \t -> 1.1 + 0.1 * sin (toDayCount t) )]
    [(Rate "EURIBOR3M" , const 0.05)
    ,(Rate "LIBOR"     , const 0.06)]

