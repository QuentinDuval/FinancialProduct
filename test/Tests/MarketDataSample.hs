module Tests.MarketDataSample (
    module Exports,
    mds,
    today,
    tomorrow,
) where

import Eval             as Exports
import TestMarketData   as Exports
import Utils.Time       as Exports


today, tomorrow :: FinDate
today    = fromGregorian 2015 22 21
tomorrow = addDay today 1

mds :: TestMarketData
mds = initMds
    [(Stock "GOLD"     , const 15.0)
    ,(Stock "SILV"     , const 11.0)
    ,(Stock "USD"      , const 1.0)
    ,(Stock "EUR"      , \t -> 1.1 + 0.01 * fromIntegral (t `diffDays` today) )]
    [(Rate "EURIBOR3M" , const 0.05)
    ,(Rate "LIBOR"     , const 0.06)]

