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
    [(Stock "GOLD"     , const (pure 15.0))
    ,(Stock "SILV"     , const (pure 11.0))
    ,(Stock "BNP"      , \t -> pure $ 100 + fromIntegral (t `diffDays` today))
    ,(Stock "BMW"      , \t -> if (t `diffDays` today > 3) then pure 100 else Nothing)
    ,(Stock "USD"      , const (pure 1.0))
    ,(Stock "EUR"      , \t -> pure $ 1.1 + 0.01 * fromIntegral (t `diffDays` today) )]
    [(Rate "EURIBOR3M" , const (pure 0.05))
    ,(Rate "LIBOR"     , const (pure 0.06))]

