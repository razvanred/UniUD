module ErrorCollector.Main where

import ErrorCollector.Collector (collect)
import ErrorCollector.ConvertForTAC

collectErrors alsoWarns block = out
    where
        out = case collect alsoWarns block of
            (Just we) -> Left we
            Nothing ->
                let t1 = convertForTAC block
                    t2 = inTreeToOut t1
                in  Right t2
