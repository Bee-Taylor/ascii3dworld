module GeneralUtility where


square :: Float -> Float
square x = x * x

--only used to fix rounding errors
approximatelyEquals :: Float -> Float -> Bool
approximatelyEquals a b
   | ((a-b < 0.1) && (b-a<0.1)) = True
   | otherwise = False

