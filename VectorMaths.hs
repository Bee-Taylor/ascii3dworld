module VectorMaths
( getVectorLength
, areVectorsDirectionEqual
, getVectorElement
, addVectorElements
, addVectorToCoordinate
, dotProduct
, crossProduct
, findAngleBetweenVectors
, scaleVector
, vectorElementDifference
, reverseVector
, normaliseVector
, findVectorBetweenPoints)
where

import DataTypes
import GeneralUtility


getVectorLength :: Vector -> Float
getVectorLength vector = sqrt (square (getVectorElement vector 0) + square (getVectorElement vector 1) +
                               square (getVectorElement vector 2))

areVectorsDirectionEqual :: Vector -> Vector -> Bool
areVectorsDirectionEqual a b
   | (normaliseVector a) == (normaliseVector b) = True
   | (normaliseVector a) == (reverseVector (normaliseVector b)) = True
   | otherwise = False

getVectorElement :: Vector -> Int -> Float
getVectorElement x y
    | y <= 0    = fst (fst x)
    | y <= 1    = snd (fst x)
    | otherwise = snd x

addVectorElements :: Vector -> Vector -> Int -> Float
addVectorElements a b element = (getVectorElement a element) + (getVectorElement b element)


-- |"addVectorToCoordinate" returns the coordinate that results if you travel from a coordinate passed along the full length of a vector passed
addVectorToCoordinate :: Coordinate -> Vector -> Coordinate
addVectorToCoordinate coordinate vector = ((addVectorElements coordinate vector 0, addVectorElements coordinate vector 1),
                                            addVectorElements coordinate vector 2)

-- |"dotProduct" returns a floating point number which results from taking the dot product of two vectors
dotProduct :: Vector -> Vector -> Float
dotProduct x y = dotProducti x y 2

dotProducti :: Vector -> Vector -> Int  -> Float --should only be called from dotProduct
dotProducti w x y
    | y<=0      = (getVectorElement w y) * (getVectorElement x y)
    | otherwise = (dotProducti w x (y-1)) + ((getVectorElement w y) * (getVectorElement x y))

-- |"crossProduct" returns the vector resulting from taking a cross product of the two vectors passed in
crossProduct :: Vector -> Vector -> Vector
crossProduct a b = (((getVectorElement a 1) * (getVectorElement b 2) - (getVectorElement a 2) * (getVectorElement b 1) ,
                     (getVectorElement a 0) * (getVectorElement b 2) - (getVectorElement a 2) * (getVectorElement b 0)),
                     (getVectorElement a 0) * (getVectorElement b 1) - (getVectorElement a 1) * (getVectorElement b 0))

-- |"findAngleBetweenVectors" returns the angle between two vectors, in radians
findAngleBetweenVectorsi:: Vector -> Vector -> Float
findAngleBetweenVectorsi a b = (acos ((dotProduct a b) / ((getVectorLength a) *(getVectorLength b))))

findAngleBetweenVectors :: Vector -> Vector -> Float
findAngleBetweenVectors a b = 3.1415927 - findAngleBetweenVectorsi a b

-- |"scaleVector" takes a vector and multiplies each element by a float
scaleVector :: Vector -> Float -> Vector
scaleVector original scalar = (((getVectorElement original 0) * scalar, (getVectorElement original 1) * scalar),
                                (getVectorElement original 2) * scalar)

-- |"vectorElementDifference" finds the difference between two vector's elements at an indexed location
vectorElementDifference :: Vector -> Vector -> Int -> Float
vectorElementDifference a b comp = (getVectorElement a comp) -(getVectorElement b comp)

-- |"reverseVector" inverts the direction of the vector
reverseVector :: Vector -> Vector
reverseVector vector = ((- getVectorElement vector 0, - getVectorElement vector 1), - getVectorElement vector 2)

-- |"normaliseVector" takes a vector and returns a vector of unit length 1
normaliseVector :: Vector -> Vector
normaliseVector vector =  scaleVector vector (1/(getVectorLength vector))

-- |"findVectorBetweenPoints" returns the vector required to get from coordinate 1 to coordinate 2
findVectorBetweenPoints :: Coordinate -> Coordinate -> Vector
findVectorBetweenPoints a b = (((vectorElementDifference a b 0), (vectorElementDifference a b 1)),
                                (vectorElementDifference a b 2))
