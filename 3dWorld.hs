{-
Module : 3dworld
Description : a basic system for maintaining a 3d world, including renderring and eventually moving objects

Maintainer : bren96.bt@gmail.com
Stability : Experimental
Portability : non-portable (only been made and tested on one ubuntu machine)
-}

import Data.List
type Coordinate   = ((Float, Float), Float)  -- ((x,y),z)
type Vector       = Coordinate   -- ((Δx,Δy),Δz), functionally no different to coordinate, just a direction
type Plane        = (Coordinate, Vector) --point on plane and normal
type Coefficients = (Float, (Float, Float)) -- reflectivity to ambient, diffuse and specular light
type Face         = ([Coordinate],  Coefficients) -- (a list of the vertices around the face, and the coefficients associated with the material)
type FaceList     = [Face]  
type LightSource  = (Coordinate, (Float, (Vector, (Float, Float)))) -- (intensity, (direction, (angle reached, gradient)))
--gradient - the light intensity outside the central cone (for each degree away from the cone, it subtracts the gradient from the intensity of the intensite one degree closer to the cone). If the gradient is 0 then the light source is constant in each direction.
type Lighting    = ([LightSource], Float) -- a list of all the light sources followed by a float storing the ambient lighting
type World       = (Lighting, FaceList) -- the whole world being renderred, with all the faces and all the information required to render each face.

square :: Float -> Float
square x = x * x

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
findAngleBetweenVectors:: Vector -> Vector -> Float
findAngleBetweenVectors a b = (acos ((dotProduct a b) / ((getVectorLength a) *(getVectorLength b))))

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

-- |"findAngleSubtendedByPoints" finds the angle between 3 points in 3 dimensions (where the angle itself is at the middle point
findAngleSubtendedByPoints :: Coordinate -> Coordinate -> Coordinate -> Float
findAngleSubtendedByPoints a b c = findAngleBetweenVectors (findVectorBetweenPoints a b) (findVectorBetweenPoints b c)

-- |"returnCoordinateList" returns a list of coordinates of the verticese in the face
returnCoordinateList :: Face -> [Coordinate]
returnCoordinateList x  = fst x

--- |"returnNormalVector" returns the vector for the normal to the plane in which the face resides
returnNormalVector :: Face -> Vector
returnNormalVector face  = crossProduct
                            (findVectorBetweenPoints (returnCoordinateList face !! 0) (returnCoordinateList face !! 1))
                            (findVectorBetweenPoints (returnCoordinateList face !! 1) (returnCoordinateList face !! 2))

-- |"checkNormals" returns True if the face all exists within the same plane
checkNormals :: Face -> Int -> Vector -> Bool
checkNormals face count vector
   | ((length (fst face)) <= (count+1)) = True
   | (count == 0) = checkNormals face 1 (crossProduct (findVectorBetweenPoints ((fst face) !! 0) ((fst face) !! 1))
                                                      (findVectorBetweenPoints ((fst face) !! 1) ((fst face) !! 2)))
   | ((areVectorsDirectionEqual (crossProduct (findVectorBetweenPoints ((fst face) !! (count-1)) ((fst face) !! count))
                                              (findVectorBetweenPoints ((fst face) !! (count)) ((fst face) !! (count + 1))))
                                 vector)) = checkNormals face (count+1) vector
   | otherwise = False

getAngleNumber :: Int -> Int -> Int
getAngleNumber angle size
   | (angle>=size)=getAngleNumber (angle-size) size
   | (angle<0)=getAngleNumber (size+angle) size
   | otherwise = angle

-- |"getNumberOfVertices" returns the number of verticese in a face
getNumberOfVertices::Face -> Int
getNumberOfVertices face = length (returnCoordinateList face)

-- |"getVertex" returns the coordinate of the ith vertex (where i is the integer passed in)
getVertex::Face -> Int -> Coordinate
getVertex face angle = (returnCoordinateList face) !! getAngleNumber angle (getNumberOfVertices face)

-- |"checkAngles" checks if a face's angles are valid, a face with two lines crossing is invalid
--if external angles > 2pi the shape crosses over itself and is not valid
--external angle = pi - internal angle
--internal angle < pi guaranteed
checkAngles :: Face -> Int -> Float
checkAngles face angle
   | (angle >= (length (fst face))) = 0
   | otherwise = ((checkAngles face (angle+1)) + pi -(findAngleBetweenVectors
                                                        (findVectorBetweenPoints
                                                            (getVertex face (getAngleNumber (angle)   (getNumberOfVertices face)))
                                                            (getVertex face (getAngleNumber (angle+1) (getNumberOfVertices face))))
                                                        (findVectorBetweenPoints
                                                            (getVertex face (getAngleNumber (angle+1) (getNumberOfVertices face)))
                                                            (getVertex face (getAngleNumber (angle+2) (getNumberOfVertices face))))))

--only used to fix rounding errors
approximatelyEquals :: Float -> Float -> Bool
approximatelyEquals a b
   | ((a-b < 0.1) && (b-a<0.1)) = True
   | otherwise = False

-- |"checkAnglesBool" returns True if the shape never has any two lines which cross eachother
checkAnglesBool :: Face -> Bool
checkAnglesBool face
   | (approximatelyEquals (checkAngles face 0) (2*pi)) = True
   | otherwise = False

-- |"heckIfValidFace" returns True if the normals are consistend and the angles are correct
checkIfValidFace :: Face -> Bool
checkIfValidFace face
   | ((length (fst face)) <3)  = False --a two sided shape is impossible in this model
   | ((length (fst face)) ==3) = True --a three sided shape cannot have crossing edges and is guaranteed to exist in one plane
   | otherwise                 = ((checkNormals face 0 ((0,0),0)) && (checkAnglesBool face))

-- |"addFace" adds a face to the world iff the face is valid, otherwise returns an error
addFace :: World -> Face -> World
addFace world face
   | (checkIfValidFace face) = (fst world, snd world ++ [face])
   | otherwise = error "That is not a valid face"

-- |"planFromFace" returns the plane in which a passed face resides
planeFromFace :: Face -> Plane
planeFromFace face = (getVertex face 0, returnNormalVector face)

-- |"vectorPLaneIntersectionScalar" returns the scalar required to go from a coordinate to a plane along a vector
--only call if VectorPlaneIntersectionCheck returned True (otherwise you divide by zero)
--pass in the plane, the vector and a point the vector crosses
vectorPlaneIntersectionScalar:: Plane -> Vector -> Coordinate -> Float
vectorPlaneIntersectionScalar plane vector coordinate = ((dotProduct
                                                            (findVectorBetweenPoints (fst plane) coordinate)
                                                            (snd plane)) /
                                                        (dotProduct (snd plane) vector ))

-- |"vectorPlaneIntersection" finds the coordinates where a vector and a plane meet
-- must be called after vectorPlaneIntersectionCheck returned True
vectorPlaneIntersection :: Plane -> Vector -> Coordinate -> Coordinate
vectorPlaneIntersection plane vector coordinate
 | vectorPlaneIntersectionCheck plane vector = addVectorToCoordinate
                                                    coordinate
                                                    (scaleVector
                                                        vector
                                                        (vectorPlaneIntersectionScalar plane vector coordinate))
 | otherwise = error "call vectorPlaneIntersectionCheck before calling vectorPlaneIntersection"

-- |"vectorPlaneIntersectinCheck" returns True if a vector will ever intersect with a plane (ie not perpendicular to normal)
vectorPlaneIntersectionCheck :: Plane -> Vector -> Bool
vectorPlaneIntersectionCheck plane vector
   | dotProduct (snd plane) vector == 0 = False
   | otherwise = True

insertIntoFacelistVector:: [Face] -> Face -> Vector -> Coordinate -> Int -> [Face]
insertIntoFacelistVector facelist face vector coordinate count
   | (count >= (length facelist)) = facelist ++ [face]
   | (vectorPlaneIntersectionScalar (planeFromFace face) vector coordinate)
        <(vectorPlaneIntersectionScalar (planeFromFace (facelist!!count)) vector coordinate)
        = (take (count) facelist) ++ [face] ++ (drop ((length facelist) - count - 2) facelist)
   | otherwise = insertIntoFacelistVector facelist face vector coordinate (count+1)

--sorts a list of faces by distance from a point in a line described by a vectoralso removes faces which wont be hit by the vector
sortFacesByDistanceFromPointi:: [Face] -> Vector -> Coordinate -> Int -> [Face]
sortFacesByDistanceFromPointi facelist vector coordinate count
   | (count ==  (length facelist)) = []
   | otherwise = insertIntoFacelistVector
                    (sortFacesByDistanceFromPointi facelist vector coordinate (count+1))
                    (facelist!!count) vector coordinate 0

sortFacesByDistanceFromPoint:: [Face] -> Vector -> Coordinate -> [Face]
sortFacesByDistanceFromPoint facelist vector coordinate = sortFacesByDistanceFromPointi facelist vector coordinate 0

-- |"removeFacesNeverHit" takes a list of faces and then returns a list of faces which get hit by a vector
-- the int passed should be 0
-- the vector and coordinate make the line which may hit the faces
removeFacesNeverHit:: [Face] -> Vector -> Coordinate -> Int -> [Face]
removeFacesNeverHit facelist vector coordinate count
   | (count>=(length facelist)) = []
   | (not(vectorPlaneIntersectionCheck (planeFromFace (facelist!!count)) vector))
        = removeFacesNeverHit facelist vector coordinate (count+1)
   | vectorPlaneIntersectionScalar (planeFromFace (facelist!!count)) vector coordinate >0
        = [facelist!!count]++removeFacesNeverHit facelist vector coordinate (count+1)
   | otherwise = removeFacesNeverHit facelist vector coordinate (count+1)

-- |"makeNewFaceList" takes a list of faces and a vector-coordinate pair then returns a list of faces which are hit by the vector sorted by the distance from the coordinate
makeNewFaceList :: [Face] -> Vector -> Coordinate -> [Face]
makeNewFaceList facelist vector coordinate = sortFacesByDistanceFromPoint
                                                (removeFacesNeverHit facelist vector coordinate 0)
                                                vector
                                                coordinate

-- |"checkCoordinateInFacialPlane returns True if the passed coordinate is in the same plane as the passed face
checkCoordinateInFacialPlane :: Face -> Coordinate -> Bool
checkCoordinateInFacialPlane face coordinate = (areVectorsDirectionEqual
                                                    (returnNormalVector face)
                                                    (crossProduct
                                                        (findVectorBetweenPoints
                                                            coordinate
                                                            (getVertex face 0))
                                                        (findVectorBetweenPoints
                                                            (getVertex face 0)
                                                            (getVertex face 1))))

-- |"getFaceAngleFromPoints" returns the angle between three points in the face, the angle in question is 1 above the index of the vertex passed in
getFaceAngleFromPoints :: Face -> Int -> Float
getFaceAngleFromPoints face angle = findAngleSubtendedByPoints
                                        (getVertex face angle)
                                        (getVertex face (angle+1))
                                        (getVertex face (angle+2))

-- |"checkIfInRange" returns True if a coordinate is within a face
--  the integer passed in should always be 0
--  assuming in same facial plane
--  assuming no reflex angles (greater than 180 degrees)
checkIfInRange :: Face -> Coordinate -> Int ->  Bool
checkIfInRange face coordinate angle
   | (angle == getNumberOfVertices face) = True
   | ((getFaceAngleFromPoints face angle)
        <(findAngleSubtendedByPoints
            (getVertex face (getAngleNumber angle (getNumberOfVertices face)))
            (getVertex face (getAngleNumber (angle+1)(getNumberOfVertices face)))
            coordinate))
                = False
   | otherwise = checkIfInRange face coordinate (angle+1)

checkForFacei :: Face -> Coordinate -> Bool
checkForFacei face coordinate = ((checkCoordinateInFacialPlane face coordinate) && (checkIfInRange face coordinate 0))

-- |"checkForFace" returns True if a vector coordinate pair ever hits a face
checkForFace :: Face -> Coordinate -> Vector -> Bool
checkForFace face vectororigin vector = checkForFacei face (vectorPlaneIntersection (planeFromFace face) vector vectororigin)

getFirstFaceHiti :: [Face] -> Coordinate -> Vector -> Int -> Face
getFirstFaceHiti facelist vectorOrigin vector count
   | count == (length facelist) = ([],(0,(0,0))) --no face
   | checkForFace (facelist!!count) vectorOrigin vector = facelist!!count
   | otherwise = getFirstFaceHiti facelist vectorOrigin vector (count+1)

-- |"getFirstFaceHit" takes the world and a vector-coordinate pair and returns the first face that is hit
getFirstFaceHit :: World -> Coordinate -> Vector -> Face
getFirstFaceHit world coordinate vector = getFirstFaceHiti (makeNewFaceList (snd world) vector coordinate) coordinate vector 0


--insertIntoFacelistVector [([((0,0),0),((1,1),0),((0,1),0)],(0,(0,0))), ([((0,0),1),((1,0),1),((0,1),1)],(0,(0,0)))] ([((0,0),1),((0,1),1),((1,0),1)],(0,(0,0))) ((0,0),1) ((0,0),-5) 0
--example face : ([((0,0),0),((1,0),0),((1,1),0),((0,1),0)],(0,(0,0)))
