module WorldMaths
where

import DataTypes
import FaceMaths
import VectorMaths

-- |"addFace" adds a face to the world iff the face is valid, otherwise returns an error and won't add
addFace :: World -> Face -> World
addFace world face
   | (checkIfValidFace face) = (fst world, snd world ++ [face])
   | otherwise = error "That is not a valid face"

-- |"addFaceList" adds all the   faces in a FaceList, if any are invalid it will return an error and add none
addFaceList :: World -> FaceList -> World
addFaceList world facelist
   | checkIfValidFaceList facelist = (fst world, snd world ++ facelist)
   | otherwise = error "One of those faces was invalid, none were added"

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
        = (take count facelist) ++ [face] ++ (drop ((length facelist) - count - 2) facelist)
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
   | count>=(length facelist) = []
   | not (vectorPlaneIntersectionCheck (planeFromFace (facelist!!count)) vector)
        = removeFacesNeverHit facelist vector coordinate (count+1)
   | vectorPlaneIntersectionScalar (planeFromFace (facelist!!count)) vector coordinate > 0
        = [facelist!!count]++removeFacesNeverHit facelist vector coordinate (count+1)
   | otherwise = removeFacesNeverHit facelist vector coordinate (count+1)

-- |"makeNewFaceList" takes a list of faces and a vector-coordinate pair then returns a list of faces which are hit by the vector sorted by the distance from the coordinate
makeNewFaceList :: [Face] -> Vector -> Coordinate -> [Face]
makeNewFaceList facelist vector coordinate = sortFacesByDistanceFromPoint
                                                (removeFacesNeverHit facelist vector coordinate 0)
                                                vector
                                                coordinate

getFirstFaceHiti :: [Face] -> Coordinate -> Vector -> Int -> Face
getFirstFaceHiti facelist vectorOrigin vector count
   | count == (length facelist) = (([],(0,(0,0))),'0') --no face
   | checkForFace (facelist!!count) vectorOrigin vector = facelist!!count
   | otherwise = getFirstFaceHiti facelist vectorOrigin vector (count+1)

-- |"getFirstFaceHit" takes the world and a vector-coordinate pair and returns the first face that is hit
getFirstFaceHit :: World -> Coordinate -> Vector -> Face
getFirstFaceHit world coordinate vector = getFirstFaceHiti (makeNewFaceList (snd world) vector coordinate) coordinate vector 0

checkForFacei :: Face -> Coordinate -> Bool
checkForFacei face coordinate = ((checkCoordinateInFacialPlane face coordinate) && (checkIfInRange face coordinate 0))

-- |"checkForFace" returns True if a vector coordinate pair ever hits a face
checkForFace :: Face -> Coordinate -> Vector -> Bool
checkForFace face vectororigin vector = checkForFacei face (vectorPlaneIntersection (planeFromFace face) vector vectororigin)

checkIfValidFaceListi :: FaceList -> Int -> Bool
checkIfValidFaceListi facelist count
    | count == (length facelist) = True
    | checkIfValidFace (facelist !! count) = checkIfValidFaceListi facelist (count + 1)
    | otherwise = False

-- |"checkIfValidFaceList" returns True if every face in a facelist is valid
checkIfValidFaceList :: FaceList -> Bool
checkIfValidFaceList facelist = checkIfValidFaceListi facelist 0