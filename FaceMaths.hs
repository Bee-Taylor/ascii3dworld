module FaceMaths where

import VectorMaths
import DataTypes
import GeneralUtility

-- |"findAngleSubtendedByPoints" finds the angle between 3 points in 3 dimensions (where the angle itself is at the middle point
findAngleSubtendedByPoints :: Coordinate -> Coordinate -> Coordinate -> Float
findAngleSubtendedByPoints a b c = findAngleBetweenVectors (findVectorBetweenPoints a b) (findVectorBetweenPoints b c)


-- |"returnCoordinateList" returns a list of coordinates of the verticese in the face
returnCoordinateList :: Face -> [Coordinate]
returnCoordinateList x  = fst $ fst x

--- |"returnNormalVector" returns the vector for the normal to the plane in which the face resides
returnNormalVector :: Face -> Vector
returnNormalVector face  = crossProduct
                            (findVectorBetweenPoints (returnCoordinateList face !! 0) (returnCoordinateList face !! 1))
                            (findVectorBetweenPoints (returnCoordinateList face !! 1) (returnCoordinateList face !! 2))

-- |"checkNormals" returns True if the face all exists within the same plane
checkNormals :: Face -> Int -> Vector -> Bool
checkNormals face count vector
   | ((length (returnCoordinateList face)) <= (count+1)) = True
   | (count == 0) = checkNormals face 1 (crossProduct (findVectorBetweenPoints ((returnCoordinateList face) !! 0) ((returnCoordinateList face) !! 1))
                                                      (findVectorBetweenPoints ((returnCoordinateList face) !! 1) ((returnCoordinateList face) !! 2)))
   | ((areVectorsDirectionEqual (crossProduct (findVectorBetweenPoints ((returnCoordinateList face) !! (count-1)) ((returnCoordinateList face) !! count))
                                              (findVectorBetweenPoints ((returnCoordinateList face) !! (count)) ((returnCoordinateList face) !! (count + 1))))
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
   | (angle >= (length (returnCoordinateList face))) = 0
   | otherwise = ((checkAngles face (angle+1)) + pi -(findAngleBetweenVectors
                                                        (findVectorBetweenPoints
                                                            (getVertex face (getAngleNumber (angle)   (getNumberOfVertices face)))
                                                            (getVertex face (getAngleNumber (angle+1) (getNumberOfVertices face))))
                                                        (findVectorBetweenPoints
                                                            (getVertex face (getAngleNumber (angle+1) (getNumberOfVertices face)))
                                                            (getVertex face (getAngleNumber (angle+2) (getNumberOfVertices face))))))


-- |"checkAnglesBool" returns True if the shape never has any two lines which cross eachother
checkAnglesBool :: Face -> Bool
checkAnglesBool face
   | (approximatelyEquals (checkAngles face 0) (2*pi)) = True
   | otherwise = False

-- |"heckIfValidFace" returns True if the normals are consistend and the angles are correct
checkIfValidFace :: Face -> Bool
checkIfValidFace face
   | ((length (returnCoordinateList face)) <3)  = False --a two sided shape is impossible in this model
   | ((length (returnCoordinateList face)) ==3) = True --a three sided shape cannot have crossing edges and is guaranteed to exist in one plane
   | otherwise                 = ((checkNormals face 0 ((0,0),0)) && (checkAnglesBool face))

-- |"planFromFace" returns the plane in which a passed face resides
planeFromFace :: Face -> Plane
planeFromFace face = (getVertex face 0, returnNormalVector face)

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

