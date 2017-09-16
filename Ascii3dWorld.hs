{-
Module : 3dworld
Description : a basic system for maintaining a 3d world, including renderring and eventually moving objects

Maintainer : bren96.bt@gmail.com
Stability : Experimental
Portability : non-portable (only been made and tested on one ubuntu machine)
-}
module Ascii3dWorld where
import DataTypes
import VectorMaths
import GeneralUtility
import WorldMaths
import FaceMaths
import Camera
import AnsiiOutput

newWorld :: World
newWorld = (([],0.5), [])

makeSquareFace :: Coordinate -> Float -> Vector -> Float -> Vector -> Coefficients -> Char -> Face
makeSquareFace bottomLeftCoordinate height upDirection width rightDirection coefficients id
    | width  == 0 = error "width  = 0"
    | height == 0 = error "height = 0"
    | not (approximatelyEquals (findAngleBetweenVectors upDirection rightDirection) 1.5708) =
                    error "upDirection and rightDirection not perpendicular"
    | otherwise= (([bottomLeftCoordinate,
                    addVectorToCoordinate bottomLeftCoordinate (scaleVector (normaliseVector rightDirection) width),
                    addVectorToCoordinate bottomLeftCoordinate (addVectorToCoordinate
                                                                    (scaleVector (normaliseVector upDirection) height)
                                                                    (scaleVector (normaliseVector rightDirection) width)),
                    addVectorToCoordinate bottomLeftCoordinate (scaleVector (normaliseVector upDirection) height)],
                    coefficients), id)

makeCube :: Coordinate -> Float -> Float -> Float -> Vector -> Vector -> Vector -> Coefficients -> FaceList
makeCube baseCoordinate width height depth upDirection rightDirection backDirection coefficients
    | depth <= 0 = error "depth <= 0"
    | width  <= 0 = error "width  <= 0"
    | height <= 0 = error "height <= 0"
    | not (approximatelyEquals (findAngleBetweenVectors upDirection rightDirection) 1.5708) =
                    error "upDirection and rightDirection not perpendicular"
    | not (approximatelyEquals (findAngleBetweenVectors upDirection backDirection) 1.5708) =
                    error "upDirection and backDirection not perpendicular"
    | not (approximatelyEquals (findAngleBetweenVectors backDirection rightDirection) 1.5708) =
                    error "upDirection and backDirection not perpendicular"
    | otherwise = [
        makeSquareFace baseCoordinate height upDirection width rightDirection coefficients 'N',
        makeSquareFace baseCoordinate depth backDirection width rightDirection coefficients 'N',
        makeSquareFace baseCoordinate height upDirection depth backDirection coefficients 'N',
        makeSquareFace  (addVectorToCoordinate
                            baseCoordinate
                            (addVectorToCoordinate
                                (addVectorToCoordinate
                                    (scaleVector (normaliseVector upDirection) height)
                                    (scaleVector (normaliseVector rightDirection) width))
                                (scaleVector (normaliseVector backDirection) depth)))
                        (-height) upDirection
                        (-width ) rightDirection
                        coefficients '*',
        makeSquareFace  (addVectorToCoordinate
                            baseCoordinate
                            (addVectorToCoordinate
                                (addVectorToCoordinate
                                    (scaleVector (normaliseVector upDirection) height)
                                    (scaleVector (normaliseVector rightDirection) width))
                                (scaleVector (normaliseVector backDirection) depth)))
                        (-depth) backDirection
                        (-width) rightDirection
                        coefficients 'X',
        makeSquareFace  (addVectorToCoordinate
                            baseCoordinate
                            (addVectorToCoordinate
                                (addVectorToCoordinate
                                    (scaleVector (normaliseVector upDirection) height)
                                    (scaleVector (normaliseVector rightDirection) width))
                                (scaleVector (normaliseVector backDirection) depth)))
                        (-height) upDirection
                        (-depth ) backDirection
                        coefficients '\\'
        ]

firstWorld :: World
firstWorld = addFaceList (newWorld) (makeCube ((0,0),0) 1 1 1 ((1,0),0) ((0,1),0) ((0,0),1) (0,(0,0)))

run :: IO()
run = do
    let world  = firstWorld
    let camera = makeExampleCamera
    let image  = cameraRun world camera
    outputAsciiImage image

--addFace world ([((0,0),0),((1,0),0),((1,1),0),((0,1),0)],(0,(0,0)))
--insertIntoFacelistVector [([((0,0),0),((1,1),0),((0,1),0)],(0,(0,0))), ([((0,0),1),((1,0),1),((0,1),1)],(0,(0,0)))] ([((0,0),1),((0,1),1),((1,0),1)],(0,(0,0))) ((0,0),1) ((0,0),-5) 0
--example face : ([((0,0),0),((1,0),0),((1,1),0),((0,1),0)],(0,(0,0)))
