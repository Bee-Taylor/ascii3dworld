module Camera where
import DataTypes
import WorldMaths
import VectorMaths
import AnsiiOutput

makeCamera :: Resolution -> Coordinate -> Vector -> Vector -> Vector -> CameraProps
makeCamera resolution pinhole topleft rightvector downvector =(resolution, (pinhole,(topleft,(rightvector,downvector))))

makeExampleResolution :: Resolution
makeExampleResolution = ((100,0.01),(40,0.015))

makeExampleCamera :: CameraProps
makeExampleCamera = makeCamera (makeExampleResolution) ((4, 4),4) ((-1,-0.5),-0.75) ((0,-1),0) ((0,0),-1)

cameraRun :: World -> CameraProps -> AsciiImage
cameraRun world camera = cameraRuni world camera 0

cameraRuni :: World -> CameraProps -> Int -> AsciiImage
cameraRuni world camera collumn
  | collumn  == cameraGetHeight camera = []
  | otherwise = [cameraRunTravRow world camera 0 collumn] ++ cameraRuni world camera (collumn + 1)

cameraRunTravRow :: World -> CameraProps -> Int -> Int -> AsciiRow
cameraRunTravRow world camera row collumn
 | row       == cameraGetWidth camera   = []
 | collumn   == cameraGetHeight camera = []
 | otherwise =  [getPixel world (cameraPinhole camera) (cameraCurrentVector camera row collumn)] ++ cameraRunTravRow world camera (row+1) collumn


getPixel :: World -> Coordinate -> Vector -> Char
getPixel world coordinate vector
 | (snd $ getFirstFaceHit world coordinate vector) == '0' = ' '
 | otherwise = (snd $ getFirstFaceHit world coordinate vector)

cameraPinhole :: CameraProps -> Coordinate
cameraPinhole camera = fst $ snd camera

cameraTLVector :: CameraProps -> Vector
cameraTLVector camera = fst $ snd $ snd camera

cameraRowElementWidth :: CameraProps -> Float
cameraRowElementWidth camera = snd $ fst $ fst camera

cameraCollumnElementHeight :: CameraProps -> Float
cameraCollumnElementHeight camera = snd $ snd $ fst camera

cameraCurrentVector :: CameraProps -> Int -> Int -> Vector
cameraCurrentVector camera row collumn = addVectorToCoordinate
                                            (addVectorToCoordinate
                                                (scaleVector (normaliseVector $ cameraRightVector camera)
                                                             ((cameraRowElementWidth camera) * (fromIntegral row)))
                                                (scaleVector (normaliseVector $ cameraDownVector camera)
                                                             ((cameraCollumnElementHeight camera) * (fromIntegral collumn))))
                                            (cameraTLVector camera)

cameraRightVector :: CameraProps -> Vector
cameraRightVector camera = fst $ snd $ snd $ snd camera

cameraDownVector :: CameraProps -> Vector
cameraDownVector camera  = snd $ snd $ snd $ snd camera

cameraGetWidth :: CameraProps -> Int
cameraGetWidth camera = fst $ fst $ fst camera

cameraGetHeight :: CameraProps -> Int
cameraGetHeight camera = fst $ snd $ fst camera