module DataTypes where
import Data.List

type Coordinate   = ((Float, Float), Float)  -- ((x,y),z)
type Vector       = Coordinate   -- ((Δx,Δy),Δz), functionally no different to coordinate, just a direction
type Plane        = (Coordinate, Vector) --point on plane and normal
type Coefficients = (Float, (Float, Float)) -- reflectivity to ambient, diffuse and specular light
type Face         = (([Coordinate],  Coefficients), Char) -- ((a list of the vertices around the face, and the coefficients associated with the material), the identifier for the object the face is associated with)
type FaceList     = [Face]
type LightSource  = (Coordinate, (Float, (Vector, (Float, Float)))) -- (intensity, (direction, (angle reached, gradient)))
--gradient - the light intensity outside the central cone (for each degree away from the cone, it subtracts the gradient from the intensity of the intensite one degree closer to the cone). If the gradient is 0 then the light source is constant in each direction.
type Lighting    = ([LightSource], Float) -- a list of all the light sources followed by a float storing the ambient lighting
type World       = (Lighting, FaceList) -- the whole world being renderred, with all the faces and all the information required to render each face.
type AsciiRow    = [Char]
type AsciiImage  = [AsciiRow]
type CameraProps = (Resolution, (Coordinate, (Vector, (Vector, Vector)))) -- a pair containing the resolution of the image and the coordinate of the pinhole and the vector associated with the top-left of the image.
type Resolution  = ((Int, Float), (Int,Float)) -- the width and height of the image in characters each paired with the width angular difference between each pixel from the camera

