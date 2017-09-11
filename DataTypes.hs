module DataTypes where
import Data.List

type Coordinate   = ((Float, Float), Float)  -- ((x,y),z)
type Vector       = Coordinate   -- ((Δx,Δy),Δz), functionally no different to coordinate, just a direction
type Plane        = (Coordinate, Vector) --point on plane and normal
type Coefficients = (Float, (Float, Float)) -- reflectivity to ambient, diffuse and specular light
type Face         = (([Coordinate],  Coefficients), Int) -- (a list of the vertices around the face, and the coefficients associated with the material)
type FaceList     = [Face]
type LightSource  = (Coordinate, (Float, (Vector, (Float, Float)))) -- (intensity, (direction, (angle reached, gradient)))
--gradient - the light intensity outside the central cone (for each degree away from the cone, it subtracts the gradient from the intensity of the intensite one degree closer to the cone). If the gradient is 0 then the light source is constant in each direction.
type Lighting    = ([LightSource], Float) -- a list of all the light sources followed by a float storing the ambient lighting
type World       = (Lighting, FaceList) -- the whole world being renderred, with all the faces and all the information required to render each face.
