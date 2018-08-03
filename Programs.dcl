definition module Programs

import iTasks
import Interpret
import Requirements

:: Program = { pId :: Int,
               title :: String,
               req ::  Main (Requirements () Stmt),
               send :: MTaskDevice MTaskInterval -> Task () }

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(String, MTaskDevice MTaskInterval -> Task ())]
programs :: [Program]
programIndex :: [(Int,String)]
