definition module Programs

import iTasks
import Interpret
import Requirements

derive class iTask Program

:: Program = { pId :: Int,
               title :: String,
               req ::  Main (Requirements () Stmt),
               fill :: Task ProgramData,
               send :: MTaskDevice MTaskInterval ProgramData -> Task () }

:: ProgramData :== (Int, [Dynamic])

programsBySpec :: (Maybe MTaskDeviceSpec) -> [Program]
programs :: [Program]
programIndex :: [(Int,String)]
