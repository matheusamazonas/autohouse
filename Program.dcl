definition module Program

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

:: ProgramInstance :== (ProgramData, MTaskInterval, Migration)

:: Migration = DoNotMigrate | SameRoom | AnyRoom

programsBySpec :: (Maybe MTaskDeviceSpec) -> [Program]
programs :: [Program]
programIndex :: [(Int,String)]
