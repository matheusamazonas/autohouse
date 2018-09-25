definition module Program

import iTasks
import Interpret
import Requirements

derive class iTask Program, ProgramInstance

:: Program = { pId :: Int,
               title :: String,
               req ::  Main (Requirements () Stmt),
               fill :: Task ProgramData,
               send :: MTaskDevice ProgramInstance -> Task () }

:: ProgramData :== (Int, [Dynamic])

:: ProgramInstance = { pIx :: Int,
                       pArgs :: [Dynamic],
                       pInt :: MTaskInterval,
                       pMig :: Migration }

:: Migration = DoNotMigrate | SameRoom | AnyRoom

programsBySpec :: MTaskDeviceSpec -> [Program]
programs :: [Program]
programIndex :: [(Int,String)]
