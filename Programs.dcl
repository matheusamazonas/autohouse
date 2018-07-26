definition module Programs

import iTasks
import Interpret
import Requirements

:: Program = { pId :: Int,
               title :: String,
               req ::  Main (Requirements () Stmt),
               send :: MTaskDevice MTaskInterval -> Task () }

class program v | arith, seq, boolExpr, noOp, vari, IF, dIO, aIO, dht22, hcsr04, sdspub, iTasksSds, assign, retrn, userLed v

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(String, MTaskDevice MTaskInterval -> Task ())]
programs :: [Program]
programIndex :: [(Int,String)]
