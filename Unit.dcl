definition module Unit

import iTasks
import TTY
import Interpret.Device
import Program
from Room import :: Room, :: RoomId
from Requirements import :: Requirements

:: UnitId :== Int

:: Unit =
		{ uId :: Int,
		  uName :: String,
		  uDev :: MTaskDevice,
		  uStatus :: Bool,
		  uTasks :: [ProgramInstance]
		}

derive class iTask Unit, BaudRate, Parity, ByteSize, DeviceData, TTYSettings, Migration

instance == Unit

instance toString Unit

addUnit :: RoomId String a -> Task () | channelSync, iTask a
newUnit :: RoomId -> Task ()
editUnit :: Unit -> Task ()
viewUnit :: Unit -> Task ()
manageUnits :: Task ()
sendNewProgram :: Unit -> Task ()
enterTaskDetails :: Task (MTaskInterval, Migration)
filterCompUnits :: Program [Unit] -> Task [Unit]
sendProgramToUnit :: ProgramInstance Unit -> Task ()
disconnectUnit :: Unit -> Task ()