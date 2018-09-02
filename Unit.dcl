definition module Unit

import iTasks
import TTY
import Interpret.Device
from Room import :: Room, :: RoomId
from Programs import :: ProgramData
from Requirements import :: Requirements

:: AutoTask :== (ProgramData, MTaskInterval, Migration)

:: Migration = DoNotMigrate | SameRoom | AnyRoom

:: UnitId :== Int

:: Unit =
		{ uId :: Int,
		  uName :: String,
		  uDev :: MTaskDevice,
		  uStatus :: Bool,
		  uTasks :: [AutoTask]
		}

derive class iTask Unit, BaudRate, Parity, ByteSize, DeviceData, TTYSettings, Migration

instance == Unit

instance toString Unit

nextUnitId :: Shared UnitId
unitSh :: SDS UnitId Unit Unit
addUnit :: RoomId String a -> Task () | channelSync, iTask a
newUnit :: RoomId -> Task ()
editUnit :: Unit -> Task ()
viewUnit :: Unit -> Task ()
manageUnits :: Task ()
sendNewTask :: Unit -> Task ()
enterTaskDetails :: Task (MTaskInterval, Migration)
filterCompUnits :: (Main (Requirements () Stmt)) [Unit] -> Task [Unit]
sendProgramToUnit :: AutoTask Unit -> Task ()
