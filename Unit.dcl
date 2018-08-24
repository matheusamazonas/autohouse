definition module Unit

import iTasks
import TTY
import Interpret.Device
from Room import :: Room
from Requirements import :: Requirements

:: Unit = Unit Int String MTaskDevice Bool

derive class iTask Unit, BaudRate, Parity, ByteSize, DeviceData, TTYSettings

instance == Unit

instance toString Unit

nextUnitId :: Shared Int
unitSh :: SDS Unit Unit Unit
addUnit :: Room String a -> Task () | channelSync, iTask a
newUnit :: Room -> Task ()
editUnit :: Unit -> Task ()
viewUnit :: Unit -> Task ()
manageUnits :: Task ()
sendTask :: Unit -> Task ()
chooseInterval :: Task MTaskInterval
getUnitName :: Unit -> String
compatible :: (Main (Requirements () Stmt)) Unit -> Task (Unit, Bool)
