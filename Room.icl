implementation module Room

import Data.List
from Data.Func import $
from Interpret.Device.Serial import instance channelSync TTYSettings
from Interpret.Device.TCP import instance channelSync TCPSettings
from Interpret.Device.Simulator import instance channelSync SimSettings

import iTasks

import House
import Unit
import Default

derive class iTask Room

instance == Room where
	(==) (Room i1 _ _) (Room i2 _ _) = i1 == i2

instance toString Room where
	toString (Room i n _) = n +++ " (" +++ toString i +++ ")"

nextRoomId :: Shared RoomId
nextRoomId = sharedStore "nextRoomId" 0

roomSh :: SDS RoomId Room Room
roomSh = sdsLens "house" (const ()) (SDSRead r) (SDSWrite w) (SDSNotifyConst n) house
where
	r :: RoomId House -> MaybeError TaskException Room
	r p rs = case find (\(Room i _ _) -> p == i) rs of
		Just r = Ok r
		Nothing = Error $ exception ("Room read: Can't find room " +++ toString p)
	w :: RoomId [Room] Room -> MaybeError TaskException (Maybe [Room])
	w p rs nr = case find (\(Room i _ _) -> p == i) rs of
		Nothing = Ok $ Just [nr:rs]
		Just _ = Ok $ Just $ replaceInList (==) nr rs
	n :: RoomId Room -> SDSNotifyPred RoomId
	n p1 _ = \_ p2 -> p1 == p2

newRoom :: Task ()
newRoom = enterInformation "Room name" [] 
	>>= \name -> upd ((+)1) nextRoomId
	>>= \i -> upd (\rs -> [(Room i name []):rs]) house @! (Room i name []) @! ()

editRoom :: Room -> Task ()
editRoom r=:(Room rid n ds) = enterChoice (Title n) [ChooseFromList \u->u.uName] ds
		>>* [OnAction (Action "New device") (always (newUnit rid)),
		     OnAction (Action "New BT") (always (quickDevice "ardBT" defaultBT)),
		     OnAction (Action "New linux") (always (quickDevice "linux" defaultTCP)),
		     OnAction (Action "New simulator") (always (quickDevice "sim" defaultSimulator)),
		     OnAction (Action "New serial") (always (quickDevice "serial" defaultSerial)),
		     OnAction (Action "Send task") (hasValue sendNewTask),
		     OnAction (Action "Edit device") (hasValue editUnit),
		     OnAction (Action "Disconnect") (hasValue disconnectUnit)]
where
	quickDevice :: String a -> Task () | channelSync, iTask a
	quickDevice name d = addUnit rid name d