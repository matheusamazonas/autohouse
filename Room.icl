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
	>>= \i -> upd (\rs -> [(Room i name []):rs]) house @! () 

editRoom :: Room -> Task ()
editRoom r=:(Room rid n ds) = enterChoice (Title n) [ChooseFromList \u->u.uName] ds
		>>* [OnAction (Action "Connect 5 BT") (always connect5BT),
		     OnAction (Action "New device") (always (newUnit rid)),
		     OnAction (Action "New BT") (always (quickDevice "bt_dev" defaultBT)),
		     OnAction (Action "New POSIX") (always (quickDevice "posix_dev" defaultTCP)),
		     OnAction (Action "New simulator") (always (quickDevice "sim_dev" defaultSimulator)),
		     OnAction (Action "New serial") (always (quickDevice "serial_dev" defaultSerial)),
		     OnAction (Action "Send task") (hasValue sendNewProgram),
		     OnAction (Action "Edit device") (hasValue editUnit),
		     OnAction (Action "Disconnect") (hasValue disconnectUnit)]
where
	quickDevice :: String a -> Task () | channelSync, iTask a
	quickDevice name d = addUnit rid name d
	connect5BT :: Task ()
	connect5BT = (quickDevice "bt01" defaultBT
		-&&- quickDevice "bt02" {defaultBT & devicePath = "/dev/tty.HC-05-02-DevB"}
		-&&- quickDevice "bt03" {defaultBT & devicePath = "/dev/tty.HC-05-03-DevB"}
		-&&- quickDevice "bt04" {defaultBT & devicePath = "/dev/tty.HC-05-04-DevB"}
		-&&- quickDevice "bt05" {defaultBT & devicePath = "/dev/tty.HC-05-05-DevB"}) @! ()
