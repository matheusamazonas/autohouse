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

nextRoomId :: Shared Int
nextRoomId = sharedStore "nextRoomId" 0

roomSh :: SDS Room Room Room
roomSh = sdsLens "house" (const ()) (SDSRead r) (SDSWrite w) (SDSNotifyConst n) house
where
	r :: Room [Room] -> MaybeError TaskException Room
	r p rs = case find ((==) p) rs of
		Just r = Ok r
		Nothing = Error $ exception ("Can't find " +++ toString p)
	w :: Room [Room] Room -> MaybeError TaskException (Maybe [Room])
	w p rs nr = case find ((==) p) rs of
		Nothing = Ok $ Just [nr:rs]
		Just _ = Ok $ Just $ replaceInList (==) nr rs
	n :: Room Room -> SDSNotifyPred Room
	n p1 _ = \_ p2 -> p1 == p2

newRoom :: Task ()
newRoom = enterInformation "Room name" [] 
	>>= \name -> upd ((+)1) nextRoomId
	>>= \i -> upd (\rs -> [(Room i name []):rs]) house @! (Room i name []) @! ()

editRoom :: Room -> Task ()
editRoom r=:(Room _ n ds) = enterChoice (Title n) [ChooseFromList \(Unit _ n _) -> n] ds
		>>* [OnAction (Action "New device") (always (newUnit r)),
		     OnAction (Action "New BT") (always (quickBT (sdsFocus r roomSh))),
		     OnAction (Action "New linux") (always (quickLinux (sdsFocus r roomSh))),
		     OnAction (Action "New simulator") (always (quickSim (sdsFocus r roomSh))),
		     OnAction (Action "New serial") (always (quickSerial (sdsFocus r roomSh))),
		     OnAction (Action "Send task") (hasValue sendTask),
		     OnAction (Action "Edit device") (hasValue (editUnit (sdsFocus r roomSh)))]
where
	// quickDevice :: a String -> Task () | channelSync a
	// quickDevice d name = addUnit r name d
	quickBT :: (Shared Room) -> Task ()
	quickBT sh
	# d = {zero & devicePath = "/dev/tty.HC-05-01-DevB", xonxoff=True}
	= (withDevice d) (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 "ardBT" d:ds]) sh @! ()) @! ()
	quickLinux :: (Shared Room) -> Task ()
	quickLinux sh
	# d = defaultTCP
	= (withDevice d) (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 "linux_client" d:ds]) sh @! ()) @! ()
	quickSim :: (Shared Room) -> Task ()
	quickSim sh = (withDevice defaultSimulator) (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 "simulator" d:ds]) sh @! ()) >>| return ()
	quickSerial :: (Shared Room) -> Task ()
	quickSerial sh
	# d = defaultSerial
	= (withDevice d) (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 "serial" d:ds]) sh @! ()) @! ()
