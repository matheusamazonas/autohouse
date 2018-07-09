implementation module AutoHouse

import StdMisc
import Data.Func
import Data.List

import iTasks
import iTasks.Internal.Store

import Interpret
import Interpret.Device
import TTY
import Programs

derive class iTask Room,Unit, TTYSettings, BaudRate, Parity, ByteSize

instance == Room where
	(==) (Room i1 _ _) (Room i2 _ _) = i1 == i2
house :: Shared House
house = sdsFocus "AutoHouse" $ memoryStore "house" (Just [])

room :: SDS Room Room Room
room = sdsLens "house" (const ()) (SDSRead r) (SDSWrite w) (SDSNotify n) house
where
	r :: Room [Room] -> MaybeError TaskException Room
	r r=:(Room i _ _) rs = case find ((==) r) rs of
		Just r = Ok r
		Nothing = Error (exception ("Can't find room " +++ toString i))
	w :: Room [Room] Room -> MaybeError TaskException (Maybe [Room])
	w p rs nr = case find ((==) p) rs of
		Nothing = Ok (Just [nr:rs])
		Just _ = Ok $ Just $ replaceInList (==) nr rs
	n :: Room [Room] Room -> SDSNotifyPred Room
	n p1 _ _ = \_ p2 -> p1 == p2


main :: Task Room
main = withShared defaultValue manageHouse

manageHouse :: (Shared House) -> Task Room
manageHouse sh = enterChoiceWithShared "Rooms" [] sh
	>>* [OnAction ActionEdit (hasValue manageRoom)]

manageRoom :: Room -> Task Room
manageRoom r=:(Room i n us) = viewInformation ("Room " +++ n) [] r

newDevice :: (Shared Room) -> Task Room
newDevice sh = enterInformation "Device name" []
	>>= \name -> enterChoiceAs "Choose the device type" [] dTypes fst
	>>= \i -> (forms !! i)
	>>= \wd -> wd (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 name d:ds]) sh)
where
	dTypes :: [(Int, String)]
	dTypes = [(0, "Simulator"),
	          (1, "TCP"),
	          (2, "Serial")]
	forms :: [Task ((MTaskDevice -> Task a) -> Task a)] | iTask a
	forms = [newSimulator @ withDevice, newTCP @ withDevice, newSerial @ withDevice]
	newSerial :: Task TTYSettings
	newSerial = updateInformation "Serial port settings" [] 
		{ zero & 
		  devicePath = "/dev/tty.usbmodem1421",
		  xonxoff = True }
	newTCP :: Task TCPSettings
	newTCP = updateInformation "TCP settings" [] {host = "localhost", port=8123}
	newSimulator :: Task SimSettings
	newSimulator = updateInformation "Simulator settings" [] $
		SimSettings 
			{ haveLed       = True
			, haveLCD       = False
			, haveHb        = False
			, haveTemp      = True
			, aPins         = 1
			, dPins         = 14
			, stackSize     = 64
			, bytesMemory   = 1024
			, resolveLabels = False
			} Automatic 1000.0

Start w = startEngine main w