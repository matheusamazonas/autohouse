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

import StdDebug, StdMisc

derive class iTask Room,Unit, TTYSettings, BaudRate, Parity, ByteSize

instance == Room where
	(==) (Room i1 _ _) (Room i2 _ _) = i1 == i2

instance toString Room where
	toString (Room i n _) = n +++ " (" +++ toString i +++ ")"

house :: Shared House
house = sdsFocus "AutoHouse" $ memoryStore "house" (Just [Room 0 "Living room" []])

room :: SDS Room Room Room
room = sdsLens "house" (const ()) (SDSRead r) (SDSWrite w) (SDSNotify n) house
where
	r :: Room [Room] -> MaybeError TaskException Room
	r p rs = case find ((==) p) rs of
		Just r = Ok r
		Nothing = Error (exception ("Can't find " +++ toString p +++ toString (length rs)))
	w :: Room [Room] Room -> MaybeError TaskException (Maybe [Room])
	w p rs nr = case find ((==) p) rs of
		Nothing = Ok (Just [nr:rs])
		Just _ = Ok $ Just $ replaceInList (==) nr rs
	n :: Room [Room] Room -> SDSNotifyPred Room
	n p1 _ _ = \_ p2 -> p1 == p2

main :: Task Room
main = manageHouse house

manageHouse :: (Shared House) -> Task Room
manageHouse sh = forever $ enterChoiceWithShared "Rooms" [ChooseFromCheckGroup \(Room _ n _) -> n] sh
	>>* [OnAction ActionEdit (hasValue manageRoom),
	     OnAction ActionNew (always newRoom)]

manageRoom :: Room -> Task Room
manageRoom r=:(Room _ n ds) = enterChoice (Title n) [] ds
	>>* [OnAction (Action "New device") (always (newDevice (sdsFocus r room))),
	     OnAction (Action "Send task") (hasValue sendTask)]
where
	sendTask (Unit _ _ d) = enterInformation "Select interval" [] 
		>>= \i -> withShared 1 \sh -> fillFactorial sh
		>>= \fac -> liftmTask d i fac
		>>= \_ -> return r

newRoom :: Task Room
newRoom = enterInformation "Room name" [] 
	>>= \name -> findFreeId
	>>= \i -> upd (\rs -> [(Room i name []):rs]) house @! (Room i name [])
where
	findFreeId :: Task Int
	findFreeId = get house 
		>>= \rs -> case rs of
			[] = return 0
			_  = return $ inc $ (\(Room i _ _) -> i) (last rs)

newDevice :: (Shared Room) -> Task Room
newDevice sh = enterInformation "Device name" []
	>>= \name -> enterChoiceAs "Choose the device type" [] dTypes fst
	>>= \ix -> (forms !! ix)
	>>= \wd -> wd (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 name d:ds]) sh) <<@ NoUserInterface
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

Start world = startEngine
		[ publish "/" $ const $ main
		, publish "/simulators" $ const $ viewSims
		] world




