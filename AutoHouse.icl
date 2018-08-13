module AutoHouse

import StdMisc, StdArray
import Data.Func, Data.List, Data.Maybe
from System.Time import :: Timespec {..}
from Data.Foldable import class Foldable, concat
import qualified Data.Map as DM

import iTasks
import iTasks.Internal.Store
import iTasks.Extensions.Admin.WorkflowAdmin

import Interpret
import Requirements
import Interpret.Device
import TTY
import Programs

import StdDebug, StdMisc

:: House :== [Room]

:: Room = Room Int String [Unit]

:: Unit = Unit Int String MTaskDevice

derive class iTask Room,Unit, TTYSettings, BaudRate, Parity, ByteSize, DeviceData, BCState

instance == Room where
	(==) (Room i1 _ _) (Room i2 _ _) = i1 == i2

instance == Unit where
	(==) (Unit i1 n1 _) (Unit i2 n2 _) = i1 == i2 && n1 == n2

instance toString Room where
	toString (Room i n _) = n +++ " (" +++ toString i +++ ")"

instance toString Unit where
	toString (Unit i n _) = n +++ " (" +++ toString i +++ ")"

// ----------- House -----------

house :: Shared House
house = sdsFocus "AutoHouse" $ memoryStore "house" (Just [Room 0 "Living room" []])

manageHouse :: (Shared House) -> Task ()
manageHouse sh = forever $ enterChoiceWithShared "Rooms" [ChooseFromList \(Room _ n _) -> n] sh
	>>* [OnAction ActionEdit (hasValue editRoom),
	     OnAction ActionNew (always newRoom)]

// ----------- Room -----------

roomSh :: SDS Room Room Room
roomSh = sdsLens "house" (const ()) (SDSRead r) (SDSWrite w) (SDSNotify n) house
where
	r :: Room [Room] -> MaybeError TaskException Room
	r p rs = case find ((==) p) rs of
		Just r = Ok r
		Nothing = Error $ exception ("Can't find " +++ toString p)
	w :: Room [Room] Room -> MaybeError TaskException (Maybe [Room])
	w p rs nr = case find ((==) p) rs of
		Nothing = Ok $ Just [nr:rs]
		Just _ = Ok $ Just $ replaceInList (==) nr rs
	n :: Room [Room] Room -> SDSNotifyPred Room
	n p1 _ _ = \_ p2 -> p1 == p2

newRoom :: Task ()
newRoom = enterInformation "Room name" [] 
	>>= \name -> findFreeId
	>>= \i -> upd (\rs -> [(Room i name []):rs]) house @! (Room i name []) @! ()
where
	findFreeId :: Task Int
	findFreeId = get house 
		>>= \rs -> case rs of
			[] = return 0
			_  = return $ inc $ (\(Room i _ _) -> i) (last rs)

editRoom :: Room -> Task ()
editRoom r=:(Room _ n ds) = enterChoice (Title n) [ChooseFromList \(Unit _ n _) -> n] ds
	>>* [OnAction (Action "New device") (always (newUnit (sdsFocus r roomSh))),
	     OnAction (Action "Send task") (hasValue sendTask),
	     OnAction (Action "Edit device") (hasValue (editUnit (sdsFocus r roomSh)))]

// ----------- Unit -----------

allUnits :: SDS () [Unit] [Room]
allUnits = mapRead (concat o (map getUnits)) house
where
	getUnits :: Room -> [Unit]
	getUnits r=:(Room _ _ us) = us

unitSh :: (Shared Room) -> SDS Unit Unit Unit
unitSh roomSh = sdsLens "room" (const ()) (SDSRead r) (SDSWrite w) (SDSNotify n) roomSh
where
	r :: Unit Room -> MaybeError TaskException Unit
	r u (Room _ _ us) = case find ((==) u) us of
		Just u = Ok u
		Nothing = Error (exception ("Can't find unit " +++ toString u))
	w :: Unit Room Unit -> MaybeError TaskException (Maybe Room)
	w p (Room i n us) u = case find ((==) p) us of
		Nothing = Ok $ Just (Room i n [u:us])
		Just _ = Ok $ Just $ (Room i n (replaceInList (==) u us))
	n :: Unit Room Unit -> SDSNotifyPred Unit
	n p1 _ _ = \_ p2 -> p1 == p2

newUnit :: (Shared Room) -> Task ()
newUnit sh = enterInformation "Device name" []
	>>= \name -> enterChoiceAs "Choose the device type" [ChooseFromDropdown snd] dTypes fst
	>>= \ix -> (forms !! ix)
	>>= \wd -> wd (\d -> upd (\(Room i n ds) -> Room i n [Unit 0 name d:ds]) sh @! ())
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
		  // devicePath = "/dev/tty.usbmodem1421",
		  devicePath = "/dev/tty.HC-05-01-DevB",
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
			, haveHumid     = True
			, haveUltra     = True
			, haveMov       = True
			, haveLightDig  = True
			, haveLightAna  = True
			, haveServo     = True
			, aPins         = 1
			, dPins         = 14
			, stackSize     = 64
			, bytesMemory   = 1024
			, resolveLabels = False
			} Automatic 1000.0

editUnit :: (Shared Room) Unit -> Task ()
editUnit rsh u=:(Unit i _ (Device dsh _)) = forever $ get dsh >>* [OnValue (hasValue editInfo)]
where
	editInfo :: DeviceData -> Task ()
	editInfo dd = updateSharedInformation title [UpdateAs getName putName] (sdsFocus u (unitSh rsh)) @! ()
	where
		title = Title $ "Edit unit #" +++ toString i
		getName :: Unit -> String
		getName (Unit _ n _) = n
		putName :: Unit String -> Unit
		putName (Unit i _ us) n = Unit i n us

viewUnit :: Unit -> Task ()
viewUnit u=:(Unit i _ (Device dsh _)) = forever $ get dsh >>* [OnValue (hasValue showInfo)]
where
	showInfo :: DeviceData -> Task ()
	showInfo dd = viewDevShares dd.deviceShares
		||- viewDevTasks dd.deviceTasks <<@ ArrangeHorizontal
				>>* [OnAction ActionRefresh (always (return ()))]
	where
		
		viewDevTasks :: ('DM'.Map Int Bool) -> Task ()
		viewDevTasks tm = viewInformation "Device Tasks "[] ()
			-|| (allTasks $ 'DM'.elems $ 'DM'.mapWithKey viewDevTask tm)
		where
			viewDevTask :: Int Bool -> Task ()
			viewDevTask i b = viewInformation ("Task " +++ toString i) [] b @! ()
		viewDevShares :: ('DM'.Map Int (Bool, SDS Bool BCValue BCValue)) -> Task ()
		viewDevShares sm = viewInformation "Device Shares" [] ()
			-|| (allTasks $ 'DM'.elems $ 'DM'.mapWithKey viewDevShare sm) 
		where
			viewDevShare :: Int (Bool, SDS Bool BCValue BCValue) -> Task ()
			viewDevShare k (ack, sds) = viewInformation "SDS id" [] k
				||- viewInformation "Acked?" [] ack
				||- try 
					(viewSharedInformation "SDS Value" [] (sdsFocus True sds) @! ())
					(\e -> viewInformation ("SDS doesnt exist anymore. " +++ e) [] ())

manageUnits :: Task ()
manageUnits = enterChoiceWithShared "Choose a unit" [ChooseFromList \(Unit i n _) -> n] allUnits
	>>* [OnAction (Action "View Tasks") (hasValue viewUnit),
	     OnAction (Action "Send Task") (hasValue sendTask) ]

getSpec :: Unit -> Task (Maybe MTaskDeviceSpec)
getSpec (Unit _ _ (Device ddsh _)) = get ddsh >>= \dd -> return dd.deviceSpec

// ----------- Task -----------

chooseInterval :: Task MTaskInterval
chooseInterval = updateInformation "Choose Interval" [] (OnInterval 1000)

sendTask :: Unit -> Task ()
sendTask u=:(Unit _ _ d) = getSpec u
	>>= \ds -> enterChoice "Choose Task" [ChooseFromList fst] (programsBySpec ds)
	>>= \(_,pt) -> chooseInterval
	>>= \i -> pt d i

newTask :: Task ()
newTask = enterChoice "Choose Task" [ChooseFromList snd] programIndex
	>>= \(ix,n) -> chooseInterval
	>>= \i -> compUnits (programs !! ix).req
	>>= \us -> enterChoice "Choose unit" [ChooseFromList unitName] us
	>>= \(Unit _ _ d) -> (programs !! ix).send d i
where
	unitName :: Unit -> String
	unitName (Unit _ n _) = n
	compUnits :: (Main (Requirements () Stmt)) -> Task [Unit]
	compUnits r = get allUnits
		>>= \us -> allTasks (map (compatible r) us)
		>>= \up -> return $ map fst $ filter snd up
	where
		compatible :: (Main (Requirements () Stmt)) Unit -> Task (Unit, Bool)
		compatible r u=:(Unit _ _ (Device ddsh _)) = get ddsh
			>>= \dd -> return (u, match r dd.deviceSpec)

// ----------- Main -----------

main :: Task ()
main = loginAndManageWorkList "Autohouse" workflows
where
	workflows = [transientWorkflow "Manage house" "Create, delete and edit rooms" (manageHouse house),
	             transientWorkflow "Manage unit" "Create, delete and edit unit" manageUnits,
	             transientWorkflow "New task" "Send a task to a unit" newTask
	             ]

Start world = startEngineWithOptions 
		(\cli options.defaultEngineCLIOptions cli {options & sessionTime = {tv_sec = 1000000000, tv_nsec=0}})
		[ publish "/" $ const $ main
		, publish "/simulators" $ const $ viewSims
		] world




