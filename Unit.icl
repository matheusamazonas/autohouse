implementation module Unit

import Data.List, Data.Eq
from Data.Func import $
import qualified Data.Map as DM

import iTasks
import Interpret
from TTY import :: BaudRate, :: Parity, :: ByteSize

import House
import Room
import Program
import Default

derive class iTask Unit, BaudRate, Parity, ByteSize, DeviceData, TTYSettings, BCState, Migration

gDefault{|Dynamic|} = dynamic ()

instance == Unit where
	(==) u1 u2 = u1.uId == u2.uId

instance toString Unit where
	toString u = u.uName +++ " (" +++ toString u.uId +++ ")"

unitData :: Unit -> Shared DeviceData
unitData {uDev=(Device dd _)} = dd

unitChannels :: Unit -> Shared Channels
unitChannels {uDev = (Device _ ch)} = ch

nextUnitId :: Shared UnitId
nextUnitId = sharedStore "nextUnitId" 0

unitSh :: SDS UnitId Unit Unit
unitSh = sdsLens "room" (const ()) (SDSRead r) (SDSWrite w) (SDSNotifyConst n) house
where
	r :: UnitId House -> MaybeError TaskException Unit
	r p [] = Error (exception $ "Unit read: There are no rooms in the house (" +++ toString p +++ ")")
	r p [(Room _ _ us):rs] = case find (\u -> u.uId == p) us of
		Just u = Ok u
		Nothing = r p rs
	w :: UnitId House Unit -> MaybeError TaskException (Maybe House)
	w _ [] _ = Error (exception "Unit write: There are no rooms in the house")
	w _ h nu = case replaceUnit nu h [] of
		Nothing = Error (exception "Can't find unit")
		Just h = Ok $ Just h
	where
		replaceUnit :: Unit House House -> Maybe House
		replaceUnit _ [] _ = Nothing
		replaceUnit u [r=:(Room i n us):rs] acc = case find ((==)u) us of
			Nothing = replaceUnit u rs [r:acc]
			Just _
				# us = replaceInList (==) u us
				= Just $ [Room i n us:rs] ++ acc
	n :: UnitId Unit -> SDSNotifyPred UnitId
	n i1 _ = \_ i2 -> i1 == i2

newUnit :: RoomId -> Task ()
newUnit rid = enterInformation "Device name" []
	>>= \name -> enterChoiceAs "Choose the device type" [ChooseFromDropdown snd] dTypes fst
	>>= saveUnit name
where
	dTypes :: [(Int, String)]
	dTypes = [(0, "Simulator"),
	          (1, "TCP"),
	          (2, "Serial")]
	saveUnit :: String Int -> Task ()
	saveUnit name 0 = newSimulator >>= addUnit rid name
	saveUnit name 1 = newTCP       >>= addUnit rid name
	saveUnit name 2 = newSerial    >>= addUnit rid name
	newSerial :: Task TTYSettings
	newSerial = updateInformation "Serial port settings" [] defaultSerial
	newTCP :: Task TCPSettings
	newTCP = updateInformation "TCP settings" [] defaultTCP
	newSimulator :: Task SimSettings
	newSimulator = updateInformation "Simulator settings" [] defaultSimulator

addUnit :: RoomId String a -> Task () | channelSync, iTask a
addUnit rid name dev = upd ((+)1) nextUnitId
	>>= \i -> withDevice dev (add i) (\_ -> migrateTasks rid i)
where
	add :: Int MTaskDevice -> Task ()
	add uid dev = upd (\(Room i n ds) -> Room i n [(createUnit uid dev):ds]) (sdsFocus rid roomSh) @! ()
	createUnit :: Int MTaskDevice -> Unit
	createUnit i d = {uId = i, uName = name, uDev = d, uStatus = True, uTasks = []}

editUnit :: Unit -> Task ()
editUnit u = forever $ get (unitData u) >>* [OnValue (hasValue editInfo)]
where
	editInfo :: DeviceData -> Task ()
	editInfo dd = updateSharedInformation title [UpdateAs (\v -> v.uName) putName] (sdsFocus u.uId unitSh) @! ()
	where
		title = Title $ "Edit unit #" +++ toString u.uId
		putName :: Unit String -> Unit
		putName u n = {u & uName = n}

viewUnit :: Unit -> Task ()
viewUnit u = forever $ get (unitData u) >>* [OnValue (hasValue showInfo)]
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
			viewDevTask i b = viewInformation ("Task " +++ toString i) [] b
				>>* [OnAction ActionDelete (always (deleteTask i))]
			deleteTask :: Int -> Task ()
			deleteTask i = sendMessages (unitChannels u) [MTTaskDel i] @! ()
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

disconnectUnit :: Unit -> Task ()
disconnectUnit u = upd (\u -> {u & uStatus = False}) (sdsFocus u.uId unitSh)
	>| disconnectDevice u.uDev @! ()

deleteUnit :: Unit -> Task ()
deleteUnit u = disconnectUnit u
	>| get house 
	>>= \h -> set (actualDelete h) house @! ()
where
	actualDelete :: House -> House
	actualDelete rs = map (\(Room i n us) -> (Room i n (delete u us))) rs

manageUnits :: Task ()
manageUnits = forever $ enterChoiceWithShared "Choose a unit" [ChooseFromList \u -> u.uName] allUnits
	>>* [OnAction (Action "View Tasks") (hasValue viewUnit),
	     OnAction (Action "Send Task") (hasValue sendNewProgram),
	     OnAction (Action "Disconnect") (hasValue disconnectUnit),
	     OnAction ActionDelete (hasValue deleteUnit)]

enterTaskDetails :: Task (MTaskInterval, Migration)
enterTaskDetails = updateInformation "Choose Interval" [] (OnInterval 1000)
	>>= \i -> updateInformation "Migration information" [] SameRoom
	>>= \m -> return (i,m)

sendNewProgram :: Unit -> Task ()
sendNewProgram u = getSpec u
	>>= \ds -> enterChoice "Choose Task" [ChooseFromList \p->p.Program.title] (programsBySpec ds)
	>>= \pt -> pt.fill
	>>= \(ix,args) -> enterTaskDetails
	>>- \(int,m)
		# pi = {pIx = ix, pArgs = args, pInt = int, pMig = m}
		= sendProgramToUnit pi u
where
	getSpec :: Unit -> Task MTaskDeviceSpec
	getSpec u = get (unitData u) 
		>>= \dd -> case dd.deviceSpec of
			Nothing = throw "Device does not have specs!"
			Just ds = return ds

sendProgramToUnit :: ProgramInstance Unit -> Task ()
sendProgramToUnit pi u
	# p = programs !! pi.pIx
	= upd (\u -> {u & uTasks = [pi:u.uTasks]}) (sdsFocus u.uId unitSh)
		>| p.send u.uDev pi

filterCompUnits :: Program [Unit] -> Task [Unit]
filterCompUnits p us = allTasks (map (compatible p.req) us)
	>>= \up -> return $ map fst $ filter snd up
where
	compatible :: (Main (Requirements () Stmt)) Unit -> Task (Unit, Bool)
	compatible r u = get (unitData u)
		>>= \dd -> case dd.deviceSpec of
			Nothing = throw "Device does not have specs!"
			Just ds = return (u, match r ds)

migrateTasks :: RoomId UnitId -> Task ()
migrateTasks rid uid = traceValue ("Trying to migrate from devices with id " +++ toString uid) 
	>| get (sdsFocus rid roomSh)
	>>= \r -> get (sdsFocus uid unitSh)
	>>= \u -> catchAll 
		(allTasks (map (migrateTask r u) u.uTasks) @! ())
		(\_ -> traceValue "Can't migrate unit tasks" @! ())
	>| deleteUnit u
where
	migrateTask :: Room Unit ProgramInstance -> Task ()
	migrateTask _ _ {pMig = DoNotMigrate} = return ()
	migrateTask (Room _ _ us) u t = migrate u t us
	migrateTask _ u t = get allUnits >>= migrate u t
	migrate :: Unit ProgramInstance [Unit] -> Task ()
	migrate _ _ [] = throw "Cant migrate task. No other compatible units were found"
	migrate u pi us 
		# p = programs!!pi.pIx
		= filterCompUnits p (filter ((/=)u) us)
			>>= \us -> case us of
				[] = throw "Cant migrate task. No other compatible units were found"
				[u:_] = sendProgramToUnit pi u


