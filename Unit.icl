implementation module Unit

import Data.List
from Data.Func import $
import qualified Data.Map as DM

import iTasks
import Interpret
from TTY import :: BaudRate, :: Parity, :: ByteSize

import House
import Room
import Programs
import Default

derive class iTask Unit, BaudRate, Parity, ByteSize, DeviceData, TTYSettings, BCState

gDefault{|Dynamic|} = dynamic ()

instance == Unit where
	(==) u1 u2 = u1.uId == u2.uId

instance toString Unit where
	toString u = u.uName +++ " (" +++ toString u.uId +++ ")"

unitData :: Unit -> Shared DeviceData
unitData {uDev=(Device dd _)} = dd

nextUnitId :: Shared Int
nextUnitId = sharedStore "nextUnitId" 0

unitSh :: SDS Unit Unit Unit
unitSh = sdsLens "room" (const ()) (SDSRead r) (SDSWrite w) (SDSNotifyConst n) house
where
	r :: Unit House -> MaybeError TaskException Unit
	r _ [] = Error (exception "There are no rooms in the house")
	r u [(Room _ _ us):rs] = case find ((==)u) us of
		Just u = Ok u
		Nothing = r u rs
	w :: Unit House Unit -> MaybeError TaskException (Maybe House)
	w _ [] _ = Error (exception "There are no rooms in the house")
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
	n :: Unit Unit -> SDSNotifyPred Unit
	n u1 _ = \_ u2 -> u1 == u2

addUnit :: Room String a -> Task () | channelSync, iTask a
addUnit r name dev
	# rSh = sdsFocus r roomSh
	= upd ((+)1) nextUnitId
	>>= \i = ((withDevice dev) (\d -> upd (\(Room i n ds) -> Room i n [(add i d):ds]) rSh)) @! ()
where
	add :: Int MTaskDevice -> Unit
	add i d = {uId = i, uName = name, uDev = d, uStatus = True, uTasks = []}

newUnit :: Room -> Task ()
newUnit r = enterInformation "Device name" []
	>>= \name -> enterChoiceAs "Choose the device type" [ChooseFromDropdown snd] dTypes fst
	>>= saveUnit name
where
	dTypes :: [(Int, String)]
	dTypes = [(0, "Simulator"),
	          (1, "TCP"),
	          (2, "Serial")]
	saveUnit :: String Int -> Task ()
	saveUnit name 0 = newSimulator >>= addUnit r name
	saveUnit name 1 = newTCP       >>= addUnit r name
	saveUnit name 2 = newSerial    >>= addUnit r name
	newSerial :: Task TTYSettings
	newSerial = updateInformation "Serial port settings" [] defaultSerial
	newTCP :: Task TCPSettings
	newTCP = updateInformation "TCP settings" [] defaultTCP
	newSimulator :: Task SimSettings
	newSimulator = updateInformation "Simulator settings" [] defaultSimulator

editUnit :: Unit -> Task ()
editUnit u = forever $ get (unitData u) >>* [OnValue (hasValue editInfo)]
where
	editInfo :: DeviceData -> Task ()
	editInfo dd = updateSharedInformation title [UpdateAs (\v -> v.uName) putName] (sdsFocus u unitSh) @! ()
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

deleteUnit :: Unit -> Task ()
deleteUnit u = upd (\u -> {u & uStatus = False}) (sdsFocus u unitSh)
	>| disconnectDevice u.uDev
	>| get house 
	>>= \h -> set (actualDelete h) house @! ()
where
	actualDelete :: House -> House
	actualDelete rs = map (\(Room i n us) -> (Room i n (delete u us))) rs

manageUnits :: Task ()
manageUnits = forever $ enterChoiceWithShared "Choose a unit" [ChooseFromList \u -> u.uName] allUnits
	>>* [OnAction (Action "View Tasks") (hasValue viewUnit),
	     OnAction (Action "Send Task") (hasValue sendTask),
	     OnAction ActionDelete (hasValue deleteUnit)]

chooseInterval :: Task MTaskInterval
chooseInterval = updateInformation "Choose Interval" [] (OnInterval 1000)

sendTask :: Unit -> Task ()
sendTask u = getSpec u
	>>= \ds -> enterChoice "Choose Task" [ChooseFromList fst] (programsBySpec ds)
	>>= \(_,pt) -> chooseInterval
	>>= \i -> upd (\u -> {u & uTasks = u.uTasks}) (sdsFocus u unitSh)// TODO: ACTUALLY ADD
	>| pt u.uDev i
where
	getSpec :: Unit -> Task (Maybe MTaskDeviceSpec)
	getSpec u = get (unitData u) >>= \dd -> return dd.deviceSpec

compatible :: (Main (Requirements () Stmt)) Unit -> Task (Unit, Bool)
compatible r u = get (unitData u)
	>>= \dd -> return (u, match r dd.deviceSpec)
