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

instance == Unit where
	(==) (Unit i1 n1 _) (Unit i2 n2 _) = i1 == i2 && n1 == n2

instance toString Unit where
	toString (Unit i n _) = n +++ " (" +++ toString i +++ ")"

nextUnitId :: Shared Int
nextUnitId = sharedStore "nextUnitId" 0

import StdMisc

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
	>>= \i -> ((withDevice dev) (\d -> upd (\(Room i n ds) -> Room i n [Unit i name d:ds]) rSh -&&- return ())) @! ()

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
editUnit u=:(Unit i _ (Device dsh _)) = forever $ get dsh >>* [OnValue (hasValue editInfo)]
where
	editInfo :: DeviceData -> Task ()
	editInfo dd = updateSharedInformation title [UpdateAs getName putName] (sdsFocus u unitSh) @! ()
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
manageUnits = forever $ enterChoiceWithShared "Choose a unit" [ChooseFromList \(Unit i n _) -> n] allUnits
	>>* [OnAction (Action "View Tasks") (hasValue viewUnit),
	     OnAction (Action "Send Task") (hasValue sendTask) ]

getSpec :: Unit -> Task (Maybe MTaskDeviceSpec)
getSpec (Unit _ _ (Device ddsh _)) = get ddsh >>= \dd -> return dd.deviceSpec

chooseInterval :: Task MTaskInterval
chooseInterval = updateInformation "Choose Interval" [] (OnInterval 1000)

sendTask :: Unit -> Task ()
sendTask u=:(Unit _ _ d) = getSpec u
	>>= \ds -> enterChoice "Choose Task" [ChooseFromList fst] (programsBySpec ds)
	>>= \(_,pt) -> chooseInterval
	>>= \i -> pt d i




