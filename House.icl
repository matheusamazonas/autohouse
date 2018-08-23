implementation module House

import Data.Maybe, Data.List
from Data.Func import $
from Data.Foldable import class Foldable, concat

import iTasks
import iTasks.Internal.Store

import Room
import Unit
import Programs

house :: Shared House
house = sdsFocus "AutoHouse" $ memoryStore "house" (Just [Room 0 "Living room" []])

manageHouse :: (Shared House) -> Task ()
manageHouse sh = forever $ enterChoiceWithShared "Rooms" [ChooseFromList \(Room _ n _) -> n] sh
	>>* [OnAction ActionEdit (hasValue editRoom),
	     OnAction ActionNew (always newRoom)]

allUnits :: SDS () [Unit] [Room]
allUnits = mapRead (concat o (map getUnits)) house
where
	getUnits :: Room -> [Unit]
	getUnits r=:(Room _ _ us) = us

newTask :: Task ()
newTask = forever $ enterChoice "Choose Task" [ChooseFromList snd] programIndex
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