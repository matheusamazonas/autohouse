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
	>>= \us -> enterChoice "Choose unit" [ChooseFromList getUnitName] us
	>>= \(Unit _ _ d _) -> (programs !! ix).send d i
where
	compUnits :: (Main (Requirements () Stmt)) -> Task [Unit]
	compUnits r = get allUnits
		>>= \us -> allTasks (map (compatible r) us)
		>>= \up -> return $ map fst $ filter snd up