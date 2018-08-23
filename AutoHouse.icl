module AutoHouse

from System.Time import :: Timespec {..}

import iTasks
import Interpret.Device.Simulator

import Default
import House
import Unit
import Room

main :: Task [()]
main = allTasks 
	[
	(manageHouse house) <<@ Title "Manage House",
	(manageUnits <<@ Title "Manage Units"),
	newTask <<@ Title "New Task"
	] <<@ ArrangeWithTabs False

Start :: *World -> *World
Start world = doTasksWithOptions
		(\cli options.defaultEngineCLIOptions cli {options & sessionTime = {tv_sec = 1000000000, tv_nsec=0}})
		[
			onRequest "/" main,
			onRequest "/simulators" viewSims
		] world
