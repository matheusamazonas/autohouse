implementation module AutoHouse

import iTasks
import Interpret.Device
// import AutoHouse.Programs

derive class iTask Room,Unit

main :: Task Room
main = withShared defaultValue manageHouse

manageHouse :: (Shared House) -> Task Room
manageHouse sh = enterChoiceWithShared "Rooms" [] sh
	>>* [OnAction ActionEdit (hasValue manageRoom)]

manageRoom :: Room -> Task Room
manageRoom r=:(Room i n us) = viewInformation ("Room " +++ n) [] r

Start w = startEngine main w