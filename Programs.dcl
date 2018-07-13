definition module Programs

import iTasks
import Interpret
import Specification
from Peripheral.LED import class userLed

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(Int,String)]
programTasks :: [MTaskDevice MTaskInterval -> Task ()]
