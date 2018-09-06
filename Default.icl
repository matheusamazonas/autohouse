implementation module Default

import TTY
import Interpret.Device.TCP
import Interpret.Device.Simulator

defaultSimulator :: SimSettings
defaultSimulator = SimSettings 
			{ haveLed       = True
			, haveLCD       = False
			, haveHb        = True
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

defaultTCP :: TCPSettings
defaultTCP = {host = "localhost", port=8123}

defaultSerial :: TTYSettings
defaultSerial = {zero & devicePath = "/dev/tty.usbmodem1421", xonxoff=True} 

defaultBT :: TTYSettings
defaultBT = {zero & devicePath = "/dev/tty.HC-05-01-DevB", xonxoff=True}