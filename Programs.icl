implementation module AutoHouse.Programs

import Language

regulateTemperature :: Int AnalogPin DigitalPin DigitalPin -> Main (ByteCode () Stmt)
regulateTemperature it tp hp acp = sds \t=0 In In { main=
		t =. analogRead p :.
		IF (t < it)
			(digitalWrite hp True :. digitalWrite acp False)
			(IF (t > it)
				(digitalWrite hp False :. digitalWrite acp True)
				noOp)
	}


