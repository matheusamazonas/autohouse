implementation module Programs

import StdInt, StdBool, StdTuple

import iTasks

import Language
import Code     //!?!?!?!?
import Interpret
import Peripheral.Pin
import Peripheral.Temperature

thermostat :: Temperature AnalogPin DigitalPin DigitalPin -> Main (ByteCode () Stmt)
thermostat target tp hp ap = vari \t=(Temp 0) In { main =
	setTempPin tp :.
	t =. getTemp :.
	IF (t <. lit target)
		(digitalWrite hp (lit True) :.
		 digitalWrite ap (lit False))
		(IF (t >. lit target)
			(digitalWrite hp (lit False) :.
			 digitalWrite ap (lit True))
			noOp
		)
	}

fillThermostat :: Task (Main (ByteCode () Stmt))
fillThermostat = (updateInformation "Target Temperature" [] (Temp 21)
	-&&- updateInformation "Temperature Pin" [] A0
	-&&- updateInformation "Heater Pin" [] D0
	-&&- updateInformation "AC Pin" [] D1)
	>>= \(target,(tp,(hp,ac))) -> return (thermostat target tp hp ac)

faculty :: (Shared Int) Int DigitalPin -> Main (ByteCode () Stmt)
faculty sh i p = vari \y=i In sds \x=sh In {main =
	IF (y <=. lit 1) (
		digitalWrite p (lit True) :.
		pub x :. 
		digitalWrite p (lit True) :.
		retrn
	) (
		x =. x *. y :.
		y =. y -. lit 1
	)}

