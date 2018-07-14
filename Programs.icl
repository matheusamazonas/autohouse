implementation module Programs

import StdInt, StdBool, StdTuple, StdMisc
import Data.Func

import iTasks

import Language
import Code     //!?!?!?!?
import Interpret
import Specification
import Peripheral.LED
import Peripheral.Pin
import Peripheral.Temperature

class program v | arith, IF, seq, boolExpr, noOp, vari, IF, digitalIO, analogIO, temperature, sdspub, iTasksSds, assign, retrn, userLed v

thermostat :: Temperature AnalogPin DigitalPin DigitalPin -> Main (v () Stmt) | program v
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

factorial :: (Shared Int) Int DigitalPin -> Main (v () Stmt) | program v
factorial sh i p = vari \y=i In sds \x=sh In {main =
	IF (y <=. lit 1) (
		pub x :. 
		digitalWrite p (lit True) :.
		retrn
	) (
		x =. x *. y :.
		y =. y -. lit 1
	)}

switch :: Main (v () Stmt) | program v
switch = { main = 
	IF (digitalRead D0) (
		ledOn (lit LED1)
	) (
		ledOff (lit LED1)
	)}

curtains :: (Shared Bool) -> Main (v () Stmt) | program v
curtains sh = sds \alarm=sh In { main = 
	IF (analogRead A0 >. lit 3) (
		digitalWrite D0 (lit True) :.
		alarm =. lit True :.
		pub alarm :.
		retrn
	) (
		digitalWrite D0 (lit False)
	)}

sendFactorial :: MTaskDevice MTaskInterval -> Task ()
sendFactorial dev i = withShared 1 \result -> (updateInformation "Faculty of what" [] 4
	-&&- updateInformation "LED to light up" [] D13)
	>>= \(x,p) -> liftmTask dev i (factorial result x p)

sendSwitch :: MTaskDevice MTaskInterval -> Task ()
sendSwitch dev i = liftmTask dev i switch

sendCurtains :: MTaskDevice MTaskInterval -> Task ()
sendCurtains dev i = withShared False \result -> liftmTask dev i (curtains result)

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(Int,String)]
programsBySpec Nothing = abort "Device doesnt have a specification"
programsBySpec (Just spec) = map (\(a,b,_) -> (a,b)) $ filter ((checkSpec spec) o thd3) programs
where
	programs :: [(Int,String,Main (Specification () Stmt))]
	programs = [(0,"Factorial",factorial undef undef undef),
	            (1, "Switch", switch),
	            (2, "Curtains", curtains undef)]

programTasks :: [MTaskDevice MTaskInterval -> Task ()]
programTasks = [sendFactorial, sendSwitch, sendCurtains]