implementation module Programs

import StdInt, StdBool, StdTuple, StdMisc
import Data.Func

import iTasks

import Language
import Code     //!?!?!?!?
import Interpret
import Specification
import Peripheral.Pin
import Peripheral.Temperature

class program v | arith, IF, seq, boolExpr, noOp, vari, IF, digitalIO, analogIO, temperature, sdspub, iTasksSds, assign, retrn v

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

sendFactorial :: MTaskDevice MTaskInterval -> Task ()
sendFactorial dev i = withShared 1 \result -> (updateInformation "Faculty of what" [] 4
	-&&- updateInformation "LED to light up" [] D13)
	>>= \(x,p) -> liftmTask dev i (factorial result x p) @! ()

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(Int,String)]
programsBySpec Nothing = abort "Device doesnt have a specification"
programsBySpec (Just spec) = map (\(a,b,_) -> (a,b)) $ filter ((checkSpec spec) o thd3) programs
where
	programs :: [(Int,String,Main (Specification () Stmt))]
	programs = [(0,"Factorial",factorial undef undef undef)]

programTasks :: [MTaskDevice MTaskInterval -> Task ()]
programTasks = [sendFactorial]