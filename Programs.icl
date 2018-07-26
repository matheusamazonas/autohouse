implementation module Programs

import StdInt, StdBool, StdTuple, StdMisc
import Data.Func

import iTasks

import Language
import Code
import Interpret
import Specification
import Peripheral.LED
import Peripheral.Pin
import Peripheral.DHT22
import Peripheral.HCSR04

class program v | arith, IF, seq, boolExpr, noOp, vari, IF, dIO, aIO, dht22, hcsr04, sdspub, iTasksSds, assign, retrn, userLed v

thermostat :: Temperature -> Main (v () Stmt) | program v
thermostat target = vari \t=(Temp 0.0) In { main =
	t =. getTemp :.
	IF (t <. lit target)
		(dIO D0 =. (lit True) :.
		 dIO D1 =. (lit False))
		(IF (t >. lit target)
			(dIO D0 =. (lit False) :.
			 dIO D1 =. (lit True))
			noOp
		)
	}

factorial :: (Shared Int) Int DigitalPin -> Main (v () Stmt) | program v
factorial sh i p = vari \y=i In sds \x=sh In {main =
	IF (y <=. lit 1) (
		pub x :. 
		dIO p =. (lit True) :.
		retrn
	) (
		x =. x *. y :.
		y =. y -. lit 1
	)}

switch :: Main (v () Stmt) | program v
switch = { main = 
	IF (dIO D0) (
		ledOn (lit LED1)
	) (
		ledOff (lit LED1)
	)}

curtains :: (Shared Bool) -> Main (v () Stmt) | program v
curtains sh = sds \alarm=sh In { main = 
	IF (aIO A0 >. lit 3) (
		dIO D0 =. (lit True) :.
		alarm =. lit True :.
		pub alarm :.
		retrn
	) (
		dIO D0 =. (lit False)
	)}

movSwitch :: Main (v () Stmt) | program v
movSwitch = { main = 
	IF (aIO A0 >. lit 2) (
		dIO D0 =. (lit True)
	) ( 
		dIO D0 =. (lit False)
	)}

shareTemp :: (Shared Temperature) -> Main (v () Stmt) | program v
shareTemp sh = sds \t=sh In { main =
	t =. getTemp :.
	pub t :. noOp
	}

shareHumid :: (Shared Humidity) -> Main (v () Stmt) | program v
shareHumid sh = sds \h=sh In { main = 
	h =. getHumid :.
	pub h :. noOp
	}

shareDistance :: (Shared Distance) -> Main (v () Stmt) | program v
shareDistance sh = sds \d=sh In { main =
	d =. getDistance :.
	pub d :. noOp
	}

sendThermostat :: MTaskDevice MTaskInterval -> Task ()
sendThermostat dev i = updateInformation "Target temperature" [] (Temp 21.0)
	>>= \temp -> liftmTask dev i (thermostat temp)

sendFactorial :: MTaskDevice MTaskInterval -> Task ()
sendFactorial dev i = withShared 1 \result -> (updateInformation "Faculty of what" [] 4
	-&&- updateInformation "LED to light up" [] D13)
	>>= \(x,p) -> liftmTask dev i (factorial result x p)

sendSwitch :: MTaskDevice MTaskInterval -> Task ()
sendSwitch dev i = liftmTask dev i switch

sendCurtains :: MTaskDevice MTaskInterval -> Task ()
sendCurtains dev i = withShared False \result -> liftmTask dev i (curtains result)

sendMovSwitch :: MTaskDevice MTaskInterval -> Task ()
sendMovSwitch dev i = liftmTask dev i movSwitch

sendShareTemp :: MTaskDevice MTaskInterval -> Task ()
sendShareTemp dev i = withShared (Temp 21.0) \tsh -> liftmTask dev i (shareTemp tsh)

sendShareHumi :: MTaskDevice MTaskInterval -> Task ()
sendShareHumi dev i = withShared (Hum 51.42) \hsh -> liftmTask dev i (shareHumid hsh)

sendShareDist :: MTaskDevice MTaskInterval -> Task ()
sendShareDist dev i = withShared 42 \dsh -> liftmTask dev i (shareDistance dsh)

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(Int,String)]
programsBySpec Nothing = abort "Device doesnt have a specification"
programsBySpec (Just spec) = map (\(a,b,_) -> (a,b)) $ filter ((checkSpec spec) o thd3) programs
where
	programs :: [(Int,String,Main (Specification () Stmt))]
	programs = [(0, "Thermostat", thermostat undef),
	            (1, "Factorial",factorial undef undef undef),
	            (2, "Switch", switch),
	            (3, "Curtains", curtains undef),
	            (4, "Movement switch", movSwitch),
	            (5, "Share temperature", shareTemp undef),
	            (6, "Share humidity", shareHumid undef),
	            (7, "Share distance", shareDistance undef) ]

programTasks :: [MTaskDevice MTaskInterval -> Task ()]
programTasks = [
	sendThermostat, 
	sendFactorial, 
	sendSwitch, 
	sendCurtains, 
	sendMovSwitch, 
	sendShareTemp, 
	sendShareHumi,
	sendShareDist
	]

