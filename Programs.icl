implementation module Programs

import StdInt, StdBool, StdTuple, StdMisc
import Data.Func

import iTasks

import Language
import Code
import Interpret
import Requirements
import Peripheral.LED
import Peripheral.Pin
import Peripheral.DHT22
import Peripheral.HCSR04
import Peripheral.PIR
import Peripheral.LightSensorDig
import Peripheral.LightSensorAna
import Peripheral.Servo

class program v | arith, seq, boolExpr, noOp, vari, IF, dIO, aIO, dht22, hcsr04, pir, lightSensorDig, lightSensorAna, servo, sdspub, iTasksSds, assign, retrn, userLed v

on :== lit True
off :== lit False

thermostat :: Temperature -> Main (v () Stmt) | program v
thermostat target = vari \t=0 In { main =
	t =. getTemp :.
	IF (t <. lit target)
	(
		dIO D0 =. on :.
		dIO D1 =. off
	) (
		dIO D0 =. off :.
		dIO D1 =. on
	)}

switch :: Main (v () Stmt) | program v
switch = { main = 
	IF (dIO D0) (
		ledOn (lit LED1)
	) (
		ledOff (lit LED1)
	)}

curtains :: Int -> Main (v () Stmt) | program v
curtains t = { main = 
	IF (getBrightness >. lit t) (
		dIO D0 =. on
	) (
		dIO D0 =. off
	)}

movSwitch :: Main (v () Stmt) | program v
movSwitch = { main = 
	IF (isMoving) (
		dIO D0 =. on
	) ( 
		dIO D0 =. off
	)}

lockWhenDark :: Int -> Main (v () Stmt) | program v
lockWhenDark t = { main = 
	getBrightness <. lit t ? dIO D1 =. on
	}

fanWhenHumid :: Main (v () Stmt) | program v
fanWhenHumid = { main = 
	IF (getHumid >. lit 5000) (
		dIO D8 =. on
	) (
		dIO D8 =. off
	)}

factorial :: (Shared Int) Int DigitalPin -> Main (v () Stmt) | program v
factorial sh i p = vari \y=i In sds \x=sh In {main =
	IF (y <=. lit 1) (
		pub x :. 
		dIO p =. on :.
		retrn
	) (
		x =. x *. y :.
		y =. y -. lit 1
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

shareMov :: (Shared Bool) -> Main (v () Stmt) | program v
shareMov sh = sds \m=sh In { main =
	m =. isMoving :.
	pub m :. noOp
	}

shareBrightDig :: (Shared Bool) -> Main (v () Stmt) | program v
shareBrightDig sh = sds \b=sh In { main =
	b =. isBright :.
	pub b :. noOp
	}

shareBrightAna :: (Shared Int) -> Main (v () Stmt) | program v
shareBrightAna sh = sds \b=sh In { main =
	b =. getBrightness :.
	pub b :. noOp
	}

servoSwitch ::  Main (v () Stmt) | program v
servoSwitch = vari \s=True In { main =
		IF (s) (
			writeAngle (lit 180)
		) (
			writeAngle (lit 0) :.
			s =. (Not s)
		)
	}

sendThermostat :: MTaskDevice MTaskInterval -> Task ()
sendThermostat dev i = updateInformation "Target temperature" [] 2100
	>>= \temp -> liftmTask dev i (thermostat temp)

sendFactorial :: MTaskDevice MTaskInterval -> Task ()
sendFactorial dev i = withShared 1 \result -> (updateInformation "Faculty of what" [] 4
	-&&- updateInformation "LED to light up" [] D13)
	>>= \(x,p) -> liftmTask dev i (factorial result x p)

sendSwitch :: MTaskDevice MTaskInterval -> Task ()
sendSwitch dev i = liftmTask dev i switch

sendCurtains :: MTaskDevice MTaskInterval -> Task ()
sendCurtains dev i = updateInformation "Choose target brightness (0-100)" [] 50 
	>>= \t -> liftmTask dev i (curtains t)

sendMovSwitch :: MTaskDevice MTaskInterval -> Task ()
sendMovSwitch dev i = liftmTask dev i movSwitch

sendShareTemp :: MTaskDevice MTaskInterval -> Task ()
sendShareTemp dev i = withShared 2100 \tsh -> liftmTask dev i (shareTemp tsh)

sendShareHumi :: MTaskDevice MTaskInterval -> Task ()
sendShareHumi dev i = withShared 5142 \hsh -> liftmTask dev i (shareHumid hsh)

sendShareDist :: MTaskDevice MTaskInterval -> Task ()
sendShareDist dev i = withShared 42 \dsh -> liftmTask dev i (shareDistance dsh)

sendShareMov :: MTaskDevice MTaskInterval -> Task ()
sendShareMov dev i = withShared False \msh -> liftmTask dev i (shareMov msh)

sendShareBrightDig :: MTaskDevice MTaskInterval -> Task ()
sendShareBrightDig dev i = withShared False \bsh -> liftmTask dev i (shareBrightDig bsh)

sendShareBrightAna :: MTaskDevice MTaskInterval -> Task ()
sendShareBrightAna dev i = withShared 0 \bsh -> liftmTask dev i (shareBrightAna bsh)

sendServoSwitch :: MTaskDevice MTaskInterval -> Task ()
sendServoSwitch dev i = liftmTask dev i servoSwitch

programsBySpec :: (Maybe MTaskDeviceSpec) -> [(String, MTaskDevice MTaskInterval -> Task ())]
programsBySpec Nothing = abort "Device doesnt have a Compatibility"
programsBySpec spec = map (\p -> (p.Program.title,p.send)) $ filter (\p -> match p.req spec) programs
where
	snd4 :: (a,b,c,d) -> b
	snd4 (_,x,_,_) = x

programs :: [Program]
programs = [
	{pId =  0, title = "Thermostat", req = thermostat undef, send = sendThermostat},
	{pId =  1, title = "Switch", req = switch, send = sendSwitch},
	{pId =  2, title = "Curtains", req = curtains undef, send = sendCurtains},
	{pId =  3, title = "Movement switch", req = movSwitch, send = sendMovSwitch},
	{pId =  4, title = "Factorial", req = factorial undef undef undef, send = sendFactorial},
	{pId =  5, title = "Share temperature", req = shareTemp undef, send = sendShareTemp},
	{pId =  6, title = "Share humidity", req = shareHumid undef, send = sendShareHumi},
	{pId =  7, title = "Share distance", req = shareDistance undef, send = sendShareDist} ,
	{pId =  8, title = "Share movement", req = shareMov undef, send = sendShareMov},
	{pId =  9, title = "Shared digital brightness", req = shareBrightDig undef, send = sendShareBrightDig},
	{pId = 10, title = "Shared analog brightness", req = shareBrightAna undef, send = sendShareBrightAna},
	{pId = 11, title = "Servo switch", req = servoSwitch, send = sendServoSwitch}]

programIndex :: [(Int,String)]
programIndex = map (\p -> (p.pId, p.Program.title)) programs

