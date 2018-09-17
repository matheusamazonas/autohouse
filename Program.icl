implementation module Program

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

class program v | arith, seq, boolExpr, noOp, vari, IF, dIO, aIO, dht22, hcsr04, pir, lightSensorDig, lightSensorAna, servo, sdspub, sds, assign, retrn, userLed v

derive class iTask Program, Requirements, ProgramInstance, Migration

gDefault{|Dynamic|} = dynamic ()

on :== lit True
off :== lit False

fromDynamic :: Dynamic -> a | TC a
fromDynamic (x :: a^) = x
fromDynamic _ = abort "Dynamic is from unexpected type"

thermostat :: (Shared Temperature) -> Main (v () Stmt) | program v
thermostat g = sds \goal=g In vari \temp=0 In { main =
	temp =. getTemp :.
	dIO D7 ? goal =. goal -. lit 1000 :.
	dIO D8 ? goal =. goal +. lit 1000 :.
	IF (temp <. goal)
	(
		heater =. on :.
		ac =. off
	) (
		heater =. off :.
		ac =. on
	) :.
	pub goal :. noOp
	}
where
	heater = dIO D12
	ac = dIO D13

switch :: DigitalPin -> Main (v () Stmt) | program v
switch p = { main = 
	IF (dIO p) (
		ledOn (lit LED1)
	) (
		ledOff (lit LED1)
	)}

curtains :: Int -> Main (v () Stmt) | program v
curtains t = vari \i=0 In { main = 
	IF (getBrightness >. lit t) (
		i <. (lit 36) ? (
			i =. i +. lit 1 :.
			writeAngle (i *. (lit 5))
		)
	) (
		i >. (lit 0) ? (
			i =. i -. lit 1 :.
			writeAngle (i *. (lit 5))
		)
	)}

movSwitch :: Main (v () Stmt) | program v
movSwitch = { main = 
	IF (isMoving) (
		dIO D12 =. on
	) ( 
		dIO D12 =. off
	)}

lockWhenDark :: Int -> Main (v () Stmt) | program v
lockWhenDark t = { main = 
	getBrightness <. lit t ? dIO D10 =. on
	}

fanWhenHumid :: Main (v () Stmt) | program v
fanWhenHumid = { main = 
	IF ((getHumid >. lit 5000) &.(getDistance >. lit 10)) (
		dIO D10 =. on
	) (
		dIO D10 =. off
	)}

factorial :: (Shared Int) Int UserLED -> Main (v () Stmt) | program v
factorial sh i l = vari \y=i In sds \x=sh In {main =
	IF (y <=. lit 1) (
		pub x :. 
		ledOn (lit l) :.
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
			writeAngle (lit 0)
		) :.
		s =. (Not s) :. noOp
	}

blink :: Main (v () Stmt) | program v
blink = vari \v=False In { main =
	IF (v) (
		ledOn (lit LED1)
	) (
		ledOff (lit LED1)
	) :.
	v =. Not v :. noOp
	}

buttonTest :: Main (v () Stmt) | program v
buttonTest = { main = 
	dIO D8 ? ledOn (lit LED1) :.
	dIO D7 ? ledOff (lit LED1)
	}

garageDoor :: Int -> Main (v () Stmt) | program v
garageDoor x = vari \open=False In { main = 
	IF (getDistance <. lit x) (
		dIO D10 =. on :.
		ledOn (lit LED1)
	) (
		dIO D10 =. off :.
		ledOff (lit LED1)
	)}

controlWindows :: Temperature Temperature -> Main (v () Stmt) | program v
controlWindows target error = vari \t=0 In vari \i=0 In { main = 
	t :. getTemp :.
	IF ((t <=. (lit target) +. (lit error)) |. (t <=. (lit target) -. (lit error))) (
		i <. (lit 90) ? (
			i =. i +. lit 1 :.
			writeAngle (i *. (lit 2))
		)
	) (
		i >. (lit 0) ? (
			i =. i -. lit 1 :.
			writeAngle (i *. (lit 2))
		)
	)}

programDataError :: String -> Task ()
programDataError name = throw $ "Trying to send " +++ name +++ " program with wrong TaskData values"

fillThermostat :: Task ProgramData
fillThermostat = updateInformation "Target temperature" [] 2100
	>>= \goal -> return (0, [dynamic goal])

sendThermostat :: MTaskDevice ProgramInstance -> Task ()
sendThermostat dev {pIx=0, pArgs=[dx:[]], pInt=i} = withShared (fromDynamic dx) \goal -> liftmTask dev i (thermostat goal)
sendThermostat _ _ = programDataError "thermostat"

fillSwitch :: Task ProgramData
fillSwitch = updateInformation "Select switch pin" [] D10
	>>= \p -> return(1, [dynamic p])

sendSwitch :: MTaskDevice ProgramInstance -> Task ()
sendSwitch dev {pIx=1, pArgs=[dp:[]], pInt=i} = liftmTask dev i (switch (fromDynamic dp))
sendSwitch _ _ = programDataError "switch"

fillCurtains :: Task ProgramData
fillCurtains = updateInformation "Choose target brightness (0-100)" [] 50
	>>= \b -> return (2,[dynamic b])

sendCurtains :: MTaskDevice ProgramInstance -> Task ()
sendCurtains dev {pIx=2, pArgs=[b:[]], pInt=i} = liftmTask dev i (curtains (fromDynamic b))
sendCurtains _ _ = programDataError "curtains"

fillMovSwitch :== return (3,[])

sendMovSwitch :: MTaskDevice ProgramInstance -> Task ()
sendMovSwitch dev pi=:{pIx=3, pArgs=[], pInt=i} = liftmTask dev i movSwitch
sendMovSwitch _ _ = programDataError "movSwitch"

fillFactorial :: Task ProgramData
fillFactorial = updateInformation "Faculty of what" [] 4
	>>= \x -> updateInformation "LED to light up" [] LED1
	>>= \l -> return (4,[dynamic x, dynamic l])

sendFactorial :: MTaskDevice ProgramInstance-> Task ()
sendFactorial dev {pIx=4, pArgs=[x,l:[]], pInt=i} = withShared 1 \result -> liftmTask dev i (factorial result (fromDynamic x) (fromDynamic l))
sendFactorial _ _ = programDataError "factorial"

fillShareTemp :== return (5,[])

sendShareTemp :: MTaskDevice ProgramInstance -> Task ()
sendShareTemp dev {pIx=5, pArgs=[], pInt=i} = withShared 2100 \tsh -> liftmTask dev i (shareTemp tsh)
sendShareTemp _ _ = programDataError "shareTemp"

fillShareHumi :== return (6,[])

sendShareHumi :: MTaskDevice ProgramInstance -> Task ()
sendShareHumi dev {pIx=6, pArgs=[], pInt=i} = withShared 5142 \hsh -> liftmTask dev i (shareHumid hsh)
sendShareHumi _ _ = programDataError "shareHumi"

fillShareDist :== return (7,[])

sendShareDist :: MTaskDevice ProgramInstance -> Task ()
sendShareDist dev {pIx=7, pArgs=[], pInt=i} = withShared 42 \dsh -> liftmTask dev i (shareDistance dsh)
sendShareDist _ _ = programDataError "shareDist"

fillShareMov :== return (8,[])

sendShareMov :: MTaskDevice ProgramInstance -> Task ()
sendShareMov dev {pIx=8, pArgs=[], pInt=i} = withShared False \msh -> liftmTask dev i (shareMov msh)
sendShareMov _ _ = programDataError "shareMov"

fillShareBrightDig :== return (9,[])

sendShareBrightDig :: MTaskDevice ProgramInstance -> Task ()
sendShareBrightDig dev {pIx=9, pArgs=[], pInt=i} = withShared False \bsh -> liftmTask dev i (shareBrightDig bsh)
sendShareBrightDig _ _ = programDataError "shareBrightDig"

fillShareBrightAna :== return (10,[])

sendShareBrightAna :: MTaskDevice ProgramInstance -> Task ()
sendShareBrightAna dev {pIx=10, pArgs=[], pInt=i} = withShared 0 \bsh -> liftmTask dev i (shareBrightAna bsh)
sendShareBrightAna _ _ = programDataError "shareBrightAna"

fillServoSwitch :== return (11,[])

sendServoSwitch :: MTaskDevice ProgramInstance -> Task ()
sendServoSwitch dev {pIx=11, pArgs=[], pInt=i} = liftmTask dev i servoSwitch
sendServoSwitch _ _ = programDataError "servoSwitch"

fillBlink :== return (12,[])

sendBlink :: MTaskDevice ProgramInstance -> Task ()
sendBlink dev {pIx=12, pArgs=[], pInt=i} = liftmTask dev i blink
sendBlink _ _ = programDataError "blink"

fillButtonTest :== return (13,[])

sendButtonTest :: MTaskDevice ProgramInstance -> Task ()
sendButtonTest dev {pIx=13, pArgs=[], pInt=i} = liftmTask dev i buttonTest
sendButtonTest _ _ = programDataError "buttonTest"

fillGarageDoor :: Task ProgramData
fillGarageDoor = updateInformation "Minimal distance in cm" [] 300
	>>= \d -> return (14, [dynamic d])

sendGarageDoor :: MTaskDevice ProgramInstance -> Task ()
sendGarageDoor dev {pIx=14, pArgs=[dd:[]], pInt=i} = liftmTask dev i (garageDoor (fromDynamic dd))
sendGarageDoor _ _ = programDataError "garageDoor"

fillControlWindows :: Task ProgramData
fillControlWindows = updateInformation "Target temperature" [] 2100
	>>= \t -> updateInformation "Temperature error" [] 500
	>>= \e -> return (15, [dynamic t, dynamic e])

sendControlWindows :: MTaskDevice ProgramInstance -> Task ()
sendControlWindows dev {pIx=15, pArgs=[dt,de:[]], pInt=i} = liftmTask dev i (controlWindows (fromDynamic dt) (fromDynamic de))
sendControlWindows _ _= programDataError "controlWindows"

programsBySpec :: (Maybe MTaskDeviceSpec) -> [Program]
programsBySpec Nothing = abort "Device doesnt have a Compatibility"
programsBySpec spec = filter (\p -> match p.req spec) programs

programs :: [Program]
programs = [
	{pId =  0, title = "Thermostat", req = thermostat undef, fill = fillThermostat, send = sendThermostat},
	{pId =  1, title = "Switch", req = switch undef, fill = fillSwitch, send = sendSwitch},
	{pId =  2, title = "Curtains", req = curtains undef, fill = fillCurtains, send = sendCurtains},
	{pId =  3, title = "Movement switch", req = movSwitch, fill = fillMovSwitch, send = sendMovSwitch},
	{pId =  4, title = "Factorial", req = factorial undef undef undef, fill = fillFactorial, send = sendFactorial},
	{pId =  5, title = "Share temperature", req = shareTemp undef, fill = fillShareTemp, send = sendShareTemp},
	{pId =  6, title = "Share humidity", req = shareHumid undef, fill = fillShareHumi, send = sendShareHumi},
	{pId =  7, title = "Share distance", req = shareDistance undef, fill = fillShareDist, send = sendShareDist} ,
	{pId =  8, title = "Share movement", req = shareMov undef, fill = fillShareMov, send = sendShareMov},
	{pId =  9, title = "Shared digital brightness", req = shareBrightDig undef, fill = fillShareBrightDig, send = sendShareBrightDig},
	{pId = 10, title = "Shared analog brightness", req = shareBrightAna undef, fill = fillShareBrightAna, send = sendShareBrightAna},
	{pId = 11, title = "Servo switch", req = servoSwitch, fill = fillServoSwitch, send = sendServoSwitch},
	{pId = 12, title = "Blink", req = blink, fill = fillBlink, send = sendBlink},
	{pId = 13, title = "Button test", req = buttonTest, fill = fillButtonTest, send = sendButtonTest},
	{pId = 14, title = "Garage door", req = garageDoor undef, fill = fillGarageDoor, send = sendGarageDoor},
	{pId = 15, title = "Control windows", req = controlWindows undef undef, fill = fillControlWindows, send = sendControlWindows}]

programIndex :: [(Int,String)]
programIndex = map (\p -> (p.pId, p.Program.title)) programs

