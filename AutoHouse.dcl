definition module AutoHouse

import Language
import Interpret.Device

:: House :== [Room]

:: Room = Room Int String [Unit]

:: Unit = Unit Int String MTaskDevice