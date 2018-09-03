definition module Room

import iTasks
from Unit import :: Unit

:: RoomId :== Int

:: Room = Room Int String [Unit]

instance == Room
instance toString Room

derive class iTask Room

roomSh :: SDS RoomId Room Room
newRoom :: Task ()
editRoom :: Room -> Task ()