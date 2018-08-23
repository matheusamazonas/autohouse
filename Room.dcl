definition module Room

import iTasks
from Unit import :: Unit

instance == Room
instance toString Room

derive class iTask Room

:: Room = Room Int String [Unit]

nextRoomId :: Shared Int
roomSh :: SDS Room Room Room
newRoom :: Task ()
editRoom :: Room -> Task ()