definition module Room

import iTasks
from Unit import :: Unit

instance == Room
instance toString Room

derive class iTask Room

:: RoomId :== Int

:: Room = Room Int String [Unit]

nextRoomId :: Shared RoomId
roomSh :: SDS RoomId Room Room
newRoom :: Task ()
editRoom :: Room -> Task ()