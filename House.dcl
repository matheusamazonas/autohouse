definition module House

import iTasks
from Unit import :: Unit
from Room import :: Room

:: House :== [Room]

house :: Shared House
manageHouse :: (Shared House) -> Task ()
allUnits :: SDS () [Unit] [Room]
newTask :: Task ()