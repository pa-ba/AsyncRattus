module Sheet where 

import Rattus.Channels
import qualified Rattus.Primitives as Prim
import qualified Data.Set

data Input = IntValue Int | CharValue Char | MouseValue Bool
type O a = Prim.O Input a


-- Identifier, value of cell, dependent on these cells
--type Cell f = (String, f, O Int)
--type Spreadsheet = Map String Cell 


(input, inputMaybe, depend, [kbChannel, mouseChanel]) = mkChannels ["keyboard", "mouse"]

