module Sheet where 

import Rattus.Channels

Data Input = IntValue Int | CharValue Char | BoolVale Bool


(input, inputMaybe, depend, kbChannel, mouseChanel) = mkChannels ["keyboard", "num", "mouse"]