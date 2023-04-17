module Rattus.Channels (
    mkChannels,
    InputFunc,
) where
import Prelude hiding (Left, Right, lookup)
import Rattus.InternalPrimitives
import qualified Data.Set as Set
import Data.Map (Map, fromList, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Function for providing input to a later
--             Channel name | value | later
type InputFunc a = String -> Value -> O a -> a
type InputChannel = O Value

mkChannelFromId :: InputChannelIdentifier -> InputChannel
mkChannelFromId id = Delay (Set.singleton id) snd

index :: [a] -> [Int]
index = zipWith const [0..]

mkChannels :: [String] -> (InputFunc a, [O Value])
mkChannels names = (inputF, map mkChannelFromId $ index names)
    where nameMapping = constructNameMapping names
          getId name = fromMaybe (error "No such input channel") (lookup name nameMapping)
          inputF name v later = adv' later (getId name, v)

constructNameMapping :: [String] -> Map String Int
constructNameMapping [] = Map.empty
constructNameMapping (name : names) = fromList $ scanl (\(_, i) name -> (name, i+1)) (name, 0) names