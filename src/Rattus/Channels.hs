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
type InputFunc v a = String -> v -> O v a -> a
type InputChannel v = O v v

input :: Map String Int -> String -> v -> O v a -> a
input nameToId name v later = adv' later (id, v)
    where id = fromMaybe (error "No such input channel") (lookup name nameToId)

mkChannelFromId :: InputChannelIdentifier -> InputChannel v
mkChannelFromId id = Delay (Set.singleton id) snd

index :: [a] -> [Int]
index = zipWith const [0..]

mkChannels :: [String] -> (InputFunc v a, [O v v])
mkChannels names = (input nameMapping, map mkChannelFromId $ index names)
    where nameMapping = constructNameMapping names
          
constructNameMapping :: [String] -> Map String Int
constructNameMapping [] = Map.empty
constructNameMapping (name : names) = fromList $ scanl (\(_, i) name -> (name, i+1)) (name, 0) names