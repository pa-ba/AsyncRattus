module Rattus.Channels (
    mkChannels,
    InputFunc,
    InputChannel,
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
type InputMaybeFunc v a = String -> v -> O v a -> Maybe a
type DependFunc v a = O v a -> [String]
type InputChannel v = O v v

input :: Map String Int -> String -> v -> O v a -> a
input nameToId name v later = 
    fromMaybe (error ("Later does not depend on input channel" ++ show name)) (inputMaybe nameToId name v later)

inputMaybe :: Map String Int -> String -> v -> O v a -> Maybe a
inputMaybe nameToId name v later =  
    if compatible then Just $ adv' later (id, v) 
    else Nothing
    where id = fromMaybe (error "No such input channel") (lookup name nameToId)
          compatible = Set.member id $ extractClock later

mkChannelFromId :: InputChannelIdentifier -> InputChannel v
mkChannelFromId id = Delay (Set.singleton id) snd

index :: [a] -> [Int]
index = zipWith const [0..]

mkChannels :: [String] -> (InputFunc v a, InputMaybeFunc v a, DependFunc v a, [InputChannel v])
mkChannels names = (input nameMapping, inputMaybe nameMapping, depend idMapping, map mkChannelFromId $ index names)
    where nameMapping = constructNameMapping names
          idMapping = constructIdToNameMapping names
          
constructNameMapping :: [String] -> Map String Int
constructNameMapping [] = Map.empty
constructNameMapping (name : names) = fromList $ scanl (\(_, i) name -> (name, i+1)) (name, 0) names

constructIdToNameMapping :: [String] -> Map Int String
constructIdToNameMapping [] = Map.empty
constructIdToNameMapping (name : names) = fromList $ scanl (\(i, _) name -> (i+1, name)) (0, name) names

-- Check which output channels depend
depend :: Map Int String -> O v a -> [String]
depend idToName later = foldl (\acc id -> fromMaybe (error "Internal error: id does not exist") (lookup id idToName) : acc) [] clock
    where clock = extractClock later