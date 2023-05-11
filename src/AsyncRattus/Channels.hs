{-# LANGUAGE TypeOperators #-}

module AsyncRattus.Channels (
    mkChannels,
    InputFunc,
    InputChannel,
) where
import Prelude hiding (Left, Right, lookup)
import AsyncRattus.InternalPrimitives
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import AsyncRattus.Strict 
import qualified AsyncRattus.Stream as Stream

-- Function for providing input to a later
--             Channel name | value | later
type InputFunc v a = String -> v -> O v a -> a
type InputMaybeFunc v a = String -> v -> O v a -> Maybe a
type DependFunc v a = O v a -> Set String
type InputChannel v = Box (O v v)

input :: Map String Int -> String -> v -> O v a -> a
input nameToId name v later = 
    fromMaybe (error ("Later does not depend on input channel" ++ show name)) (inputMaybe nameToId name v later)

inputMaybe :: Map String Int -> String -> v -> O v a -> Maybe a
inputMaybe nameToId name v later =  
    if compatible then Just $ adv' later (id, v) 
    else Nothing
    where id = fromMaybe (error "No such input channel") (lookup name nameToId)
          compatible = Set.member id $ extractClock later

mkChannelFromId :: InputChannelIdentifier -> O v v
mkChannelFromId id = Delay (Set.singleton id) snd

index :: List a -> List (Int :* a)
index Nil = Nil
index (name :! names) = reverse' $ foldl (\acc@((lastId :* _) :! _) name -> (lastId + 1 :* name) :! acc) ((0 :* name) :! Nil) names   

mkChannels :: (Stable v) => List String -> (InputFunc v a, InputMaybeFunc v a, DependFunc v a, List (InputChannel v))
mkChannels names = (input nameMapping, inputMaybe nameMapping, depend idMapping, map' (box . mkChannelFromId) . map' fst' $ indexed)
    where nameMapping = constructNameMapping indexed
          idMapping = constructIdToNameMapping indexed
          indexed = index names
          
constructNameMapping :: List (Int :* String) -> Map String Int
constructNameMapping = foldl (\acc (id :* name) -> Map.insert name id acc) Map.empty

constructIdToNameMapping :: List (Int :* String) -> Map Int String
constructIdToNameMapping = foldl (\acc (id :* name) -> Map.insert id name acc) Map.empty 


-- Check which output channels depend
depend :: Map Int String -> O v a -> Set String
depend idToName later = Set.map (\id -> fromMaybe (error "Internal error: id does not exist") (lookup id idToName)) clock -- foldl (\acc id -> fromMaybe (error "Internal error: id does not exist") (lookup id idToName) : acc) [] clock
    where clock = extractClock later