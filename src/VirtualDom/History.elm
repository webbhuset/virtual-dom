module VirtualDom.History exposing
  ( History
  , Address
  , Msg(..)
  , empty
  , size
  , latestAddress
  , initialModel
  , add
  , get
  , visibleAddressBefore
  , visibleAddressAfter
  , openGroup
  , closeGroup
  , view
  , decoder
  , encode
  )


import Array exposing (Array)
import Char
import Time exposing (Time)
import Json.Decode as Decode
import Json.Encode as Encode
import Native.Debug
import VirtualDom.Helpers as VDom exposing (Node)
import VirtualDom.Metadata as Metadata



-- CONSTANTS


minSnapshotSize : Int
minSnapshotSize =
  64



-- HISTORY


type alias History model msg =
  { snapshots : Array (Snapshot model msg)
  , recent : Snapshot model msg
  , numMessages : Int
  , lastTimestamp : Maybe Time
  }


type alias Snapshot model msg =
  { model : model
  , groups : Array (Group msg)
  , numMessages : Int
  }


empty : model -> History model msg
empty model =
  History Array.empty (Snapshot model Array.empty 0) 0 Nothing


size : History model msg -> Int
size history =
  history.numMessages


initialModel : History model msg -> model
initialModel  { snapshots, recent } =
  case Array.get 0 snapshots of
    Just { model } ->
      model

    Nothing ->
      recent.model



-- GROUP


type alias Group msg =
  { name : String
  , collapsed : Bool
  , messages : List msg
  , numMessages : Int
  }


initGroup : String -> msg -> Group msg
initGroup groupName msg =
  { name = groupName
  , collapsed = False
  , messages = [ msg ]
  , numMessages = 1
  }


lastMessage : Group msg -> msg
lastMessage group =
  case group.messages of
    [] ->
      Debug.crash "Group must contain at least one message."

    last :: _ ->
      last



-- JSON


decoder : model -> (msg -> model -> model) -> Decode.Decoder (model, History model msg)
decoder initialModel update =
  let
    addMessage rawMsg (model, history) =
      let
        msg =
          jsToElm rawMsg
      in
        (update msg model, add Nothing msg model history)

    updateModel rawMsgs =
      List.foldl addMessage (initialModel, empty initialModel) rawMsgs
  in
    Decode.map updateModel (Decode.list Decode.value)


jsToElm : Encode.Value -> a
jsToElm =
  Native.Debug.unsafeCoerce


encode : History model msg -> Encode.Value
encode { snapshots, recent } =
  let
    recentJson =
      consEncodedGroups recent.groups []
  in
    Encode.list <| Array.foldr consEncodedSnapshot recentJson snapshots


consEncodedSnapshot : Snapshot model msg -> List Encode.Value -> List Encode.Value
consEncodedSnapshot snapshot allMessages =
  consEncodedGroups snapshot.groups allMessages


consEncodedGroups : Array (Group msg) -> List Encode.Value -> List Encode.Value
consEncodedGroups groups allMessages =
  Array.foldl consEncodedGroup allMessages groups


consEncodedGroup : Group msg -> List Encode.Value -> List Encode.Value
consEncodedGroup group allMessages =
  List.foldl (\elm msgs -> elmToJs elm :: msgs) allMessages group.messages


elmToJs : a -> Encode.Value
elmToJs =
  Native.Debug.unsafeCoerce



-- ADD MESSAGES


add : Maybe Time -> msg -> model -> History model msg -> History model msg
add timestamp msg model { snapshots, recent, numMessages, lastTimestamp } =
  case addRecent lastTimestamp timestamp msg model recent of
    (Just snapshot, newRecent) ->
      History (Array.push snapshot snapshots) newRecent (numMessages + 1) timestamp

    (Nothing, newRecent) ->
      History snapshots newRecent (numMessages + 1) timestamp


addRecent : Maybe Time -> Maybe Time -> msg -> model -> Snapshot model msg -> (Maybe (Snapshot model msg), Snapshot model msg)
addRecent lastTimestamp timestamp msg model recent =
  case addMessageToGroups lastTimestamp timestamp msg recent.numMessages recent.groups of
    (groups, Just newGroup) ->
      ( Just recent
      , Snapshot model (Array.fromList [ newGroup ]) 1
      )

    (groups, Nothing) ->
      ( Nothing
      , Snapshot recent.model groups (recent.numMessages + 1)
      )


addMessageToGroups : Maybe Time -> Maybe Time -> msg -> Int -> Array (Group msg) -> (Array (Group msg), Maybe (Group msg))
addMessageToGroups lastTimestamp timestamp msg numMessages groups =
  let
    groupName =
      Native.Debug.messageToGroupName msg
  in
    case Array.get (Array.length groups - 1) groups of
      Nothing ->
        (Array.fromList [ initGroup groupName msg ], Nothing)

      Just headGroup ->
        if isSameGroup groupName headGroup.name then
          ( Array.set
              (Array.length groups - 1)
              (updateLatestGroup lastTimestamp timestamp msg headGroup)
              groups
          , Nothing
          )
        else if numMessages < minSnapshotSize then
          (Array.push (initGroup groupName msg) groups, Nothing)
        else
          (groups, Just (initGroup groupName msg))


updateLatestGroup : Maybe Time -> Maybe Time -> msg -> Group msg -> Group msg
updateLatestGroup lastTimestamp timestamp msg group =
  let
    collapsed =
      if group.numMessages == 1 && isImmediateAfter lastTimestamp timestamp then
        True
      else if group.numMessages == 5 && timestamp == Nothing then
        True
      else
        group.collapsed
  in
    { group
        | collapsed = collapsed
        , messages = msg :: group.messages
        , numMessages = group.numMessages + 1
    }


isSameGroup : String -> String -> Bool
isSameGroup groupName1 groupName2 =
  groupName1 /= "" && groupName1 == groupName2


isImmediateAfter : Maybe Time -> Maybe Time -> Bool
isImmediateAfter timestamp1 timestamp2 =
  timestamp1
    |> Maybe.andThen (\t1 -> timestamp2
    |> Maybe.map (\t2 ->
      t2 - t1 < 120
    ))
    |> Maybe.withDefault False



-- GET SUMMARY


get : (msg -> model -> (model, a)) -> Address -> History model msg -> ( model, msg )
get update address history =
  case findSnapshot address history of
    Just snapshot ->
      let
        updateHelp msg model =
          Tuple.first <| update msg model
      in
        undone <|
          Array.foldl (getHelp updateHelp address.message) (Stepping address.group snapshot.model) snapshot.groups

    Nothing ->
      Debug.crash ("UI should only let you ask for real indexes! " ++ toString address)


type GetResult model msg
  = Stepping Int model
  | Done msg model


getHelp : (msg -> model -> model) -> Int -> Group msg -> GetResult model msg -> GetResult model msg
getHelp update messageIndex group getResult =
  case getResult of
    Done _ _ ->
      getResult

    Stepping n model ->
      if n == 0 then
        List.foldr (getHelpHelp update) (Stepping messageIndex model) group.messages
      else
        Stepping (n - 1) (List.foldr update model group.messages)


getHelpHelp : (msg -> model -> model) -> msg -> GetResult model msg -> GetResult model msg
getHelpHelp update msg getResult =
  case getResult of
    Done _ _ ->
      getResult

    Stepping n model ->
      if n == 0 then
        Done msg (update msg model)

      else
        Stepping (n - 1) (update msg model)


undone : GetResult model msg -> ( model, msg )
undone getResult =
  case getResult of
    Done msg model ->
      ( model, msg )

    Stepping _ _ ->
      Debug.crash "Bug in History.get"



-- ADDRESS


type alias Address =
  { snapshot : Int
  , group : Int
  , message : Int
  }


lastIndex : Int
lastIndex = -2


increment : Address -> Address
increment address =
  { address | message = address.message + 1 }


decrement : Address -> Address
decrement address =
  { address | message = address.message - 1 }


incrementGroup : Address -> Address
incrementGroup address =
  { address | group = address.group + 1, message = 0 }


decrementGroup : Address -> Address
decrementGroup address =
  { address | group = address.group - 1, message = lastIndex }


incrementSnapshot : Address -> Address
incrementSnapshot address =
  { address
      | snapshot = address.snapshot + 1
      , group = 0
      , message = 0
  }


decrementSnapshot : Address -> Address
decrementSnapshot address =
  { address
      | snapshot = address.snapshot - 1
      , group = lastIndex
      , message = lastIndex
  }



-- UP AND DOWN


latestAddress : History model msg -> Maybe Address
latestAddress history =
  validateAddress
    history
    ( Address (Array.length history.snapshots) lastIndex lastIndex )


visibleAddressBefore : Address -> History model msg -> Maybe Address
visibleAddressBefore =
  nextVisibleAddress False


visibleAddressAfter : Address -> History model msg -> Maybe Address
visibleAddressAfter =
  nextVisibleAddress True


nextVisibleAddress : Bool -> Address -> History model msg -> Maybe Address
nextVisibleAddress forward fromAddress history =
  case findGroup fromAddress history of
    Just (group, address) ->
      validateAddress history (nextAddress forward group.collapsed address)

    Nothing ->
      Debug.crash ("UI should only let you ask for real indexes! " ++ toString fromAddress)


nextAddress : Bool -> Bool -> Address -> Address
nextAddress forward groupCollapsed =
  if groupCollapsed then
    if forward then incrementGroup else decrementGroup
  else
    if forward then increment else decrement



-- OPEN AND CLOSE


openGroup : Address -> History model msg -> (History model msg, Address)
openGroup = updateGroup False


closeGroup : Address -> History model msg -> (History model msg, Address)
closeGroup = updateGroup True


updateGroup : Bool -> Address -> History model msg -> (History model msg, Address)
updateGroup collapsed address history =
  let
    updateGroup maybeGroup =
      case maybeGroup of
        Just group ->
          if List.length group.messages >= 2 then
            Just { group | collapsed = collapsed }
          else
            Nothing

        Nothing ->
          Debug.crash "UI should only let you ask for real indexes!"

    newHistory =
      updateSnapshot
        (arrayUpdate updateGroup address.group)
        address.snapshot
        history
  in
    (newHistory, { address | message = 0 })


updateSnapshot : (Array (Group msg) -> Array (Group msg)) -> Int -> History model msg -> History model msg
updateSnapshot updateGroups snapshotIndex history =
  if snapshotIndex == Array.length history.snapshots then
    { history
        | recent =
            Snapshot
              history.recent.model
              (updateGroups history.recent.groups)
              history.recent.numMessages
    }
  else
    { history
        | snapshots =
            history.snapshots
              |> arrayUpdate (\snapshot ->
                case snapshot of
                  Just snapshot ->
                    Just <| Snapshot snapshot.model (updateGroups snapshot.groups) snapshot.numMessages

                  Nothing ->
                    Debug.crash "UI should only let you ask for real indexes!"
                ) snapshotIndex
    }


arrayUpdate : (Maybe a -> Maybe a) -> Int -> Array a -> Array a
arrayUpdate f index array =
  case f (Array.get index array) of
    Just a ->
      Array.set index a array

    Nothing ->
      array



-- ACCESS BY ADDRESS


validateAddress : History model msg -> Address -> Maybe Address
validateAddress history address =
  if address.message == -1 then
    validateAddress history (decrementGroup address)
  else
    findGroup address history
      |> Maybe.andThen (\(group, address) ->
          if address.message >= group.numMessages then
            validateAddress history (incrementGroup address)
          else if group.collapsed || address.message == lastIndex then
            Just { address | message = group.numMessages - 1 }
          else
            Just address
        )


findGroup : Address -> History model msg -> Maybe (Group msg, Address)
findGroup address history =
  if address.group == -1 then
    findGroup (decrementSnapshot address) history
  else
    findSnapshot address history
      |> Maybe.andThen (\snapshot ->
        if address.group >= Array.length snapshot.groups then
          findGroup (incrementSnapshot address) history
        else
          let
            newAddress =
              if address.group == lastIndex then
                { address | group = Array.length snapshot.groups - 1 }
              else
                address
          in
            Array.get newAddress.group snapshot.groups
              |> Maybe.map (\group -> (group, newAddress))
        )


findSnapshot : Address -> History model msg -> Maybe (Snapshot model msg)
findSnapshot address history =
  if address.snapshot < 0 then
    Nothing
  else if address.snapshot == Array.length history.snapshots then
    Just history.recent
  else
    Array.get address.snapshot history.snapshots



-- VIEW


type Msg
  = OpenGroup Address
  | CloseGroup Address
  | Select Address


view : Maybe Address -> History model msg -> Node Msg
view currentAddress { snapshots, recent, numMessages } =
  let
    className =
      if currentAddress == Nothing then
        "debugger-sidebar-messages"
      else
        "debugger-sidebar-messages-paused"

    oldStuff =
      VDom.lazy3 viewSnapshots currentAddress (numMessages - recent.numMessages) snapshots

    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterBySnapshot (Array.length snapshots))

    newStuff =
      Array.foldr (consGroup currentAddressHelp) (numMessages - 1, (Array.length recent.groups - 1), []) recent.groups
        |> (\(_, _, nodes) -> nodes)
        |> VDom.div []
        |> VDom.map ((|>) (Array.length snapshots))
  in
    VDom.div [ VDom.class className ] (oldStuff :: newStuff :: [])



-- VIEW SNAPSHOTS


viewSnapshots : Maybe Address -> Int -> Array (Snapshot model msg) -> Node Msg
viewSnapshots currentAddress highIndex snapshots =
  VDom.div [] <| (\(index, _, nodes) -> nodes) <|
    Array.foldr (consSnapshot currentAddress) (highIndex, (Array.length snapshots - 1), []) snapshots


consSnapshot : Maybe Address -> Snapshot model msg -> ( Int, Int, List (Node Msg) ) -> ( Int, Int, List (Node Msg) )
consSnapshot currentAddress snapshot (index, snapshotIndex, rest) =
  let
    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterBySnapshot snapshotIndex)
  in
    ( index - snapshot.numMessages
    , snapshotIndex - 1
    , VDom.map
        ((|>) snapshotIndex)
        (VDom.lazy3 viewSnapshot currentAddressHelp index snapshot)
      :: rest
    )


viewSnapshot : Maybe Address -> Int -> Snapshot model msg -> Node (Int -> Msg)
viewSnapshot currentAddress index { groups } =
  groups
    |> Array.foldr (consGroup currentAddress) (index - 1, (Array.length groups - 1), [])
    |> (\(_, _, nodes) -> nodes)
    |> VDom.div []



-- VIEW GROUP


consGroup : Maybe Address -> Group msg -> ( Int, Int, List (Node (Int -> Msg)) ) -> ( Int, Int, List (Node (Int -> Msg)) )
consGroup currentAddress group (index, groupIndex, rest) =
  let
    currentAddressHelp =
      currentAddress
        |> Maybe.andThen (filterByGroup groupIndex)
  in
    ( index - group.numMessages
    , groupIndex - 1
    , viewGroup currentAddressHelp index groupIndex group :: rest
    )


viewGroup : Maybe Address -> Int -> Int -> Group msg -> Node (Int -> Msg)
viewGroup currentAddress index groupIndex group =
  let
    children =
      if group.collapsed then
        viewCollapsedMessage
          currentAddress
          groupIndex
          (index - group.numMessages + 1)
          index
          (lastMessage group)
          :: []
      else
        group.messages
          |> List.foldl (consMsg currentAddress) (index, (group.numMessages - 1), [])
          |> (\(_, _, nodes) -> nodes)

    folder =
      VDom.lazy groupFolder group
  in
    VDom.div [ VDom.class "messages-group" ] (folder :: children)
      |> VDom.map ((|>) groupIndex)


groupFolder : Group msg -> Node (Int -> Int -> Msg)
groupFolder group =
  let
    msg = \groupIndex snapshotIndex ->
      (if group.collapsed then OpenGroup else CloseGroup)
        (Address snapshotIndex groupIndex 0)
  in
    VDom.div
      [ VDom.onClick msg
      , VDom.class "message-group-folder"
      ]
      (groupFolderHelp group)


groupFolderHelp : Group msg -> List (Node a)
groupFolderHelp group =
  if List.length group.messages <= 1 then
    []
  else if group.collapsed then
    [ groupToggleButton "+" ]
  else
    let
      height =
        (toFloat (List.length group.messages) - 1.5) * 26

      groupGuide =
        VDom.div
          [ VDom.class "messages-group-guide"
          , VDom.style [("height", toString height ++ "px")]
          ]
          []
    in
      [ groupToggleButton "-", groupGuide ]


groupToggleButton : String -> Node a
groupToggleButton s =
  VDom.div
    [ VDom.class "messages-group-button" ]
    [ VDom.div [ VDom.class "messages-group-button-text"] [ VDom.text s ] ]


viewCollapsedMessage : Maybe Address -> Int -> Int -> Int -> msg -> Node (Int -> Int -> Msg)
viewCollapsedMessage currentAddress groupIndex firstIndex lastIndex msg =
  let
    selected =
      case currentAddress of
        Just address ->
          address.group == groupIndex

        Nothing ->
          False

    indexStr =
      ".." ++ toString lastIndex
  in
    VDom.lazy3 viewRow selected indexStr (Native.Debug.messageToString msg)
      |> VDom.map ((|>) (lastIndex - firstIndex - 1))



-- VIEW MESSAGE


consMsg : Maybe Address -> msg -> ( Int, Int, List (Node (Int -> Int -> Msg)) ) -> ( Int, Int, List (Node (Int -> Int -> Msg)) )
consMsg currentAddress msg (index, msgIndex, rest) =
  ( index - 1
  , msgIndex - 1
  , viewMessage currentAddress index msgIndex msg :: rest
  )


viewMessage : Maybe Address -> Int -> Int -> msg -> Node (Int -> Int -> Msg)
viewMessage currentAddress index msgIndex msg =
  let
    selected =
      case currentAddress of
        Just address ->
          address.message == msgIndex

        Nothing ->
          False
  in
    VDom.lazy3 viewRow selected (toString index) (Native.Debug.messageToString msg)
      |> VDom.map ((|>) msgIndex)


viewRow : Bool -> String -> String -> Node (Int -> Int -> Int -> Msg)
viewRow selected index content =
  let
    className =
      if selected then
        "messages-entry messages-entry-selected"

      else
        "messages-entry"

    msg = \msgIndex groupIndex snapshotIndex ->
      Select (Address snapshotIndex groupIndex msgIndex)
  in
    VDom.div
      [ VDom.class className
      , VDom.on "click" (Decode.succeed msg)
      ]
      [ VDom.span [VDom.class "messages-entry-content", VDom.attribute "title" content ] [ VDom.text content ]
      , VDom.span [VDom.class "messages-entry-index"] [ VDom.text index ]
      ]



-- ADDRESS HELPER


emptyAddress : Address
emptyAddress = Address 0 0 0


filterBySnapshot : Int -> Address -> Maybe Address
filterBySnapshot = filterBy .snapshot


filterByGroup : Int -> Address -> Maybe Address
filterByGroup = filterBy .group


filterBy : (Address -> Int) -> Int -> Address -> Maybe Address
filterBy f index address =
  if f address == index then
    Just address
  else
    Nothing
