module Matrix where

-------------- Imports --------------

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Random.Array as ShuffleArray
import Array
import String

-------------- Model --------------

type alias Model = 
  { rowDimensions : List Dimension
  , columnDimensions : List Dimension
  , data : List (List Data)
  , seed : Random.Seed
  , factor : Int
  }

type alias Dimension =
  { id : String
  , label : String
  , members : List Member
  }

type alias Member =
  { id : String
  , label : String
  }

type alias Data = Int

initialModel = 
  { rowDimensions = []
  , columnDimensions = []
  , data = []
  , seed = Random.initialSeed 42
  , factor = 10
  }

partitionData data partitionSize output =
  let
    sublist = List.take partitionSize data
    restData = List.drop partitionSize data
  in
    if List.length data > 0
    then partitionData restData partitionSize ( sublist :: output )
    else output

getMemberCount : List Dimension -> Int
getMemberCount dimensions =
  List.foldr addLength 1 dimensions

addLength dimension sum =
  sum * ( List.length dimension.members )

model : Signal Model
model =
  Signal.foldp update ( createRandomModel initialModel ) actions.signal

createRandomModel : Model -> Model
createRandomModel model =
  let
    rowDimensions = model.rowDimensions
    columnDimensions = model.columnDimensions
    rowCount = getMemberCount rowDimensions
    columnCount = getMemberCount columnDimensions
    (shuffledArray, newSeed) = ShuffleArray.shuffle model.seed numbers
    neededData = List.take ( rowCount * columnCount ) ( Array.toList shuffledArray )
    dataList = partitionData neededData rowCount []
  in
    { rowDimensions = rowDimensions
    , columnDimensions = columnDimensions
    , factor = model.factor
    , data = dataList
    , seed = newSeed
    }

-------------- Update --------------

type Action = 
  NoOp
  | DistToRows
  | DistToColumns
  | RegionToRows
  | RegionToColumns
  | QuarterToRows
  | QuarterToColumns
  | CreateRandom

update : Action -> Model -> Model
update action model = 
  case action of
    CreateRandom -> createRandomModel model
    DistToRows -> distToRows model |> createRandomModel
    DistToColumns -> distToColumns model |> createRandomModel
    RegionToRows -> regionToRows model |> createRandomModel
    RegionToColumns -> regionToColumns model |> createRandomModel
    QuarterToRows -> quarterToRows model |> createRandomModel
    QuarterToColumns -> quarterToColumns model |> createRandomModel
    _ -> model

dropDimension list single =
  List.filter (\x -> x.id /= single.id) list

distToRows model = 
  { rowDimensions = distributionChannel :: model.rowDimensions
  , columnDimensions = dropDimension model.columnDimensions distributionChannel
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }

distToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions distributionChannel
  , columnDimensions =  distributionChannel :: model.columnDimensions
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }

regionToRows model = 
  { rowDimensions = dropDimension model.rowDimensions regionCode
  , columnDimensions = regionCode :: model.columnDimensions
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }

regionToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions regionCode
  , columnDimensions = regionCode :: model.columnDimensions
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }

quarterToRows model = 
  { rowDimensions = dropDimension model.rowDimensions quarter
  , columnDimensions = quarter :: model.columnDimensions
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }

quarterToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions quarter
  , columnDimensions = quarter :: model.columnDimensions
  , factor = model.factor
  , data = model.data
  , seed = model.seed
  }


main = Signal.map (view actions.address) model

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

-------------- View --------------

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    rowDimensionLabels = List.map (\x -> x.label) model.rowDimensions
    columnDimensionLabels = List.map (\x -> x.label) model.columnDimensions
  in
    Html.div [ generalStyle ]
      [ Html.button [ Events.onClick address CreateRandom ] [ Html.text "Create Random" ]
      , Html.button [ Events.onClick address DistToRows ] [ Html.text "Dist to rows" ]
      , Html.button [ Events.onClick address DistToColumns ] [ Html.text "Dist to columns" ]
      , Html.button [ Events.onClick address RegionToRows ] [ Html.text "Region to rows" ]
      , Html.button [ Events.onClick address RegionToColumns ] [ Html.text "Region to columns" ]
      , Html.button [ Events.onClick address QuarterToRows ] [ Html.text "Quarter to rows" ]
      , Html.button [ Events.onClick address QuarterToColumns ] [ Html.text "Quarter to columns" ]
      , drawTable model.data
      , Html.div [] [ String.join ", " rowDimensionLabels |> (++) "Row Dimensions: " |> Html.text ]
      , Html.div [] [ String.join ", " columnDimensionLabels |> (++) "Column Dimensions: " |> Html.text ]
      ]

drawTable : List (List Data) -> Html.Html
drawTable tableData =
  Html.table []
    ( List.map drawColumn tableData )

drawColumn : List Data -> Html.Html
drawColumn column =
  Html.tr []
    ( List.map drawCell column )

drawCell : Data -> Html.Html
drawCell data =
  Html.td [ cellStyle ]
    [ List.repeat data "+" |> String.join "" |> Html.text ]

-------------- Sample Data --------------

allDimensions = [ regionCode, quarter, distributionChannel ]

regionCode =
  { id = "regionCode"
  , label = "regionCode"
  , members = [ east, west, north, south ]
  }

east =
  { id = "east", label = "east" }
south =
  { id = "south", label = "south" }
north =
  { id = "north", label = "north" }
west =
  { id = "west", label = "west" }

quarter =
  { id = "quarter"
  , label = "quarter"
  , members = [ q1, q2, q3, q4 ]
  }

q1 =
  { id = "q1", label = "q1" }
q4 =
  { id = "q4", label = "q4" }
q3 =
  { id = "q3", label = "q3" }
q2 =
  { id = "q2", label = "q2" }

distributionChannel =
  { id = "distributionChannel"
  , label = "distributionChannel"
  , members = [ edi, internet, phone ]
  }

edi =
  { id = "edi", label = "edi" }
phone =
  { id = "phone", label = "phone" }
internet =
  { id = "internet", label = "internet" }

generalStyle =
  Attr.style
    [ ("font-size", "16px")
    , ("font-family", "monospace")
    ]

labelStyle =
  Attr.style
    [ 
    ]

cellStyle =
  Attr.style
    [ ("border", "1px solid black")
    , ("text-align", "left")
    , ("font-size", "8px")
    , ("height", "20px")
    , ("width", "100px")
    ]


numbers = Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
