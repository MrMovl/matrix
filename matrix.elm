module Matrix where

-------------- Imports --------------

import Html
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Random.Array as ShuffleArray
import Array
import String
import Mouse

-------------- Model --------------

type alias Model = 
  { rowDimensions : List Dimension
  , columnDimensions : List Dimension
  , data : List (List Data)
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

type Action = 
  NoOp
  | Dimension2ToRows
  | Dimension2ToColumns
  | Dimension1ToRows
  | Dimension1ToColumns
  | Dimension3ToRows
  | Dimension3ToColumns
  | CreateRandom

main = Signal.map (view actions.address) model

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

model : Signal Model
model =
  Signal.foldp update ( createModel initialModel ) actions.signal

---------------------------------------------------------------------------------

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    rowDimensionLabels = List.map (\x -> x.label) model.rowDimensions
    columnDimensionLabels = List.map (\x -> x.label) model.columnDimensions
  in
    Html.div [ generalStyle ]
      [ Html.button [ Events.onClick address CreateRandom ] [ Html.text "Create Random" ]
      , Html.button [ Events.onClick address Dimension1ToRows ] [ Html.text "dimension1 to rows" ]
      , Html.button [ Events.onClick address Dimension1ToColumns ] [ Html.text "dimension1 to columns" ]
      , Html.button [ Events.onClick address Dimension2ToRows ] [ Html.text "dimension2 to rows" ]
      , Html.button [ Events.onClick address Dimension2ToColumns ] [ Html.text "dimension2 to columns" ]
      , Html.button [ Events.onClick address Dimension3ToRows ] [ Html.text "dimension3 to rows" ]
      , Html.button [ Events.onClick address Dimension3ToColumns ] [ Html.text "dimension3 to columns" ]
      , Html.div [] [ String.join ", " rowDimensionLabels |> (++) "Row Dimensions: " |> Html.text ]
      , Html.div [] [ String.join ", " columnDimensionLabels |> (++) "Column Dimensions: " |> Html.text ]
      , drawTable model.data
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

---------------------------------------------------------------------------------

update : Action -> Model -> Model
update action model = 
  case action of
    CreateRandom -> createModel model
    Dimension1ToRows -> dimension1ToRows model |> createModel
    Dimension1ToColumns -> dimension1ToColumns model |> createModel
    Dimension2ToRows -> dimension2ToRows model |> createModel
    Dimension2ToColumns -> dimension2ToColumns model |> createModel
    Dimension3ToRows -> dimension3ToRows model |> createModel
    Dimension3ToColumns -> dimension3ToColumns model |> createModel
    _ -> model

initialModel = 
  { rowDimensions = []
  , columnDimensions = []
  , data = []
  }

createModel : Model -> Model
createModel model =
  let
    rowDimensions = model.rowDimensions
    columnDimensions = model.columnDimensions
    rowCount = getMemberCount rowDimensions
    columnCount = getMemberCount columnDimensions
    neededData = List.take ( rowCount * columnCount ) ( Array.toList numbers )
    dataList = partitionData rowCount [] neededData
  in
    { rowDimensions = rowDimensions
    , columnDimensions = columnDimensions
    , data = dataList
    }

getMemberCount : List Dimension -> Int
getMemberCount dimensions =
  List.foldr addLength 1 dimensions

addLength dimension sum =
  sum * ( List.length dimension.members )

partitionData partitionSize output data =
  let
    sublist = List.take partitionSize data
    restData = List.drop partitionSize data
  in
    if List.length data > 0
    then partitionData partitionSize ( sublist :: output ) restData
    else output

---------------------------------------------------------------------------------

dimension1ToRows model = 
  { rowDimensions = addUnique model.rowDimensions dimension1
  , columnDimensions = dropDimension model.columnDimensions dimension1
  , data = model.data
  }

dimension1ToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions dimension1
  , columnDimensions = addUnique model.columnDimensions dimension1
  , data = model.data
  }

dimension2ToRows model = 
  { rowDimensions = addUnique model.rowDimensions dimension2
  , columnDimensions = dropDimension model.columnDimensions dimension2
  , data = model.data
  }

dimension2ToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions dimension2
  , columnDimensions = addUnique model.columnDimensions dimension2
  , data = model.data
  }

dimension3ToRows model = 
  { rowDimensions = addUnique model.rowDimensions dimension3
  , columnDimensions = dropDimension model.columnDimensions dimension3
  , data = model.data
  }

dimension3ToColumns model = 
  { rowDimensions = dropDimension model.rowDimensions dimension3
  , columnDimensions = addUnique model.columnDimensions dimension3
  , data = model.data
  }

dropDimension list single =
  List.filter (\x -> x.id /= single.id) list

addUnique : List Dimension -> Dimension -> List Dimension
addUnique list dimension =
  dimension :: ( dropDimension list dimension)

-------------- Sample Data --------------

allDimensions = [ dimension1, dimension3, dimension2 ]

dimension1 =
  { id = "dimension1"
  , label = "dimension1"
  , members = [ dim1meber1, dim1member2, dim1member3, dim1member4 ]
  }

dim1meber1 =
  { id = "dim1meber1", label = "dim1meber1" }
dim1member2 =
  { id = "dim1member2", label = "dim1member2" }
dim1member3 =
  { id = "dim1member3", label = "dim1member3" }
dim1member4 =
  { id = "dim1member4", label = "dim1member4" }

dimension3 =
  { id = "dimension3"
  , label = "dimension3"
  , members = [ dim3member1, dim3member2, dim3member3, dim3member4 ]
  }

dim3member1 =
  { id = "dim3member1", label = "dim3member1" }
dim3member2 =
  { id = "dim3member2", label = "dim3member2" }
dim3member3 =
  { id = "dim3member3", label = "dim3member3" }
dim3member4 =
  { id = "dim3member4", label = "dim3member4" }

dimension2 =
  { id = "dimension2"
  , label = "dimension2"
  , members = [ dim2member1, dim2member2, dim2member3 ]
  }

dim2member1 =
  { id = "dim2member1", label = "dim2member1" }
dim2member2 =
  { id = "dim2member2", label = "dim2member2" }
dim2member3 =
  { id = "dim2member3", label = "dim2member3" }

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

numbers = Array.fromList [ 31, 52, 62, 53, 85, 74, 51, 77, 24, 64, 99, 82, 6, 59, 58, 15, 49, 47, 66, 72, 34, 96, 64, 72, 56, 29, 10, 63, 11, 57, 7, 17, 83, 47, 15, 76, 43, 38, 45, 69, 68, 31, 16, 40, 42, 98, 21, 62, 88, 79, 63, 82, 34, 24, 96, 71, 62, 85, 81, 28, 71, 8, 21, 93, 15, 17, 92, 11, 82, 69, 66, 46, 60, 51, 32, 44, 54, 80, 22, 46, 44, 61, 99, 23, 71, 9, 48, 62, 9, 7, 73, 91, 16, 50, 69, 89, 34, 38, 28, 51, 81, 62, 72, 100, 92, 53, 44, 73, 17, 67, 41, 49, 30, 13, 16, 21, 20, 58, 94, 74, 32, 70, 33, 53, 59, 93, 40, 10, 51, 86, 88, 75, 3, 14, 8, 48, 84, 19, 86, 40, 67, 42, 14, 74, 52, 15, 68, 35, 32, 79, 1, 30, 61, 64, 72, 39, 96, 97, 71, 16, 36, 81, 65, 49, 12, 65, 72, 34, 69, 75, 33, 25, 19, 97, 41, 2, 63, 47, 74, 38, 22, 35, 17, 68, 22, 89, 94, 48, 35, 54, 57, 59, 86, 88, 97, 65, 35, 5, 92, 92, 99, 62, 88, 25, 52, 45, 98, 11, 96, 12, 11, 96, 89, 1, 85, 88, 75, 28, 25, 93, 76, 64, 64, 20, 27, 83, 40, 22, 58, 24, 43, 6, 31, 5, 28, 45, 1, 50, 30, 58, 85, 47, 20, 13, 89, 91, 61, 64, 35, 53, 13, 49, 58, 53, 62, 37, 40, 12, 46, 69, 70, 17, 29, 69, 94, 73, 1, 99, 40, 75, 28, 42, 33, 88, 69, 56, 92, 29, 37, 10, 71, 9, 95, 24, 8, 27, 89, 9, 66, 22, 38, 36, 93, 43, 59, 92, 10, 25, 73, 51, 62, 15, 31, 46, 91, 52, 99, 81, 2, 37, 2, 89, 30, 2, 37, 83, 82, 79, 49, 22, 64, 62, 46, 19, 77, 56, 47, 34, 40, 92, 91, 84, 20, 84, 82, 68, 68, 74, 94, 82, 29, 67, 33, 91, 98, 24, 50, 37, 52, 8, 77, 44, 72, 38, 71, 78, 11, 75, 35, 48, 26, 42, 78, 41, 85, 70, 43, 75, 38, 88, 30, 90, 75, 35, 72, 91, 29, 46, 96, 27, 1, 44, 32, 81, 30, 45, 23, 57, 75, 59, 4, 41, 20, 51, 92, 54, 34, 84, 81, 46, 76, 85, 4, 62, 15, 26, 21, 69, 8, 37, 24, 93, 22, 75, 36, 50, 81, 69, 40, 29, 31, 49, 44, 21, 83, 19, 93, 53, 91, 78, 95, 95, 62, 27, 73, 92, 40, 62, 33, 85, 17, 15, 24, 34, 90, 60, 69, 4, 7, 22, 17, 46, 35, 79, 25, 24, 75, 77, 46, 35, 10, 98, 40, 45, 70, 81, 15, 31, 18, 93, 39, 28, 53, 13, 37, 33, 32, 68, 4, 27, 37, 87, 18, 91, 89, 55, 35, 78, 60, 14, 86, 71, 100, 35, 2, 84, 1, 84, 66, 65, 9, 9, 48, 55, 64, 34, 52, 99, 53, 38, 46, 75, 68, 60, 90, 85, 9, 91, 56, 4, 85, 85, 11, 71, 88, 85, 57, 18, 14, 93, 19, 41, 64, 36, 85, 98, 8, 42, 54, 57, 21, 18, 41, 15, 55, 36, 6, 71, 51, 27, 69, 14, 85, 55, 11, 82, 14, 14, 33, 6, 47, 100, 22, 20, 41, 47, 68, 64, 18, 95, 60, 50, 15, 1, 39, 98, 70, 85, 59, 2, 10, 1, 46, 17, 92, 36, 10, 54, 89, 65, 43, 23, 18, 35, 74, 82, 86, 84, 61, 61, 25, 58, 97, 34, 91, 25, 71, 14, 13, 86, 5, 91, 49, 24, 11, 72, 3, 27, 39, 66, 81, 17, 74, 51, 62, 44, 3, 7, 85, 79, 40, 76, 77, 59, 23, 73, 67, 43, 93, 64, 51, 68, 19, 1, 49, 60, 93, 15, 95, 17, 55, 69, 74, 85, 60, 65, 59, 59, 89, 17, 55, 42, 52, 18, 52, 29, 6, 15, 31, 47, 6, 95, 41, 99, 38, 36, 22, 77, 90, 96, 67, 35, 72, 1, 79, 91, 38, 81, 16, 40, 56, 70, 31, 74, 61, 88, 67, 2, 98, 4, 43, 92, 85, 25, 27, 36, 95, 24, 8, 22, 36, 34, 50, 50, 8, 92, 94, 33, 26, 89, 79, 18, 10, 9, 17, 8, 29, 66, 91, 90, 73, 47, 70, 51, 65, 56, 56, 1, 70, 61, 59, 17, 85, 53, 19, 19, 13, 85, 85, 48, 26, 10, 35, 89, 63, 87, 20, 30, 62, 93, 26, 65, 10, 85, 83, 57, 4, 79, 65, 42, 96, 6, 90, 65, 88, 61, 53, 76, 31, 4, 88, 97, 50, 48, 19, 59, 88, 29, 45, 91, 8, 2, 12, 48, 5, 45, 30, 38, 46, 41, 57, 8, 46, 87, 42, 31, 70, 51, 23, 47, 45, 48, 16, 42, 86, 61, 63, 63, 81, 26, 77, 55, 64, 65, 53, 39, 94, 78, 65, 42, 57, 51, 80, 7, 19, 22, 6, 95, 71, 34, 25, 35, 98, 82, 94, 44, 42, 47, 37, 94, 5, 19, 54, 29, 2, 84, 14, 84, 71, 74, 14, 1, 20, 33, 54, 64, 54, 58, 59, 67, 87, 19, 91, 50, 37, 76, 67, 10, 33, 87, 92, 34, 39, 14, 76, 37, 39, 74, 83, 90, 44, 40, 24, 94, 20, 70, 32, 90, 53, 78, 15, 2, 96, 39, 39, 91, 92, 32, 66, 100, 89 ] 
