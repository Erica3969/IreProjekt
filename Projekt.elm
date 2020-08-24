module Main exposing (main)

-- in Ellie
-- module Main exposing (main)

import Axis
import Browser
import Color exposing (Color)
import Html exposing (Html, a, b, br, button, div, h2, li, text, ul)
import Html.Attributes
import Html.Events exposing (onClick)
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, polyline, rect, style, svg, text_, radialGradient, defs, stop)
import TypedSvg.Attributes exposing (id, class, d, fill, fontFamily, fontSize, points, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))

import GeoJson exposing (GeoJson, Geometry(..))
import Html.Styled exposing (toUnstyled)
import Html.Styled.Attributes
import Html.Styled.Events
import Http
import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Marker as Marker exposing (Marker)

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )
middle =
    Maps.Geo.latLng 30 10

italy= 
    Maps.Geo.latLng 42 13
type PlotType
    = Sweetness
    | Alcohol
    | Body
    | Tannins
    | Acidity
    
type alias Model =
    {  selectedWineName : String, selectedPlot : PlotType, map : Maps.Model Msg }

type Msg
    = ChangeSelectedWine String PlotType 
    | MapsMsg (Maps.Msg Msg)
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MapsMsg msg_ -> model.map
                        |> Maps.update msg_
                |> Tuple.mapFirst (\m -> { model | map = m })
                |> Tuple.mapSecond (Cmd.map MapsMsg)
        ChangeSelectedWine newWine newPlot ->
            ({model| selectedWineName= newWine, selectedPlot = newPlot}  ,Cmd.none)        
         
              
      

view : String -> Html Msg
view selectedWineName =
    let        
        pointList : List Wine
        pointList =
            List.filter (\w_ -> selectedWineName==w_.wineName) wines
        in
        Html.p[][
            Html.div[][Html.text selectedWineName]
          , Html.div [] ((List.map drawPlot pointList))       
       ]



init : () -> ( Model, Cmd Msg )
init () =
    let 
        view_ =
            Html.Styled.span
                []
                [Html.Styled.text <| "📍"]
        datas_ = 
            [(Marker.createCustom view_ (italy))]
    in
    ( { selectedWineName = "riesling", selectedPlot = Tannins
    , map = Maps.defaultModel
                |> Maps.updateMap (Map.setZoom 4 >> Map.moveTo middle)
                |> Maps.updateMap (Map.setWidth 500)
                |> Maps.updateMap (Map.setHeight 300)
                |> Maps.updateMarkers  (\markers -> datas_)}
    , Cmd.none
    )



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = viewGlobal --view2 >>toUnStyled
        , update = update
        , subscriptions = \m -> Sub.none
        }


viewGlobal : Model -> Html Msg
viewGlobal model =
    Html.div []
        [ Html.h3 []
            [ Html.text "Scatterplot" ]
        , Html.p []
            [  Html.div [] 
                [ button [ Html.Events.onClick (ChangeSelectedWine model.selectedWineName Sweetness) ] [ Html.text "Sweetness " ]
                 , button [ Html.Events.onClick (ChangeSelectedWine model.selectedWineName Alcohol) ] [ Html.text "Alcohol " ]
                 , button [ Html.Events.onClick (ChangeSelectedWine model.selectedWineName Body) ] [ Html.text "Body " ]
                 , button [ Html.Events.onClick (ChangeSelectedWine model.selectedWineName Tannins) ] [ Html.text "Tannins " ]
                 , button [ Html.Events.onClick (ChangeSelectedWine model.selectedWineName Acidity) ] [ Html.text "Acidity " ]]
                ]            
        , Html.p []
            [ Html.div []
                [ scatterplot wines model.selectedPlot]
            ]
        , Html.h3 []
            [ Html.text "Star-like Coordinates" ]
        , Html.p []
            [ Html.div []
                [ view model.selectedWineName ]
            ] 
        , Html.h3 []
            [ Html.text "Wine Map of The World" ]
        , Html.p []
            [ Html.div []
                [toUnstyled (view2) ]
            ] 
        ]
view2 : Html.Styled.Html Msg
view2 =
    Html.Styled.div
    [] [ map ]
  
map : Html.Styled.Html Msg
map =
    let 
        view_ : Int -> Html.Styled.Html Msg
        view_ col =             
            Html.Styled.span
                []
                [Html.Styled.text <| (sym col)] --"📍"
        datas_ = 
            List.map (\wi -> (Marker.createCustom (view_ (round wi.body)) (wi.coord)) ) wines
    in
            Maps.defaultModel
                |> Maps.updateMap (Map.setZoom 1 >> Map.moveTo middle)
                |> Maps.updateMap (Map.setWidth 700)
                |> Maps.updateMap (Map.setHeight 400)
                |> Maps.updateMarkers  (\markers -> datas_)
                |> Maps.view 
                |> Maps.mapView MapsMsg
                
sym : Int ->  String
sym col = 
    case col of 
        1 -> "⚪" -- "🏻"
        2 -> "🟡"
        3 -> "🟠"
        4 -> "🔴"
        5 -> "🟤"        
        _ -> "🟣"
            
drawPlot : Wine -> Svg Msg
drawPlot point =
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 30
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 30
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        [ TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill TypedSvg.Types.PaintNone
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , strokeWidth <| Px 0.5

            --, fill <| Paint <| Color.rgba 0 0 0 1
            ]
            []
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ circle [ cx (w / 2), cy (h / 2), r 5 ] []
            , text_
                [ x (w / 2 - 20), y -10 ]
                [ TypedSvg.Core.text "Sweetness" ]
            , text_
                [ transform [ Rotate 72 (w / 2) (h / 2) ], x (w / 2 - 20), y -10 ]
                [ TypedSvg.Core.text "Alcohol content" ]
            , text_
                [ transform [ Rotate 144 (w / 2) (h / 2) ], x (w / 2 - 20), y -10 ]
                [ TypedSvg.Core.text "Body" ]
            , text_
                [ transform [ Rotate 216 (w / 2) (h / 2) ], x (w / 2 - 20), y -10 ]
                [ TypedSvg.Core.text "Tannins" ]
            , text_
                [ transform [ Rotate 288 (w / 2) (h / 2) ], x (w / 2 - 20), y -10 ]
                [ TypedSvg.Core.text "Acidity" ]
            , line [ transform [ Rotate (cos 90) (w / 2) (h / 2) ], x1 (w / 2), y1 (h / 2), x2 (w / 2), y2 0, TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black) ] []
            , line [ transform [ Rotate 72 (w / 2) (h / 2) ], x1 (w / 2), y1 (h / 2), x2 (w / 2), y2 0, TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black) ] []
            , line [ transform [ Rotate 144 (w / 2) (h / 2) ], x1 (w / 2), y1 (h / 2), x2 (w / 2), y2 0, TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black) ] []
            , line [ transform [ Rotate 216 (w / 2) (h / 2) ], x1 (w / 2), y1 (h / 2), x2 (w / 2), y2 0, TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black) ] []
            , line [ transform [ Rotate 288 (w / 2) (h / 2) ], x1 (w / 2), y1 (h / 2), x2 (w / 2), y2 0, TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black) ] []
            , defs[]
            [ radialGradient [ id "base" ]
                [ stop [TypedSvg.Attributes.offset "0%", TypedSvg.Attributes.stopColor "rgba(253, 185, 231, 0.5)" ] []
                , stop [ TypedSvg.Attributes.offset "100%"] []
                ]
            ]
            --, circle [ cx (w/2), cy (40), r 5 ] []
            --, circle [ cx (664), cy (155), r 5 ] []
            --, circle [ transform[Rotate 72 (w/2) (h/2)], cx (w/2), cy (40), r 5 ] []
            , polyline
                [ stroke <| Paint <| Color.rgba 0 0 0 1
                , strokeWidth <| Px 0.5
                , fill <| Paint <| Color.rgba 0 0 0 0.3--0 0.4 0.5 0.3--PaintNone
                , points
                
                    [ ( w / 2, 0 )
                    , ( w / 2 + (h / 2) * 0.951, (h / 2)-(h / 2) * 0.309 )
                    , ( w / 2 + (h / 2) * 0.588, (h / 2)-(h / 2) * -0.809 )
                    , ( w / 2 + (h / 2) * -0.588, (h / 2)-(h / 2) * -0.809 )
                    , ( w / 2 + (h / 2) * -0.951, (h / 2)-(h / 2) * 0.309 )
                    , ( w / 2, 0 )
                    ]
                ][radialGradient [ ]
                [ stop [TypedSvg.Attributes.offset "0%", TypedSvg.Attributes.stopColor "rgba(253, 185, 231, 0.5)" ] []
                , stop [ TypedSvg.Attributes.offset "100%"] []
                ]]
            ,g[] <| List.map (\wp -> 
                polyline
                [ stroke <| Paint <| Color.rgba 1 1 1 0.8
                , strokeWidth <| Px 2
                , fill <| Paint <| Color.rgba 0.0 0.4 0.6 0.4--PaintNone
                , points
                    [ ( w / 2, (h / 2) - (h - Scale.convert yA wp.sweetness) ) 
                    , ( w / 2 + (h - Scale.convert yB wp.alcohol) * 0.951, (h / 2) - (h - Scale.convert yB wp.alcohol) * 0.309 )
                    , ( w / 2 + (h - Scale.convert yC wp.body) * 0.588, (h / 2) - (h - Scale.convert yC wp.body) * -0.809 )
                    , ( w / 2 + (h - Scale.convert yD wp.tannins) * -0.588, (h / 2) - (h - Scale.convert yD wp.tannins) * -0.809 )
                    , ( w / 2 + (h - Scale.convert yE wp.acid) * -0.951, (h / 2) - (h - Scale.convert yE wp.acid) * 0.309 )
                    , ( w / 2, (h / 2) - (h - Scale.convert yA wp.sweetness) )
                    ]
                --, TypedSvg.Events.onClick <| ChangeSelectedWine "" point.wineName
                ]
                [] ) [mean, point]    
        ]]


scatterplot : List Wine -> PlotType -> Svg Msg
scatterplot model char =
    let
        xValues : List Float
        xValues =
            List.map .year model

        yValues : List Float
        yValues =
            case char of 
                Alcohol -> List.map .alcohol model
                Sweetness -> List.map .sweetness model
                Body -> List.map .body model
                Tannins -> List.map .tannins model
                _ -> List.map .acid model
        
        yName : String
        yName = 
            case char of 
                Alcohol -> "Alcohol"
                Sweetness -> "Sweetness"
                Body -> "Body"
                Tannins -> "Tannins"
                _ -> "Acidity"

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale1 yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg
        [ viewBox 0 0 w h
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        [ style [] [ TypedSvg.Core.text """
            
            .point text { display: none; }
            .point:hover text { display: inline; }
          """ ]

        -- .point circle { stroke: rgba(0, 0, 0,0.4);  }
        -- fill: rgba(224, 242, 219,0.3);
        -- .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
        , TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill TypedSvg.Types.PaintNone
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , strokeWidth <| Px 0.5

            --, fill <| Paint <| Color.rgba 0 0 0 1
            ]
            []
        , g
            [ transform [ Translate (padding - 1) (h - padding) ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ xAxis xValues
            , text_
                [ x (w / 2 - 50)
                , y 30
                ]
                [ TypedSvg.Core.text "Aging" ]
            ]
        , g
            [ transform [ Translate (padding - 1) padding ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ yAxis1 yValues
            , text_
                [ x -30
                , y (Scale.convert yScaleLocal labelPositions.y - 20)
                ]
                [ TypedSvg.Core.text yName ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (punkt xScaleLocal yScaleLocal char) model)
        ]


punkt : ContinuousScale Float -> ContinuousScale Float -> PlotType -> Wine -> Svg Msg
punkt scaleX scaleY name xyPoint =
    let
        y : Float
        y = 
            case name of 
                Alcohol -> xyPoint.alcohol
                Sweetness -> xyPoint.sweetness
                Body -> xyPoint.body
                Tannins -> xyPoint.tannins
                _ -> xyPoint.acid
    in
    g [ class [ "point" ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]
        [ circle
            [ fill <| Paint <| Color.rgb255 xyPoint.r xyPoint.g xyPoint.b --(TypedSvg.Types.Paint Color.lightGrey)
            , stroke <| Paint <| Color.rgba 0 0 0 1
            , strokeWidth <| Px 0.5
            , cx (Scale.convert scaleX xyPoint.year)
            , cy (Scale.convert scaleY y)
            , r 15            
            , TypedSvg.Events.onClick <| ChangeSelectedWine xyPoint.wineName name
            ]
            []
        , text_ [ transform [ Translate (Scale.convert scaleX xyPoint.year) (Scale.convert scaleY y - (2 * radius)) ]
                , textAnchor AnchorMiddle] 
                [ TypedSvg.Core.text xyPoint.wineName ]
        ]



wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * xTickCount)
    in
    ( Tuple.first closeExtent - extension
      --|> max 0
    , Tuple.second closeExtent + extension
    )


tickCount_ : Int
tickCount_ =
    10


xTickCount : Int
xTickCount =
    10


yTickCount : Int
yTickCount =
    10


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale1 : List Float -> ContinuousScale Float
yScale1 values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h, h / 2 ) <| Tuple.mapFirst (max 0) <| wideExtent values


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount xTickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount yTickCount ] (yScale values)


yAxis1 : List Float -> Svg msg
yAxis1 values =
    Axis.left [ Axis.tickCount yTickCount ] (yScale1 values)


a : List Float
a =
    List.map .sweetness wines


b : List Float
b =
    List.map .alcohol wines


c : List Float
c =
    List.map .body wines


d : List Float
d =
    List.map .tannins wines


e : List Float
e =
    List.map .acid wines
    
mean : Wine
mean = 
    let
        getMax li = (List.sum li) / (toFloat (List.length li))
    in
        Wine "" "" (getMax a) (getMax b) (getMax c) (getMax d) (getMax e) 0 0 0 0 (Maps.Geo.latLng 0 0)
        


yA : ContinuousScale Float
yA =
    yScale a


yB : ContinuousScale Float
yB =
    yScale b


yC : ContinuousScale Float
yC =
    yScale c


yD : ContinuousScale Float
yD =
    yScale d


yE : ContinuousScale Float
yE =
    yScale e


type alias Point =
    { pointName : String, a : Float, b : Float, c : Float, d : Float, e : Float }


type alias Wine =
    { wineName : String
    , color : String
    , sweetness : Float
    , alcohol : Float
    , body : Float
    , tannins : Float
    , acid : Float
    , year : Float
    , r : Int
    , g : Int
    , b : Int
    , coord : Maps.Geo.LatLng
    }


wines : List Wine
wines =
    [ Wine "sparkling wine" "almost clear, straw" 1 12.5 3 1 5 1 244 242 219 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "vinho verde" "pale straw" 2 11 1 1 5 3 244 242 219 (Maps.Geo.latLng 39.557 -7.8536)
    , Wine "muscadet" "yellow" 1 12 1 1 5 3 245 243 176 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "riesling" "yellow" 3 8 1 1 5 2.5 245 243 176 (Maps.Geo.latLng 51.1642 10.4541)
    , Wine "sauvignon blanc" "green yellow" 2 14 2 1 5 3 191 209 117 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "verdejo" "green yellow" 2 13 2 1 4 3 191 209 117 (Maps.Geo.latLng 40.416775 -3.703790)
    , Wine "Gruener Veltliner" "green yellow" 2 11.5 2 1 5 10 191 209 117 (Maps.Geo.latLng 47.6964719 13.3457347)
    , Wine "albarino" "platinum yellow" 2 13.5 1 1 5 5 215 219 157 (Maps.Geo.latLng 40.416775 -3.703790)
    , Wine "pinot gris" "platinum yellow" 2 14.5 1 1 4 5 215 219 157 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "semillon" "platinum yellow" 2 14 5 1 3 8 215 219 157 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "garganega" "platinum yellow" 2 14 2 1 4 8 215 219 157 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "Chenin blanc" "pale yellow" 2 14.5 1 1 5 8 225 222 128 (Maps.Geo.latLng -26.195246 28.034088)
    , Wine "moscato" "pale yellow" 5 9 1 1 3 1 225 222 128 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "pinot blanc" "pale yellow" 2 15 1 1 3 2 225 222 128 (Maps.Geo.latLng 51.1642 10.4541)
    , Wine "gewuerztraminer" "pale yellow" 3 14.5 3 1 1 3 225 222 128 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "chardonnay" "pale gold" 2 14.5 4 1 3 6 219 193 78 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "roussanne" "pale gold" 2 15 3 1 3 8 219 193 78 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "viognier" "pale gold" 2 14.5 3 1 2 5 219 193 78 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "noble rot wines riesling" "deepgold" 3 13 3 4 5 3 222 163 0 (Maps.Geo.latLng 51.1642 10.4541)
    , Wine "rioja" "deepgold" 2 13.5 4 4 4 10 222 163 0 (Maps.Geo.latLng 40.416775 -3.703790)
    , Wine "white port" "pale amber" 5 20 5 1 4 10 250 207 102 (Maps.Geo.latLng 39.557 -7.8536)
    , Wine "tokaji" "medium amber" 5 14 5 1 5 15 244 171 59 (Maps.Geo.latLng 47.497913 19.040236)
    , Wine "rose of pinot noir" "pale salmon" 2 12.9 3 2 4 7.5 222 164 107 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "carignan" "pale salmon" 2 15 3 3 4 8 222 164 107 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "zinfandel" "pale salmon" 2 13.5 4 4 2 5 222 164 107 (Maps.Geo.latLng 36.778259 -119.417931)
    , Wine "rose of merlot" "deep pink" 1 14 4 3 3 11 205 41 94 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "grenache" "deep pink" 2 15 4 3 3 7.5 205 41 94 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "sangiovese" "deep pink" 1 14.5 4 4 4 15 205 41 94 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "rose of cabernet" "deep salmon" 2 11 5 4 3 7.5 204 61 60 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "tempranillo" "deep salmon" 2 12.5 4 4 4 8 204 61 60 (Maps.Geo.latLng 40.416775 -3.703790)
    , Wine "pinot noir" "pale ruby" 2 13.5 3 2 4 7.5 172 18 46 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "nebbiolo" "pale ruby" 1 14 4 4 4 8 172 18 46 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "merlot" "deep violet" 1 14.5 4 4 4 11 72 0 22 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "cabernet franc" "deep violet" 2 11 3 4 4 9 72 0 22 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "barbera" "deep violet" 1 15.5 4 2 5 10 72 0 22 (Maps.Geo.latLng 41.2925 12.5736)
    , Wine "syrah" "deep purple" 2 15.5 4 4 3 7.5 55 13 26 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "cabernet sauvignon" "deep purple" 2 13.5 5 4 3 12.5 55 13 26 (Maps.Geo.latLng 46.7111 1.7191)
    , Wine "malbec" "deep purple" 2 13.5 5 3 2 7.5 55 13 26 (Maps.Geo.latLng -38.4192641 -63.5989206)
    , Wine "mourvedre" "deep purple" 2 14 6 5 3 10 55 13 26 (Maps.Geo.latLng 40.416775 -3.703790)
    , Wine "petite sirah" "deep purple" 2 20 5 5 3 15 55 13 26 (Maps.Geo.latLng -35.473469 149.012375)
    , Wine "port" "tawny" 5 19 5 5 4 20 153 55 3 (Maps.Geo.latLng 39.557 -7.8536)
    , Wine "tawny port" "tawny" 5 20 5 5 4 25 153 55 3 (Maps.Geo.latLng 39.557 -7.8536)
    ]
 