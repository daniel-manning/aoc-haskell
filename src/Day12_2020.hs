module Day12_2020
    (
    ) where

    import Text.ParserCombinators.Parsec
    import Data.Either.Combinators
    import Data.List (foldl')

    data Position = Position Double Double deriving Show
    data Ship = Ship Double Position deriving Show
    
    newtype ShipWaypoint = ShipWaypoint Position deriving Show
    newtype Waypoint = Waypoint Position deriving Show

    data Command = North Double | East Double | South Double | West Double | LeftTurn Double | RightTurn Double | Forward Double deriving Show

    parseNorth =  do
        char 'N'
        n <- many1 digit
        return $ North (read n) 

    parseEast =  do
        char 'E'
        n <- many1 digit
        return $ East (read n)
    
    parseSouth =  do
        char 'S'
        n <- many1 digit
        return $ South (read n)

    parseWest =  do
        char 'W'
        n <- many1 digit
        return $ West (read n)

    parseLeft =  do
        char 'L'
        n <- many1 digit
        return $ LeftTurn (read n)

    parseRight =  do
        char 'R'
        n <- many1 digit
        return $ RightTurn (read n)

    parseForward =  do
        char 'F'
        n <- many1 digit
        return $ Forward (read n)    

    parseCommand = choice [try parseNorth, try parseEast, try parseSouth, try parseWest, try parseLeft, try parseRight, try parseForward]

    turn :: Double -> Command -> Double
    turn d (LeftTurn n) = (d + n) `doubleMod` 360
    turn d (RightTurn n) = (d - n) `doubleMod` 360

    doubleMod :: Double -> Double -> Double
    doubleMod x n | x < 0 = doubleMod (n + x) n
                  | x >= n = doubleMod (x - n) n
                  | otherwise = x

    evaluateCommand :: Ship -> Command -> Ship
    evaluateCommand (Ship d (Position x y)) (North n)   =     Ship d (Position x     (y+n))
    evaluateCommand (Ship d (Position x y)) (East n)    =     Ship d (Position (x+n)  y)
    evaluateCommand (Ship d (Position x y)) (South n)   =     Ship d (Position x     (y-n))
    evaluateCommand (Ship d (Position x y)) (West n)    =     Ship d (Position (x-n) y)
    evaluateCommand (Ship degree p) (LeftTurn n)        =     Ship (turn degree (LeftTurn n)) p
    evaluateCommand (Ship degree p) (RightTurn n)       =     Ship (turn degree (RightTurn n)) p
    evaluateCommand (Ship d (Position x y)) (Forward n) =     Ship d (Position (x + xStep) (y + yStep))
        where
           xStep = n*cos (pi * d / 180 )
           yStep = n*sin (pi * d / 180 )
    
    runInstructions :: Ship -> [Command] -> Ship
    runInstructions = foldl' evaluateCommand

    parseInstructions = map (fromRight' . parse parseCommand "")

    manhattenDistance x y = abs x + abs y

    day12Pt1 = (\(Ship _ (Position x y)) -> manhattenDistance x y) . runInstructions (Ship 0.0 (Position 0.0 0.0)) . parseInstructions <$> readInstructions

    -----------------

    rotate :: Double -> Double ->  Command -> Position
    rotate wx wy (LeftTurn n) = Position (radius*cos (angle + rad_n)) (radius*sin (angle + rad_n))
        where
            angle = atan2 wy wx
            rad_n = pi*n/180
            radius = sqrt $ wx^2 + wy^2
    rotate wx wy (RightTurn n) = Position (radius*cos (angle - rad_n)) (radius*sin (angle - rad_n))
        where
            angle = atan2 wy wx
            rad_n = pi*n/180
            radius = sqrt $ wx^2 + wy^2     

    evaluateWaypointCommand :: (ShipWaypoint, Waypoint) -> Command -> (ShipWaypoint, Waypoint)
    evaluateWaypointCommand (s, Waypoint (Position wx wy)) (North n)   =     (s, Waypoint (Position wx (wy+n)))
    evaluateWaypointCommand (s, Waypoint (Position wx wy)) (East n)    =     (s, Waypoint (Position (wx+n)  wy))
    evaluateWaypointCommand (s, Waypoint (Position wx wy)) (South n)   =     (s, Waypoint (Position wx     (wy-n)))
    evaluateWaypointCommand (s, Waypoint (Position wx wy)) (West n)    =     (s, Waypoint (Position (wx-n) wy))
    evaluateWaypointCommand (ShipWaypoint (Position x y), Waypoint (Position wx wy)) (LeftTurn n)  = (ShipWaypoint (Position x y), Waypoint $ rotate wx wy (LeftTurn n))
    evaluateWaypointCommand (ShipWaypoint (Position x y), Waypoint (Position wx wy)) (RightTurn n) = (ShipWaypoint (Position x y), Waypoint $ rotate wx wy (RightTurn n))
    evaluateWaypointCommand (ShipWaypoint (Position x y), Waypoint (Position wx wy)) (Forward n)   = (ShipWaypoint (Position (x + n*wx) (y + n*wy)), Waypoint (Position wx wy))

    runWaypointInstructions :: (ShipWaypoint, Waypoint) -> [Command] -> (ShipWaypoint, Waypoint)
    runWaypointInstructions = foldl' evaluateWaypointCommand

    day12Pt2 = (\(ShipWaypoint (Position x y), _) -> manhattenDistance x y) . runWaypointInstructions (ShipWaypoint (Position 0.0 0.0), Waypoint (Position 10.0 1.0)) . parseInstructions <$> readInstructions

    readInstructions :: IO [String]
    readInstructions = lines <$> readFile "resource/2020/day12"
    