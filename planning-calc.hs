module Main where

import Data.List (replicate)
import Data.Time
import Control.Monad

type Name = String
type Duration = Integer

data Release = Release Name [Phase] deriving Show
data Phase = Phase Name [Step] deriving Show
data Step = SimpleStep Name Duration | ParallelStep Name Duration [Step] deriving Show

data PRelease = PRelease Name UTCTime Duration [PPhase] deriving Show
data PPhase = PPhase Name UTCTime Duration [PStep] deriving Show
data PStep = PSimpleStep Name UTCTime Duration | PParallelStep Name UTCTime Duration [PStep] deriving Show

create_planning :: Release -> UTCTime -> PRelease
create_planning (Release r_name phases) start = PRelease r_name start r_duration phases'
    where
        phases' = bla phases start
        r_duration = sum $ map phase_calc_duration phases
        bla :: [Phase] -> UTCTime -> [PPhase]
        bla [] _ = []
        bla (p@(Phase name steps):ps) now = p' : bla ps now'
            where
                phase_duration = phase_calc_duration p
                p' = PPhase name now phase_duration steps'
                now' = addUTCTime (realToFrac (phase_duration * 60)) now
                steps' = bla2 steps now
                    where
                        bla2 :: [Step] -> UTCTime -> [PStep]
                        bla2 [] _ = []
                        bla2 (s:ss) s_start = s':bla2 ss s_start'
                            where
                                (s', s_start') = convert_step s s_start


convert_step :: Step -> UTCTime -> (PStep, UTCTime)
convert_step (SimpleStep name duration) start = (PSimpleStep name start duration, start')
    where
        start' = addUTCTime (realToFrac (duration * 60)) start
convert_step (ParallelStep name _ steps) start = (PParallelStep name start duration steps'', start')
    where
        duration = maximum $ map step_calc_duration steps
        start' = addUTCTime (realToFrac (duration * 60)) start
        steps' = map (flip convert_step start) steps
        steps'' = map fst steps'


phase_calc_duration :: Phase -> Integer
phase_calc_duration (Phase _ steps) = sum $ map step_calc_duration steps

step_calc_duration :: Step -> Integer
step_calc_duration (SimpleStep _ duration) = duration
step_calc_duration (ParallelStep _ _ steps) = maximum $ map step_calc_duration steps

release :: Release
release = Release "app-16.05" phases
    where
        phases = [phase_stop, phase_migration, phase_start, phase_phony]
        phase_stop = Phase "Stop" [ SimpleStep "Notification" 1
                                  , ParallelStep "Stop" 0 [ SimpleStep "Stop App" 10
                                                          , SimpleStep "Stop App2" 15
                                                          , SimpleStep "Stop App3" 5
                                                          ]
                                  , SimpleStep "Deactivate database tasks" 5
                                  ]
        phase_migration = Phase "Migration" [ SimpleStep "Undeploy App" 1
                                            , ParallelStep "Migration" 0 [ SimpleStep "Migration Servers" 90
                                                                         , SimpleStep "Migration Database" 60
                                                                         , SimpleStep "Migration template" 0
                                                                         ]
                                            , SimpleStep "Migration finished" 0
                                            ]
        phase_start = Phase "Start" [ SimpleStep "Notification" 1
                                  , ParallelStep "Start" 0 [ SimpleStep "Start App" 10
                                                          , SimpleStep "Start App2" 15
                                                          , SimpleStep "Start App3" 5
                                                          ]
                                  , SimpleStep "Start database tasks" 5
                                  ]
        phase_phony = Phase "Phone" [ SimpleStep "Notification" 1
                                    , ParallelStep "Block 1" 0 [ SimpleStep "Start Block 1" 1
                                                               , ParallelStep "Subblock 1" 0 [ SimpleStep "Start subblock 1.a" 10
                                                                                             , SimpleStep "Start subblock 1.b" 15]
                                                               , ParallelStep "Subblock 2" 0 [ SimpleStep "Start subblock 2.a" 9
                                                                                             , SimpleStep "Start subblock 2.b" 1
                                                                                             ]
                                                               ]
                                    ]


get_phases :: Release -> [Phase]
get_phases (Release _ p) = p

display_release :: PRelease -> IO ()
display_release (PRelease r_name start duration phases) = do
    putStrLn $ "Release: " ++ r_name ++ " @ " ++ format_time start ++ " ~ " ++ show duration ++ " minutes"
    mapM_ (display_phase 1) phases
    where

format_time :: UTCTime -> String
format_time = formatTime defaultTimeLocale "%Y/%m/%d %H:%M"

display_phase :: Int -> PPhase -> IO ()
display_phase level (PPhase name start duration steps) = do
    putStrLn $ indent ++ "Phase: " ++ name ++ " @ " ++ format_time start ++ " ~ " ++ show duration ++ " minutes"
    mapM_ (display_step (level+1)) steps
    where
        indent = replicate level '\t'

display_step :: Int -> PStep -> IO ()
display_step level (PSimpleStep name start duration) = do
    putStrLn $ indent ++ "Step: " ++ name ++ " @ " ++ format_time start ++ " ~ " ++ show duration ++ " minutes"
    where
        indent = replicate level '\t'
display_step level (PParallelStep name start duration steps) = do
    putStrLn $ indent ++ "PStep: " ++ name ++ " @ " ++ format_time start ++ " ~ " ++ show duration ++ " minutes"
    mapM_ (display_step (level+1)) steps
    where
        indent = replicate level '\t'

do_test :: Release -> IO ()
do_test release = do
    now <- getCurrentTime
    let p = create_planning release now
    display_release p

main :: IO ()
main = putStrLn "Hello World"
