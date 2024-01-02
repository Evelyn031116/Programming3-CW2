module Test where
import Challenges
import Test.HUnit

testChallenge1 :: Test
testChallenge1 = TestList [
        TestCase (
            assertEqual "Challenge 1: Test case #1: The given S puzzle in coursework spec is complete"
                (isPuzzleComplete [[ Wire [East, South], Wire [West, East], Source [West] ], [ Wire [North,East], Wire [East,West], Wire [West,South] ], [ Sink [East] , Wire [West,East] , Wire [North,West] ] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #2: Simple 1x2 puzzle [es] is complete"
                (isPuzzleComplete [ [Sink [East], Source [West]] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #3: Simple 1x3 puzzle [e s] is not complete"
                (isPuzzleComplete [ [Sink [East], Wire [], Source [West]] ])
                False
        ),
       TestCase (
            assertEqual "Challenge 1: Test case #4: Is not complete because incomplete wire/s"
                (isPuzzleComplete [ [Wire [], Source [South]], [Wire [West, East], Wire [North, West, South]], [Wire [], Sink [North]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #5: A puzzle with a isolated looping wire, is connected"
                (isPuzzleComplete [ [Source [East], Sink [West], Wire []], [Wire [], Wire [South, East], Wire [West, South]], [Wire [], Wire [North, East], Wire [North, West]] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #6: A cross with 2 sources and 2 sinks, is connected"
                (isPuzzleComplete [ [Wire [], Source [South], Wire []], [Source [East], Wire [North, South, West, East], Sink [West]], [Wire [], Sink [North], Wire []] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #7: A partial connected puzzle with 3 widowed wires, is not connected"
                (isPuzzleComplete [ [Wire [East, South], Wire [West, East], Source [West]], [Wire [North, East], Wire [West, East], Sink [West]], [Wire [North, South], Wire [West, East], Wire [South, East]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #8: A puzzle with 2 connected components, where each connected component (may have loops) is connected, is connected"
                (isPuzzleComplete [ [Wire [South, East], Source [West], Source [South]], [Sink [North], Wire [South, East], Wire [North, West, South]], [Sink [East], Wire [North, West, East], Wire [North, West]] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #9: A puzzle with 1 component but has multiple loops, is connected"
                (isPuzzleComplete [ [Wire [East, South], Wire [West, East, South], Source [West]], [Wire [North, South, East], Sink [North, West, East], Wire [West, South]], [Sink [North, East], Wire [West, East], Wire [North, West]] ])
                True
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #10: A puzzle without sources or sinks is not connected"
                (isPuzzleComplete [ [Wire [South, East], Wire [West, South]], [Wire [North, East], Wire [North, West]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #11: A puzzle with a loop without sinks is not connected"
                (isPuzzleComplete [ [Wire [South, East], Source [West, South]], [Wire [North, East], Wire [North, West]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #12: A puzzle with a loop without sources is not connected"
                (isPuzzleComplete [ [Wire [South, East], Wire [West, South]], [Sink [North, East], Wire [North, West]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #13: A puzzle with a loop without sinks is not connected"
                (isPuzzleComplete [ [Source [East], Wire [West, South, East], Wire [West, South]], [Wire [], Wire [North, East], Wire [North, West]] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #14: An empty puzzle is not connected"
                (isPuzzleComplete [ [Wire []] ])
                False
        ),
        TestCase (
            assertEqual "Challenge 1: Test case #15: A puzzle with 2 components are connected"
                (isPuzzleComplete [ [Wire [South, East], Source [West], Sink [East], Wire [West, South]], [Wire [North, South], Wire [South, East], Wire [West, South], Wire [North, South]], [Wire [North, South], Sink [North], Source [North], Wire [North, South]], [Wire [North, East], Wire [West, East], Wire [East, West], Wire [North, West]] ])
                True
        ),
       TestCase (
            assertEqual "Challenge 1: Test case #16: A 40x40 puzzle from @Pretty Pigeon (Discord)"
                (isPuzzleComplete [[Wire[],Wire[South,West],Wire[South,West],Wire[],Wire[South,West],Wire[East,South],Wire[],Wire[North,West],Wire[East,South],Wire[],Wire[],Wire[East,South],Wire[North,South],Wire[East,West],Wire[North,South],Wire[North,West],Wire[],Wire[],Wire[],Wire[],Wire[North,East],Wire[South,West],Wire[North,West],Wire[South,West],Wire[North,West],Wire[South,West],Wire[North,East],Wire[South,West],Wire[North,West],Wire[North,East],Wire[East,South],Wire[North,South],Wire[East,South],Wire[],Wire[],Wire[],Wire[],Wire[North,East],Wire[East,West],Wire[South,West]],[Wire[East,South],Wire[North,East],Wire[East,West],Wire[],Wire[North,South],Wire[North,West],Wire[North,West],Wire[East,West],Wire[North,West],Sink[South,West],Sink[North,East],Wire[North,East,South,West],Wire[East,South],Wire[North,West],Wire[North,South],Wire[North,East,South,West],Wire[North,West],Wire[],Wire[],Wire[South,West],Wire[North,East],Wire[North,East],Wire[East,South],Wire[North,West],Wire[North,East,South,West],Wire[South,West],Wire[East,South],Wire[North,East,South,West],Wire[North,East],Wire[North,East],Wire[North,East],Wire[South,West],Source[North,West],Wire[South,West],Wire[North,South],Wire[East,West],Wire[South,West],Sink[North,South],Wire[],Wire[North,South]],[Wire[South,West],Wire[East,West],Wire[East,South],Wire[South,West],Wire[North,East,South,West],Wire[North,West],Wire[South,West],Wire[East,South],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[South,West],Wire[South,West],Wire[East,South],Wire[],Wire[North,South],Wire[South,West],Wire[South,West],Wire[],Wire[North,East],Wire[South,West],Wire[],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[South,West],Wire[East,South],Wire[North,East,South,West],Wire[East,South],Wire[North,West],Wire[North,West],Wire[East,West],Wire[],Wire[North,South],Wire[North,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[East,West]],[Wire[South,West],Wire[North,South],Wire[East,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Sink[North,West],Wire[North,South],Wire[East,South],Wire[],Wire[North,South],Wire[],Wire[],Wire[],Wire[],Wire[East,West],Wire[South,West],Wire[North,East,South,West],Wire[North,South],Wire[South,West],Wire[East,West],Wire[North,East],Wire[North,East],Wire[East,West],Wire[North,South],Wire[North,South],Wire[North,South],Wire[North,South],Wire[East,West],Wire[South,West],Wire[South,West],Wire[South,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,West],Wire[South,West],Wire[],Wire[],Wire[North,South]],[Wire[East,South],Wire[East,West],Wire[North,East],Wire[East,West],Wire[East,West],Wire[South,West],Wire[North,East],Sink[North,East],Wire[North,East,South,West],Wire[East,South],Wire[North,East],Wire[North,South],Wire[East,West],Wire[East,West],Wire[East,West],Wire[North,West],Wire[North,South],Wire[North,South],Wire[South,West],Wire[North,East,South,West],Wire[North,West],Wire[South,West],Wire[North,East,South,West],Wire[North,West],Wire[East,West],Sink[East,West],Wire[North,South],Wire[East,South],Wire[East,South],Wire[North,East],Wire[North,South],Wire[North,South],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[East,South],Wire[North,South],Wire[North,South],Wire[East,South]],[Wire[East,South],Wire[East,West],Wire[North,East],Wire[North,South],Wire[South,West],Wire[North,East,South,West],Wire[North,South],Source[North,East,South,West],Wire[North,East,South,West],Wire[North,East],Wire[South,West],Wire[North,South],Wire[East,West],Wire[South,West],Wire[],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,West],Wire[South,West],Wire[South,West],Wire[East,South],Wire[North,South],Wire[East,West],Wire[North,South],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[East,South],Wire[North,South],Wire[South,West],Wire[East,West],Sink[North,West]],[Wire[East,West],Wire[East,South],Wire[North,West],Wire[East,South],Wire[East,South],Wire[North,South],Wire[North,West],Wire[North,West],Wire[North,South],Wire[North,West],Wire[North,East,South,West],Wire[North,South],Wire[North,South],Wire[North,East,South,West],Wire[East,West],Wire[South,West],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[East,South],Wire[],Sink[East,South],Wire[East,West],Wire[North,East],Wire[East,South],Wire[North,East,South,West],Wire[South,West],Wire[North,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Sink[North,West],Wire[East,South],Wire[East,West],Wire[North,East,South,West],Wire[East,South],Wire[North,South],Wire[North,East],Wire[North,East],Wire[North,South]],[Wire[North,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,East],Wire[North,South],Wire[East,South],Wire[North,West],Wire[East,South],Wire[South,West],Wire[North,East],Wire[East,West],Wire[North,South],Wire[North,East,South,West],Wire[East,West],Wire[North,East],Wire[South,West],Wire[East,West],Wire[North,East],Wire[],Wire[North,West],Wire[East,South],Wire[],Wire[],Wire[North,West],Wire[South,West],Wire[North,East],Source[North,East,South,West],Wire[South,West],Wire[East,West],Wire[East,West],Wire[East,West],Wire[East,West],Wire[East,South],Wire[North,East],Wire[North,West],Wire[North,East,South,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East]],[Wire[],Wire[East,South],Wire[South,West],Wire[],Wire[],Wire[East,West],Wire[South,West],Wire[North,West],Wire[East,South],Wire[North,East],Wire[],Wire[],Wire[East,South],Wire[East,South],Wire[North,East],Wire[North,South],Wire[North,East],Wire[],Wire[],Wire[East,South],Wire[North,West],Wire[North,East],Wire[North,West],Wire[East,South],Wire[North,East,South,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East],Wire[South,West],Wire[South,West],Wire[North,West],Wire[North,East],Wire[East,West],Wire[North,West],Sink[North,East],Wire[North,East],Wire[East,South],Wire[North,West],Wire[East,South],Wire[]],[Wire[North,West],Wire[North,East],Wire[],Wire[North,East],Wire[South,West],Wire[North,South],Wire[East,South],Wire[North,West],Wire[North,East],Wire[North,West],Wire[North,East],Wire[North,East],Wire[North,East],Wire[East,West],Sink[South,West],Wire[],Wire[North,West],Wire[East,West],Wire[North,South],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[North,West],Wire[],Wire[East,South],Wire[North,South],Wire[South,West],Wire[South,West],Wire[North,West],Wire[],Wire[East,South],Wire[East,South],Wire[East,West],Wire[North,West],Wire[East,West],Wire[South,West],Wire[],Wire[]],[Wire[East,West],Wire[North,South],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,West],Wire[North,East],Wire[North,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,South],Wire[East,West],Wire[North,South],Wire[North,East],Wire[],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[North,South],Wire[South,West],Wire[North,South],Wire[],Wire[],Wire[],Wire[East,South],Wire[North,South],Wire[South,West],Sink[East,West],Wire[],Wire[North,East],Wire[East,South],Wire[North,South],Wire[],Wire[],Wire[North,West],Wire[North,West],Wire[]],[Sink[North,East],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[North,South],Wire[East,West],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[East,West],Wire[North,West],Sink[South,West],Wire[North,East],Wire[South,West],Wire[North,East],Wire[East,South],Wire[North,South],Wire[North,West],Wire[],Wire[North,West],Wire[North,East],Wire[],Wire[North,South],Wire[North,South],Wire[],Wire[],Wire[],Wire[North,West],Wire[South,West],Wire[South,West],Wire[North,East],Wire[],Wire[North,West],Wire[South,West],Wire[South,West],Wire[North,South],Wire[South,West],Wire[North,East],Wire[North,West],Wire[]],[Wire[],Wire[North,East],Wire[North,East,South,West],Wire[North,West],Wire[North,West],Wire[North,East],Wire[North,East],Wire[North,East],Wire[East,West],Wire[East,South],Wire[North,South],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[East,South],Wire[East,West],Wire[South,West],Wire[],Wire[],Wire[North,South],Wire[East,West],Wire[],Wire[North,West],Wire[South,West],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[],Wire[East,West],Wire[],Wire[North,East],Wire[North,West],Wire[],Wire[South,West],Wire[South,West]],[Wire[North,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[South,West],Wire[North,East],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[East,South],Sink[East,South],Wire[North,West],Wire[North,West],Wire[North,East,South,West],Sink[North,South],Wire[East,South],Wire[East,South],Wire[East,West],Wire[North,South],Wire[East,West],Wire[North,East],Wire[North,East],Wire[South,West],Wire[North,East],Wire[South,West],Wire[North,East],Wire[North,East,South,West],Wire[South,West],Wire[North,West],Wire[East,South],Wire[],Wire[North,West],Wire[North,West],Wire[],Wire[South,West],Wire[North,West],Wire[],Wire[East,South],Wire[North,West]],[Wire[East,West],Wire[],Wire[East,West],Wire[North,South],Wire[East,West],Wire[North,South],Wire[North,West],Wire[East,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[North,West],Wire[East,West],Wire[East,West],Sink[North,East,South,West],Wire[North,West],Wire[North,South],Wire[East,West],Wire[North,East],Wire[North,East],Wire[South,West],Wire[North,East],Wire[North,West],Wire[East,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[East,South],Wire[South,West],Wire[],Wire[South,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[North,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East],Wire[South,West],Wire[East,South]],[Wire[East,West],Wire[North,West],Wire[North,East],Wire[South,West],Wire[North,East,South,West],Wire[East,South],Sink[East,South],Wire[North,South],Wire[North,West],Wire[North,South],Wire[],Wire[North,East],Wire[East,West],Wire[East,West],Wire[North,West],Sink[East,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,South],Wire[North,South],Wire[North,West],Wire[East,South],Wire[],Wire[North,South],Wire[East,West],Wire[],Wire[],Wire[North,West],Wire[North,West],Wire[East,South],Wire[North,South],Wire[South,West],Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[South,West],Wire[North,East],Wire[North,South],Wire[East,West]],[Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[East,South],Wire[East,South],Wire[North,West],Wire[North,West],Wire[North,East],Wire[North,West],Wire[North,South],Wire[],Wire[East,West],Wire[South,West],Wire[South,West],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[South,West],Sink[North,South],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[East,West],Wire[East,West],Wire[South,West],Wire[East,South],Wire[North,East],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[East,West],Wire[East,South],Wire[],Wire[North,East],Wire[East,South],Wire[East,West],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[North,East]],[Wire[North,West],Wire[North,East,South,West],Wire[North,West],Wire[South,West],Wire[North,East],Wire[North,West],Wire[East,South],Wire[North,East],Wire[North,West],Wire[North,South],Wire[North,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,East,South,West],Wire[North,West],Wire[East,South],Wire[North,East,South,West],Wire[North,South],Wire[North,South],Wire[North,West],Wire[],Wire[North,West],Wire[South,West],Wire[North,South],Wire[South,West],Wire[South,West],Wire[South,West],Wire[East,South],Wire[East,West],Wire[],Wire[North,South],Wire[],Wire[East,West],Wire[East,West],Wire[]],[Wire[East,West],Wire[North,East],Wire[East,West],Sink[East,South],Wire[North,West],Wire[North,West],Wire[South,West],Wire[East,West],Wire[North,East,South,West],Wire[East,South],Wire[East,West],Wire[North,South],Wire[East,West],Wire[],Wire[North,West],Wire[North,South],Wire[North,West],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[East,West],Sink[South,West],Wire[South,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Sink[East,West],Wire[North,East,South,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,West],Wire[East,West],Wire[],Wire[North,South],Wire[North,South],Wire[]],[Wire[South,West],Wire[South,West],Wire[],Wire[North,South],Wire[South,West],Wire[North,West],Wire[North,East],Wire[North,East],Wire[North,West],Sink[North,East],Wire[East,West],Wire[South,West],Wire[North,East,South,West],Wire[North,East],Wire[East,West],Wire[],Wire[],Wire[North,South],Wire[North,South],Wire[North,South],Wire[North,South],Wire[South,West],Wire[East,South],Wire[South,West],Sink[East,South],Wire[North,East],Wire[North,East],Wire[East,South],Wire[East,South],Wire[],Wire[South,West],Wire[North,South],Wire[North,East,South,West],Wire[East,South],Wire[North,East],Wire[North,East,South,West],Wire[North,West],Wire[North,West],Wire[North,West],Wire[]],[Wire[],Wire[East,West],Wire[],Wire[East,West],Wire[North,East],Wire[North,South],Wire[East,West],Sink[North,West],Wire[],Wire[North,South],Wire[North,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,East],Wire[East,West],Sink[North,East],Wire[North,East],Sink[North,South],Wire[North,South],Wire[North,South],Wire[],Wire[East,West],Wire[North,West],Wire[East,South],Wire[East,South],Wire[North,South],Wire[East,West],Wire[North,East,South,West],Wire[North,West],Wire[North,East],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[],Wire[]],[Wire[North,West],Wire[East,South],Wire[North,East],Wire[North,East,South,West],Wire[East,West],Wire[North,South],Wire[North,South],Wire[East,West],Wire[North,South],Wire[South,West],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,West],Wire[North,South],Wire[East,West],Wire[North,East],Wire[North,East],Wire[North,West],Wire[South,West],Wire[North,East],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,West],Wire[East,West],Wire[South,West],Wire[North,West],Wire[South,West],Wire[East,West],Wire[North,West],Wire[East,South],Wire[North,East],Wire[East,West],Wire[],Wire[North,West],Wire[East,West],Wire[North,East]],[Sink[North,South],Wire[],Wire[North,East],Source[North,East],Wire[],Wire[],Sink[North,East],Wire[South,West],Wire[South,West],Wire[North,East],Wire[North,East],Wire[East,South],Wire[East,West],Wire[East,West],Wire[East,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,East,South,West],Wire[North,South],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,West],Wire[East,West],Sink[North,West],Wire[North,East],Wire[North,South],Wire[South,West],Wire[],Wire[],Wire[],Wire[East,South],Wire[North,East],Wire[],Wire[East,South],Wire[North,South],Wire[North,West]],[Wire[North,East],Wire[South,West],Wire[North,East],Wire[North,East],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[North,East,South,West],Sink[North,East,South,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[East,West],Wire[South,West],Wire[North,East,South,West],Wire[North,East],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East],Wire[East,West],Wire[South,West],Wire[North,West],Wire[South,West],Wire[North,East,South,West],Wire[East,West],Wire[East,West],Wire[North,West],Wire[North,West],Wire[North,South],Wire[North,East],Wire[],Wire[South,West],Wire[South,West],Wire[East,West],Sink[North,East],Wire[North,South],Wire[North,South],Wire[North,East],Wire[]],[Wire[],Wire[East,West],Sink[North,South],Wire[North,South],Wire[East,South],Wire[East,South],Wire[North,South],Wire[North,East],Wire[North,West],Wire[South,West],Wire[East,West],Wire[East,West],Wire[North,East,South,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[East,South],Wire[North,East,South,West],Wire[East,South],Wire[East,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[North,South],Wire[East,South],Wire[East,West],Wire[East,South],Wire[South,West],Wire[South,West],Sink[East,West],Wire[North,West],Wire[North,West],Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[North,East],Wire[South,West],Wire[]],[Wire[South,West],Wire[North,East,South,West],Sink[East,South],Wire[South,West],Wire[North,East,South,West],Wire[North,South],Wire[North,West],Wire[],Wire[],Wire[],Wire[North,East],Wire[East,West],Wire[North,East],Wire[North,East],Wire[South,West],Wire[North,South],Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[South,West],Wire[North,East],Wire[North,East],Wire[North,East],Wire[South,West],Wire[North,East,South,West],Wire[North,West],Wire[],Wire[North,West],Wire[North,South,West],Wire[East,West],Wire[North,East],Wire[East,South],Wire[South,West],Wire[South,West],Wire[East,South],Wire[East,West],Wire[North,South],Wire[East,South],Wire[],Wire[]],[Wire[East,West],Wire[North,West],Wire[South,West],Sink[East,South],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[North,West],Wire[North,South],Wire[East,West],Wire[North,East],Wire[],Wire[],Wire[South,West],Wire[North,West],Wire[North,South],Wire[],Sink[East,South],Wire[North,East],Wire[East,West],Wire[],Wire[],Wire[North,West],Wire[North,West],Wire[East,West],Wire[South,West],Wire[North,East],Wire[North,South],Wire[East,South],Wire[North,East,West],Wire[North,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[East,West],Wire[North,South],Wire[North,West],Wire[North,East],Wire[East,South]],[Wire[North,East],Wire[North,West],Wire[South,West],Wire[South,West],Wire[North,South],Wire[East,West],Wire[],Wire[North,East,West],Wire[North,South,West],Wire[East,West],Wire[North,South],Source[East,South],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[],Wire[South,West],Source[North,West],Wire[],Wire[North,West],Wire[North,East,South,West],Wire[North,West],Wire[North,West],Wire[North,East],Wire[South,West],Wire[North,South],Wire[East,South],Wire[North,East],Wire[North,East],Wire[North,West],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[North,East],Wire[North,West],Wire[North,West],Wire[North,West]],[Wire[South,West],Wire[East,South],Wire[],Wire[North,West],Wire[North,East],Wire[East,South],Wire[North,West],Wire[North,South],Wire[East,West],Wire[],Wire[],Wire[North,South],Wire[North,West],Wire[East,South],Wire[North,South],Wire[North,South],Wire[East,West],Wire[South,West],Wire[East,South],Wire[],Sink[South,West],Wire[North,South],Wire[North,South],Wire[South,West],Wire[North,East],Wire[North,West],Wire[East,South],Wire[North,South],Wire[South,West],Wire[],Wire[North,West],Wire[North,East],Wire[],Wire[East,South],Wire[North,East,South,West],Wire[North,West],Wire[North,South],Wire[],Wire[North,West],Wire[East,South]],[Wire[North,South],Wire[South,West],Wire[East,South],Wire[North,West],Wire[South,West],Wire[North,East],Wire[South,West],Wire[East,West],Wire[North,West],Wire[East,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,South],Wire[South,West],Wire[East,South],Wire[North,East,South,West],Sink[East,South],Wire[South,West],Wire[East,West],Wire[North,South],Wire[North,South],Wire[East,West],Wire[South,West],Wire[North,South],Wire[East,West],Wire[North,South],Wire[],Wire[North,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[North,East],Wire[North,East,South,West],Wire[East,South],Wire[North,West],Wire[North,West],Wire[North,South],Wire[East,West]],[Wire[North,West],Wire[North,East],Wire[East,South],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Sink[North,East],Wire[East,South],Wire[North,West],Wire[],Sink[East,South],Wire[North,East,South,West],Wire[North,West],Wire[],Wire[North,East],Wire[East,West],Source[North,East,South,West],Wire[East,West],Wire[North,South],Wire[North,East],Wire[],Wire[],Wire[South,West],Wire[North,East,South,West],Wire[North,East],Wire[South,West],Wire[East,South],Wire[],Wire[North,West],Wire[East,West],Wire[North,East,South,West],Wire[North,West],Wire[],Wire[North,West],Wire[East,South],Wire[],Wire[],Wire[East,West],Wire[North,South],Wire[North,South]],[Wire[East,South],Wire[North,South],Wire[South,West],Wire[North,West],Wire[South,West],Wire[East,West],Wire[North,East],Wire[North,West],Wire[North,South],Wire[North,West],Wire[South,West],Wire[South,West],Wire[North,South],Wire[North,West],Wire[North,South],Wire[],Wire[East,West],Wire[South,West],Sink[North,South],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[North,West],Wire[North,West],Wire[South,West],Wire[North,East],Wire[],Sink[East,South],Sink[North,East,South,West],Wire[East,South],Wire[South,West],Wire[North,East],Wire[South,West],Wire[North,West],Wire[North,West],Wire[South,West],Wire[North,West],Wire[North,East],Wire[North,South],Wire[North,South]],[Wire[South,West],Wire[North,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[North,West],Wire[East,West],Wire[East,South],Wire[North,South],Wire[East,West],Wire[North,West],Wire[East,West],Wire[East,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[North,South],Wire[South,West],Wire[East,South],Wire[South,West],Wire[South,West],Wire[South,West],Wire[East,South],Wire[North,East],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,West],Wire[North,South],Wire[East,South],Wire[North,East,South,West],Wire[East,West],Wire[South,West],Wire[East,South],Wire[South,West],Wire[East,South],Wire[East,South],Wire[North,South],Wire[],Wire[East,South],Wire[South,West]],[Wire[North,West],Wire[North,South],Wire[North,East],Wire[North,South],Wire[East,South],Wire[North,West],Wire[North,West],Wire[East,West],Wire[East,South],Wire[South,West],Wire[North,East,South,West],Wire[South,West],Wire[South,West],Wire[North,East,South,West],Wire[South,West],Wire[],Wire[East,West],Wire[East,South],Sink[North,East,South,West],Wire[North,West],Wire[],Wire[North,South],Wire[North,West],Wire[North,East],Wire[North,South],Wire[East,South],Wire[North,East,South,West],Wire[North,East,South,West],Wire[East,West],Wire[North,East],Wire[North,West],Wire[East,South],Wire[],Wire[],Wire[],Wire[South,West],Sink[South,West],Wire[],Wire[North,East],Wire[North,East]],[Wire[South,West],Wire[North,South],Wire[North,East],Wire[North,West],Wire[North,West],Wire[North,West],Wire[North,East,South,West],Wire[North,West],Wire[South,West],Wire[East,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[South,West],Wire[South,West],Sink[East,South],Wire[East,South],Wire[North,West],Wire[South,West],Sink[North,East,South,West],Wire[South,West],Wire[South,West],Wire[North,East,South,West],Wire[South,West],Wire[East,South],Wire[East,South],Wire[],Wire[North,South],Wire[North,West],Sink[North,West],Wire[North,West],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[South,West],Wire[East,South],Wire[East,West],Wire[],Wire[],Wire[East,West],Wire[North,South]],[Wire[],Wire[North,East],Wire[North,East],Wire[],Wire[North,West],Wire[North,East],Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[East,West],Wire[East,West],Wire[North,East],Wire[South,West],Wire[],Wire[East,West],Wire[],Wire[],Wire[North,East],Wire[East,West],Wire[North,West],Wire[North,West],Wire[North,East],Wire[North,South],Wire[East,South],Wire[South,West],Wire[North,South],Wire[],Wire[North,East],Wire[North,East],Wire[South,West],Wire[South,West],Wire[],Wire[North,South],Wire[East,West],Wire[East,South],Wire[North,East],Wire[],Wire[East,West],Wire[East,West]],[Wire[],Wire[North,West],Wire[South,West],Wire[East,South],Wire[North,East],Wire[East,South],Wire[East,West],Wire[North,East,South,West],Wire[North,South],Wire[North,East],Wire[East,West],Wire[North,South],Wire[North,South],Wire[North,West],Wire[North,South],Wire[North,West],Wire[North,West],Wire[East,West],Wire[North,East],Wire[East,South],Wire[South,West],Wire[],Sink[North,West],Wire[South,West],Wire[East,West],Wire[East,West],Wire[South,West],Wire[East,West],Wire[South,West],Wire[],Wire[],Wire[North,South],Wire[South,West],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[East,West],Wire[],Wire[South,West],Wire[North,South,West]],[Wire[East,South],Wire[South,West],Wire[East,West],Wire[South,West],Wire[North,East],Wire[East,South],Wire[North,South],Wire[East,South],Wire[],Wire[East,West],Wire[North,South],Wire[North,West],Wire[North,West],Wire[South,West],Wire[North,South],Wire[North,South],Wire[South,West],Wire[],Wire[North,West],Wire[North,West],Wire[East,West],Wire[North,West],Wire[North,East,South,West],Wire[East,West],Wire[South,West],Wire[North,West],Wire[North,South],Wire[North,South],Wire[North,West],Wire[East,South],Wire[South,West],Wire[North,South],Wire[East,West],Wire[East,West],Wire[North,West],Wire[South,West],Wire[North,South],Wire[],Wire[],Wire[North,South]],[Wire[North,East],Wire[North,East,South,West],Wire[North,East],Wire[],Wire[North,West],Wire[North,East,South,West],Wire[East,South],Wire[],Wire[South,West],Wire[North,East,West],Wire[East,South],Wire[East,West],Wire[South,West],Wire[North,West],Wire[South,West],Wire[South,West],Wire[East,West],Wire[East,West],Wire[North,East],Wire[],Wire[East,West],Wire[North,South],Wire[North,East],Wire[North,West],Wire[North,East],Wire[North,West],Wire[],Wire[East,South],Wire[East,West],Sink[North,East,South,West],Source[North,East],Wire[South,West],Wire[North,East,South,West],Wire[North,East,South,West],Wire[North,East],Wire[East,West],Wire[East,West],Wire[],Wire[North,East],Wire[East,South,West]],[Wire[],Wire[North,West],Wire[North,South],Wire[East,West],Wire[East,South],Wire[North,East],Wire[North,East,West],Wire[North,South],Sink[North,West],Wire[East,South],Wire[North,South],Wire[East,West],Wire[East,South],Wire[],Wire[North,East],Wire[North,West],Wire[],Wire[],Wire[North,East],Wire[East,West],Wire[North,East],Wire[North,East],Sink[North,South],Wire[North,East],Wire[North,East],Wire[North,East],Wire[],Wire[North,East],Wire[East,West],Wire[East,South],Wire[],Wire[],Wire[East,South],Wire[North,East],Wire[],Wire[North,West],Wire[North,East],Wire[],Wire[East,South],Wire[North,West]]])
                True
        ) 
    ]

testChallenge2 :: Test
testChallenge2 = TestList [

    ]

testChallenge3 :: Test
testChallenge3 = TestList [
        TestCase (
            assertEqual "Challenge 3: Test case #1: Example 1 from document"
                (prettyPrint (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))))
                "(\\x1 -> x1) \\x1 -> x1"
        ),
        TestCase (
            assertEqual "Challenge 3: Test case #2: Example 2 from document"
                (prettyPrint (Let Discard (Var 0) (Abs (V 1) (App (Var 1) (Abs (V 1) (Var 1))))))
                "let _ = x0 in \\x1 -> x1 \\x1 -> x1"
        ),
        TestCase (
            assertEqual "Challenge 3: Test case #3: Example 3 from document"
                (prettyPrint (Abs (V 1) (Abs Discard (Abs (V 2) (App (Var 2 ) (Var 1 ) ) ) )))
                "\\x1 _ x2 -> x2 x1"
        ),
        TestCase (
            assertEqual "Challenge 3: Test case #4: Example 4 from document"
                (prettyPrint (App (Var 2) (Abs (V 1) (Abs Discard (Var 1)))))
                "x2 \\x1 _ -> x1"
        )
    ]

testChallenge4 :: Test
testChallenge4 = TestList[
        TestCase (
            assertEqual "Challenge 4: Test case #1: Example 1 from document"
                (parseLetx "x1 (x2 x3)")
                (Just (App (Var 1) (App (Var 2) (Var 3))))
        ),
        TestCase (
            assertEqual "Challenge 4: Test case #2: Example 2 from document"
                (parseLetx "x1 x2 x3")
                (Just (App (App (Var 1) (Var 2)) (Var 3)))
        ),
        TestCase (
            assertEqual "Challenge 4: Test case #3: Example 3 from document"
                (parseLetx "let x1 x3 = x2 in x1 x2")
                (Just (Let (V 1) (Abs (V 3) (Var 2)) (App (Var 1) (Var 2))))
        ),
        TestCase (
            assertEqual "Challenge 4: Test case #4: Example 4 from document"
                (parseLetx "let x1 _ x3 = x3 in \\x3 -> x1 x3 x3")
                (Just (Let (V 1) (Abs Discard (Abs (V 3) (Var 3))) (Abs (V 3) (App (App (Var 1) (Var 3)) (Var 3)))))
        )
    ]

testChallenge5 :: Test
testChallenge5 = TestList[
        TestCase (
            assertEqual "Challenge 5: Test case #1: Example 1 from document"
                (letEnc (Let Discard (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))))
                (LamApp (LamAbs 0 (LamAbs 2 (LamVar 2))) (LamAbs 2 (LamVar 2)))
        ),
        TestCase (
            assertEqual "Challenge 5: Test case #2: Example 2 from document"
                (letEnc (Fst (Pair (Abs (V 1) (Var 1)) (Abs Discard (Var 2)))))
                (LamApp (LamAbs 0 (LamApp (LamApp (LamVar 0) (LamAbs 2 (LamVar 2))) (LamAbs 0 (LamVar 2)))) (LamAbs 0 (LamAbs 1 (LamVar 0))))
        )
    ]

testChallenge6 :: Test
testChallenge6 = TestList[
        TestCase (
            assertEqual "Challenge 6: Test case #1: Example 1 from document"
                (compareRedn (Let (V 3) (Pair (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2))) (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2)))) (Fst (Var 3))) 10)
                (6,8,4,6)
        ),
        TestCase (
            assertEqual "Challenge 6: Test case #2: Example 2 from document"
                (compareRedn (Let Discard (App (Abs (V 1) (Var 1)) (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))) (Snd (Pair (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))) (Abs (V 1) (Var 1))))) 10)
                (5,7,2,4)
        ),
        TestCase (
            assertEqual "Challenge 6: Test case #3: Example 3 from document"
                (compareRedn (Let (V 2) (Let (V 1) (Abs (V 0) (App (Var 0) (Var 0))) (App (Var 1) (Var 1))) (Snd (Pair (Var 2) (Abs (V 1) (Var 1))))) 100)
                (100,100,2,4)
        )
    ]

main :: IO ()
main = do
    _ <- runTestTT testChallenge1
    _ <- runTestTT testChallenge2
    _ <- runTestTT testChallenge3
    _ <- runTestTT testChallenge4
    _ <- runTestTT testChallenge5
    _ <- runTestTT testChallenge6
    return ()    



