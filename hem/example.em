from std import *
from IO import print

// A comment

/*
A multiline comment
*/

type ATuple = (Int, Int)

type AnArray = [Int]

type ARecord = {
    x: int,
    y: int
}

type AGeneric TypeArg = {
    example: [TypeArg]
}

type NestedGeneric TypeArg = {
    example: [AGeneric TypeArg]
}

type NestedGeneric2 TypeArg = NestedGeneric AGeneric TypeArg

aString = "test"

aMultilineString = "
    hello
    world
"

type AddFn = Int -> Int -> Int

export add: AddFn = x -> y -> x + y

export sub = x: Int -> y: Int -> x - y

export main = () ->
    print(add(1, sub(3, 1)))
