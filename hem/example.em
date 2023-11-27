from std import *
from IO import print

// A comment

/*
A multiline comment
*/

type ATuple = (Int, Int)

type AnArray = Int[]

type ARecord = {
  x: int,
  y: int
}

type AGeneric<T> = {
  example: T[]
}

type NestedGeneric<T> = {
  example: AGeneric<T>[]
}

type NestedGeneric2 = NestedGeneric<AGeneric<T>>

aString = "test"

aMultilineString = "
hello
world
"

type AddFn = Int -> Int -> Int

export add: AddFn = x -> y -> x + y

export sub = x: Int -> y: Int -> x - y

export main = () ->
  print add 1 sub 3 1
