package main

type Node interface{}

type Num struct{ Val int64 }
type Symbol struct{ Name string }
type List struct{ Items []Node }
