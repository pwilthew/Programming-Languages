data Term = Num Int | Var String | Fun String [Term]

type Substitucion = [(Term,Term)]
