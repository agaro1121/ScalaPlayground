#Strictness and Laziness

- `&&` and `||` are lazy
- Adding the lazy keyword to a val declaration will cause Scala to delay evaluation of the right-hand side of that lazy val declaration until it’s first referenced.