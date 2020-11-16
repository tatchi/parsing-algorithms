type expression =
  | NumericLiteral(int)
  | BinaryExpression(binaryExpression)
and binaryExpression = {
  op: string,
  left: expression,
  right: expression,
};

type t = expression;
