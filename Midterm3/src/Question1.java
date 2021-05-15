class TestDriver {
    // This is a setup method that you don't have to diagram
    Expression getTestExpression() {
        // do not diagram the internals of this method
        Expression exp = new MulOp(
                new MulOp(
                        new Literal(5),
                        new Literal(6)
                ),
                new Literal(3)
        );
        return exp;
    }
    // refactor this and provide the code you refactored it to.
    void testDriver() {
        Expression exp = getTestExpression();
        // these 2 lines can change when you refactor
        Integer result = new RecursiveEvaluator().evaluate(exp); // 1
        assert( result ==90 );                            // 2
    }
}

/**
 abstract class Expression {
 getValue(): Integer
 }

 class MulOp extends Expression {
 public left: Expression
 public right: Expression
 MulOp(Expression left, Expression right)

 getValue(): Integer
 }

 class Literal extends Expression {
 public value: Integer
 Literal(int value)
 getValue(): Integer
 }

 class ExpressionEvaluator {
 evaluate(e: Expression): Integer
 }*/