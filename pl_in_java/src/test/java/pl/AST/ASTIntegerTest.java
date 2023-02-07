package pl.AST;

import org.junit.Test;
import pl.Meaning.IntegerRepresentation;
import pl.TypePrediction.Type;

import static org.junit.Assert.*;

public class ASTIntegerTest extends ParentTest{

    @Test
    public void typeCheck() throws Exception {
        assertEquals(one.typeCheck(typeAcc), Type.INTEGER);
        assertEquals(negfive.typeCheck(typeAcc), Type.INTEGER);
    }

    @Test
    public void value() throws Exception {
        assertEquals(one.value(valAcc), new IntegerRepresentation(1));
        assertEquals(negfive.value(valAcc), new IntegerRepresentation(-5));
    }

    @Test
    public void staticDistance() {
        assertEquals(one.staticDistance(sdAcc, numLets-1), one);
        assertEquals(negfive.staticDistance(sdAcc, numLets-1), negfive);
    }

    @Test
    public void countNumLets() {
        assertEquals(one.countNumLets(0), 0);
        assertEquals(negfive.countNumLets(3), 3);
    }

    @Test
    public void valueSD() throws Exception {
        assertEquals(one.valueSD(sdValAcc, numLets), new IntegerRepresentation(1));
        assertEquals(negfive.valueSD(sdValAcc, numLets), new IntegerRepresentation(-5));
    }

}