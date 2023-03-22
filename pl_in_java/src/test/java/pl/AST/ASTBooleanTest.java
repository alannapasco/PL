package pl.AST;

import org.junit.Test;

import static org.junit.Assert.*;

public class ASTBooleanTest extends ParentTest {

    @Test
    public void typeCheck() throws Exception {
        assertEquals(t.typeCheck(typeAcc), VarType.BOOLEAN);
        assertEquals(f.typeCheck(typeAcc), VarType.BOOLEAN);
    }

    @Test
    public void value() throws Exception {
        assertEquals(t.value(valAcc), boolrepT);
        assertEquals(f.value(valAcc), boolrepF);
    }

    @Test
    public void staticDistance() {
        assertEquals(t.staticDistance(sdAcc, numLets-1), t);
        assertEquals(f.staticDistance(sdAcc, numLets-1), f);
    }

    @Test
    public void countNumLets() {
        assertEquals(t.countNumLetsInAST(0), 0);
        assertEquals(f.countNumLetsInAST(3), 3);
    }

    @Test
    public void valueSD() throws Exception {
        assertEquals(t.valueSD(sdValAcc, numLets), boolrepT);
        assertEquals(f.valueSD(sdValAcc, numLets), boolrepF);
    }
}